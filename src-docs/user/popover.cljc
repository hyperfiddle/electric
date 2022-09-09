(ns user.popover
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.ui.codemirror :refer [read-edn write-edn]]
            [user.util :refer [pprint-str]])
  #?(:cljs (:require-macros user.popover)))

(defonce !conn #?(:cljs nil ; state survives reload
                  :clj (doto (d/create-conn {:order/email {}})
                         (d/transact! [{:order/email "alice@example.com" :order/gender :order/female}
                                       {:order/email "bob@example.com" :order/gender :order/male}
                                       {:order/email "charlie@example.com" :order/gender :order/male}]))))

(p/def db)
(p/def stage!)

#?(:clj
   (defn teeshirt-orders [db ?email]
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(user.util/includes-str? ?email ?needle)]]
            db (or ?email "")))))

(defn abc [] (->> (range 33 126) (map (comp str char))))
(defn password [] (apply str (repeatedly 10 #(rand-nth (abc)))))

(p/defn Teeshirt-orders-view []
  (dom/div {:class "hyperfiddle-hfql"}
    (ui/button {::ui/click-event (p/fn [_]
                                   (p/server
                                     (stage! [{:order/email (str (password) "@example.com")}])))}
      "new record")
    (let [!filter (atom ""), filter (p/watch !filter)]
      (ui/input {::dom/type        :search
                 ::dom/placeholder "Filter…"
                 ::ui/input-event  (p/fn [e] (reset! !filter (.. e -target -value)))})
      (dom/div {:style {:height "30em"}}
        (dom/table
          (p/server
            (p/for [id (teeshirt-orders db filter)]
              (p/client
                (dom/tr
                  (dom/td id)
                  (dom/td (p/server (:order/email (d/entity db id))))
                  (dom/td (p/server (:order/gender (d/entity db id)))))))))))))

(p/defn StagingArea [stage !stage]
  (p/client
    (ui/textarea {::dom/class "stage"
                  ::ui/value (some-> stage vec write-edn)
                  ::ui/input-event (p/fn [e]
                                     (let [tx (read-edn (.. e -target -value))]
                                       (p/server (reset! !stage tx))))})))

(p/defn Popover [Body]
  (let [!open (atom false)
        open? (p/watch !open)
        !ret  (atom nil)]

    ; popover anchor
    (ui/button {::ui/click-event (p/fn [_] (reset! !ret nil) (swap! !open not))}
      (if open? "close" "open") " popover")

    ; popover body
    (if-not open?
      (p/watch !ret) ; return tx on close
      (dom/div {:style {:position "fixed"}}
        (dom/div {:style {:border "1px pink solid" :padding "5px"
                          :position "relative" :left "3em" :top "2em" :z-index "1"
                          :width "50em" :height "40em"
                          :background-color "rgb(248 250 252)"}}
          (p/server
            (let [!stage (atom [])
                  stage (p/watch !stage)] ; fork
              (binding [db (:db-after (d/with db stage))
                        stage! (partial swap! !stage concat)]
                (p/client
                  (Body.)
                  (dom/hr)
                  (ui/button {::ui/click-event (p/fn [e] (reset! !ret stage) (reset! !open false))} "commit!")
                  (ui/button {::ui/click-event (p/fn [e] (reset! !open false))} "cancel")
                  (p/server (StagingArea. stage !stage)))))))))))

(p/defn App []
  (dom/h2 "Time travel demo - tee shirt orders and popovers")
  (dom/element "style" "textarea.stage {display: block; width: 100%; height: 20em;}")
  (p/server
    (let [!stage (atom [])               ; root
          stage  (p/watch !stage)]
      (binding [db (:db-after (d/with (p/watch !conn) stage))
                stage! (partial swap! !stage concat)]
        (p/client
          (dom/div
            (when-let [tx (Popover. Teeshirt-orders-view)]
              (p/server (swap! !stage concat tx)))
            (Teeshirt-orders-view.)
            (dom/p "Root stage")
            (p/server (StagingArea. stage !stage))))))))

(comment
  (d/transact! !conn [[:db/retractEntity id]])
  (swap! !stage concat [[:db/retractEntity id]])
  (stage! [[:db/retractEntity id]])
  `(stage! ~[[:db/retractEntity id]])
  `(stage! ~[:db/retractEntity id])
  (retractEntity! id)
  [[:db/retractEntity id]]
  `(d/transact! !conn ~[[:db/retractEntity id]])
  [::retractEntity id])

; next steps - a hyper-button with pending, cancel, retry on a server command
