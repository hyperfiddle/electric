(ns user.popover-ui1
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.ui.codemirror :refer [read-edn write-edn]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.popover-ui1)))

(defmacro with-cycle [[s i] & body]
  `(let [a# (atom ~i) ~s (p/watch a#)]
     (reset! a# (do ~@body))))

(defonce !conn #?(:cljs nil ; state survives reload
                  :clj (doto (d/create-conn {:order/email {}})
                         (d/transact! [{:order/email "alice@example.com" :order/gender :order/female}
                                       {:order/email "bob@example.com" :order/gender :order/male}
                                       {:order/email "charlie@example.com" :order/gender :order/male}]))))

(p/def db)

#?(:clj
   (defn teeshirt-orders [db ?email]
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(clojure.string/includes? ?email ?needle)]]
            db (or ?email "")))))

(defn abc [] (->> (range 33 126) (map (comp str char))))
(defn password [] (apply str (repeatedly 10 #(rand-nth (abc)))))

;; {:status :idle}
;; {:status :create :record {:order/email ""}}
;; {:status :pending :record {:order/email ""}}
(p/defn Teeshirt-orders-view [busy]
  (:record
    (let [!state (atom {:status :idle}) state (p/watch !state)]
      (dom/div {:class "hyperfiddle-hfql"}
        (ui/button {::dom/aria-busy  busy
                    ::ui/click-event (p/fn [_] (reset! !state {:status :create :record [{:order/email (str (password) "@example.com")}]}))}
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
                      (dom/td (p/server (:order/gender (d/entity db id))))))))))))
      (reset! !state
        (case (:status state)
          :idle state
          :create (assoc state :status :pending)
          :pending (if busy state {:status :idle}))))))

(p/defn StagingArea [stage !stage]
  (p/client
    (ui/textarea {::dom/class "stage"
                  ::ui/value (some-> stage vec write-edn)
                  ::ui/input-event (p/fn [e]
                                     (let [tx (read-edn (.. e -target -value))]
                                       (p/server (reset! !stage tx))))})))

; data PopoverState = Closed Tx | Open
(p/defn Popover [Body X busy] ; only client
  (:request
    (let [!state (atom {:status :closed})
          {:keys [status]} (p/watch !state)]
      ; popover anchor
      (ui/button {::ui/click-event (p/fn [_] (swap! !state update :status {:closed :open :open :closed}))}
        (case status :closed "open" :open "close" "") " popover")
      ; popover body
      (let [request (case status
                      :closed nil
                      (dom/div {:style {:position "fixed"}}
                        (dom/div {:style {:border           "1px pink solid" :padding "5px"
                                          :position         "relative" :left "3em" :top "2em" :z-index "1"
                                          :width            "50em" :height "40em"
                                          :background-color "rgb(248 250 252)"}}
                          (Body. X busy))))]
        (reset! !state
          {:status  (case status
                      :closed :closed
                      :open (case request nil :open :pending)
                      :pending (if busy :pending :closed))
           :request request})))))                        ; client bias, careful

(p/defn StagedBody [Body busy]
  (p/server
    (let [!stage (atom []), stage (p/watch !stage)] ; fork
      (binding [db (:db-after (d/with db stage))]
        (p/client
          (with-cycle [busy false]
            (if-some [tx (Body. busy)]
              (try (p/server (swap! !stage concat tx) false)
                   (catch Pending _ true)) false))
          (dom/hr)
          (let [!state (atom {:status :idle})
                state (p/watch !state)]
            (ui/button {::ui/click-event (p/fn [_] (reset! !state {:status :request :command stage}))} "commit!")
            (ui/button {::ui/click-event (p/fn [_] (reset! !state {:status :request :command []}))} "discard")
            (p/server (StagingArea. stage !stage))
            (:command
              (reset! !state
                (case (:status state)
                  :idle state
                  :request (assoc state :status :pending)
                  :pending (if busy state {:status :idle}))))))))))

;; busy is a boolean flag passed as argument to the UI component
;; the component returns a non-nil value when it needs a server action
;; the busy flag is immediately set to true in response to that state change
;; when the server action is done, the busy flag is set to false
;; in response to the flag change to false, the UI component result changes to nil.
(p/defn App []
  (p/client
    (dom/h2 "Time travel demo - tee shirt orders and popovers")
    (dom/element "style" "textarea.stage {display: block; width: 100%; height: 20em;}")
    (p/server
      (let [!stage (atom []), stage (p/watch !stage)] ; root
        (binding [db (:db-after (d/with (p/watch !conn) stage))]
          (p/client
            (with-cycle [busy false]
              (if-some [tx (seq (concat
                                  (Popover. StagedBody Teeshirt-orders-view busy)
                                  (Teeshirt-orders-view. busy)))]
                (try (p/server (swap! !stage concat tx) false)
                     (catch Pending _ true)) false))
            (dom/p "Root stage")
            (ui/button {::ui/click-event (p/fn [e]
                                           (p/server
                                             (let [_ (d/transact! !conn stage)]
                                               (reset! !stage []))))} "transact!")
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
