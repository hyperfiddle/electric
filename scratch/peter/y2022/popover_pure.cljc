(ns peter.y2022.popover-pure
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror :refer [read-edn write-edn]]))

;; (dom/div) returns a pure value (doesn't mount to dom)
;; (dom/div!) is just (dom/mount! (dom/div))
;; When we return a pure value we can do fun stuff with it!
;;
;;   (let [btn (dom/button)] (new (:click btn)))
;;
;; This returns the DOM click events on the button.
;; `dom/on` makes this a bit more convenient:
;;
;;  (dom/on btn :click "hi")
;;
;; This desugars to
;;
;;  (binding [dom/event (new (:click btn))] "hi")
;;
;; This decoupling allows for more pure code.
;;
;; A few more wishful thinking ideas used here:
;; - `p/state` with `p/reset!` and `p/swap!`. Not sure if doable, can unroll to `atom` and `p/watch`
;; - `p/all`, which is like `m/amb=`
;;
;; Q:
;; - Where is Pending?
;;   idk. Maybe inside `dom/on`, or in a separate `dom/pending`?

(defonce !conn #?(:cljs nil             ; state survives reload
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

(p/defn Teeshirt-orders-view []
  (dom/div! {:class "hyperfiddle-hfql"}
    (let [btn (dom/button "new record")
          input (dom/input {:type :search :placeholder "Filter..."})]
      (dom/mount! btn)
      (dom/mount! input)
      (dom/div! {:style {:height "30em"}}
        (dom/table!
          (p/server
            (p/for [id (teeshirt-orders db (p/client (dom/on input :input dom/event)))]
              (p/client
                (dom/tr!
                  (dom/td! id)
                  (dom/td! (p/server (:order/email (d/entity db id))))
                  (dom/td! (p/server (:order/gender (d/entity db id))))))))))
      (dom/on btn :click {:order/email (str (password) "@example.com")}))))

(p/defn StagingArea [stage]
  (p/client
    (let [textarea (dom/textarea {:class "stage"})]
      (dom/set textarea :value (some-> stage vec write-edn))
      (dom/on :input (p/server (p/reset! stage (p/client (-> dom/event :target :value read-edn))))))))

(p/defn Popover [Body]
  (let [stage (p/state []), root (dom/div {:style {:position "fixed"}})]
    (p/mount! root
      (dom/div! {:style {:border "1px pink solid" :padding "5px"
                         :position "relative" :left "3em" :top "2em" :z-index "1"
                         :width "50em" :height "40em"
                         :background-color "rgb(248 250 252)"}}
        (p/server
          (binding [db (:db-after (d/with db stage))]
            (p/client
              (p/swap! stage conj (Body.))
              (dom/hr!)
              (let [ret (p/all          ; like m/amb=
                          (let [btn (dom/button "commit!")]
                            (dom/mount! btn)
                            ;; return `stage` after unmount
                            ;; maybe it should be `(dom/unmount! root) stage`
                            (dom/on btn :click (dom/unmount! root stage)))
                          (let [btn (dom/button "cancel")]
                            (dom/mount! btn)
                            (dom/on btn :click (dom/unmount! root []))))]
                (p/server (StagingArea. stage))
                ret))))))))

(p/defn App []
  (p/client
    (dom/h2! "Time travel demo - tee shirt orders and popovers")
    (dom/element! "style" "textarea.stage {display: block; width: 100%; height: 20em;}")
    (p/server
      (let [stage (p/state [])]
        (binding [db (:db-after (d/with (p/watch !conn) stage))]
          (p/client
            (dom/div!
              (p/server (p/swap! stage into (p/client (Popover. Teeshirt-orders-view))))
              (p/swap! stage conj (Teeshirt-orders-view.))
              (dom/p! "Root stage")
              (p/server (StagingArea. stage)))))))))
