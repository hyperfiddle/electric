(ns hyperfiddle.photon-ui3
  "uses photon-dom2, so no syntax for text and props"
  #?(:cljs (:require-macros hyperfiddle.photon-ui3))
  (:refer-clojure :exclude [long double keyword symbol uuid])
  (:import (hyperfiddle.photon Pending))
  (:require
    [contrib.str :refer [blank->nil pprint-str]]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    [hyperfiddle.photon-dom2 :as dom2]
    [hyperfiddle.photon-ui2 :as ui2 :refer [Focused?
                                            parse-edn parse-keyword parse-symbol parse-date]]
    [hyperfiddle.rcf :as rcf :refer [tests tap with %]]))

(p/defn InputController [controlled-value syncing event setter getter]
  ;(when-not (= syncing (try controlled-value false (catch Pending _ true)))
  ;  (println "can controlled-value and syncing go out of sync? yes it can, first page load at least"))
  #_(println "InputController " controlled-value syncing)
  (assert (some? syncing) "common userland error")
  (case (Focused?.)
    true (when-some [e (dom2/Event. event syncing)]
           #_(println 'InputController (getter e))
           (getter e))
    false (case syncing
            true nil ;(throw (Pending.)) -- only the transact callback should throw pending, nil is safe
            false (try ; double d-glitch
                    (when (some? (p/Unglitch. controlled-value))
                      (setter controlled-value))
                    nil (catch Pending _ nil))
            (assert "InputController missing case, syncing: " syncing))))

(comment
  "desired future implementation - after d-glitch fix"
  (p/defn InputController [controlled-value event setter getter]
    (case (Focused?.)
      true (when-some [e (dom2/Event. event controlled-value #_(try controlled-value false (catch Pending _ true)))]
             (getter e))
      ;; D: some? is a hack that might break in e.g. Datomic context
      false (when (some? controlled-value) (setter controlled-value) nil))))

#?(:cljs (defn -set-input-value! [node v] (set! (.-value node) v)))
#?(:cljs (defn -get-input-value! [node e] (.-target.value ^js e)))

(defmacro input [v syncing & body]
  `(dom2/input
     (dom2/props {:type "text"})
     (let [ic# (new InputController ~v ~syncing "input"
                 (partial -set-input-value! dom/node)
                 (partial -get-input-value! dom/node))]
       ~@body                           ; discard body result, v' wins always
       ic#)))

(comment
  (dom/dt (dom/text "name"))
  (dom/dd (when (p/with-cycle [syncing false]
                  (println 'syncing syncing)
                  (if-some [v (ui3/input (:label/name record) syncing
                                         (dom/props {:style {:background-color (if syncing "orange")}}))]
                    (try (p/server (hf/Transact!. [[:db/add e :label/name v]]))
                         false (catch Pending _ true))
                    syncing)) ; with-cycle is not the right structure for this, need a swap!
            (throw (Pending.))))
  )

(p/defn InputController! "callback version, no looped pending" [v V! event setter getter]
  (let [x (p/with-cycle [?v' nil]
            #_(println "?v' " ?v')
            (let [syncing (= ?v' ::p/pending)]
              (when-some [v' (new InputController v syncing event setter getter)]
                #_(println "input! v': " v')
                (try (dom2/props {:style {:background-color "yellow"}})
                     (new V! v') ; fmap V!, typically ought to return nil but we permit escape
                     (catch Pending _ ::p/pending)))))]
    (if (not= x ::p/pending)
      x (throw (Pending.)))))

(defmacro input! [v V! & body] ; todo nominal args
  `(dom2/input (dom2/props {:type "text"})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (partial -get-input-value! dom/node))]
       ~@body ic#)))

(comment
  "callback usage"
  (dom/dt (dom/text "name"))
  (dom/dd (ui3/input! (:label/name record)
                      (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/name v]])))))

  (dom/dt (dom/text "id"))
  (dom/dd (ui3/input! (:db/id record) nil (dom/props {::dom/disabled true})))

  ; the callback version can't support circuit usage because that needs to inject the syncing
  ; state from above as the circuit callback isn't managed. use ui/input as above
)

(defmacro textarea [v syncing & body]
  `(dom2/textarea
     (let [ic# (new InputController ~v ~syncing "input"
                 (partial -set-input-value! dom/node)
                 (partial -get-input-value! dom/node))]
       ~@body ic#)))

(defmacro textarea! [v V! & body]
  `(dom2/textarea
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (partial -get-input-value! dom/node))]
       ~@body ic#)))

(defmacro edn-editor [v syncing & body]
  `(ui2/read-str-maybe (textarea (pprint-str ~v) ~syncing ~@body)))

(defmacro edn-editor! [v V! & body]
  `(let [V!# ~V!]
     (textarea! (pprint-str ~v) (p/fn [v#] (new V!# (ui2/read-str-maybe v#))) ~@body)))

#?(:cljs (defn -set-input-checked! [node v] (set! (.-checked node) v)))
#?(:cljs (defn -get-input-checked! [node e] (.-target.checked ^js e)))

(defmacro checkbox [v waiting & body]
  `(dom2/input
     (dom2/props {:type "checkbox"})
     (let [ic# (new InputController ~v ~waiting "change"
                 (partial -set-input-checked! dom/node)
                 (partial -get-input-checked! dom/node))]
       ~@body ic#)))

(defmacro checkbox! [v V! & body]
  `(dom2/input
     (dom2/props {:type "checkbox"})
     (let [ic# (new InputController! ~v ~V! "change"
                 (partial -set-input-checked! dom/node)
                 (partial -get-input-checked! dom/node))]
       ~@body ic#)))

(defmacro long [v waiting & body]
  ;; note: in firefox, clicking the arrows doesn't trigger focus event so the events are ignored. ui4 fixes this.
  `(dom2/input
     (dom/props {:type "number"})
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-long (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro long! [v V! & body]
  ;; note: in firefox, clicking the arrows doesn't trigger focus event so the events are ignored. ui4 fixes this.
  `(dom2/input
     (dom/props {:type "number"})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-long (partial -get-input-value! dom/node)))]
       ~@body ic#)))

;(defmacro range)

(defmacro range! [v V! & body]
  `(dom2/input
     (dom/props {:type "range"})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-long (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro double [v waiting & body]
  `(dom2/input (dom/props {:type "number"})
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-double (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro double! [v V! & body]
  `(dom2/input (dom/props {:type "number"})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-double (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(def uuid-pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(defmacro uuid [v waiting & body]
  `(dom2/input (dom/props {:type "text" :pattern uuid-pattern})
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-uuid (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro uuid! [v V! & body]
  `(dom2/input (dom/props {:type "text" :pattern uuid-pattern})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-uuid (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro keyword [v waiting & body]
  `(dom2/input
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-keyword (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro keyword! [v V! & body]
  `(dom2/input
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-keyword (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro symbol [v waiting & body]
  `(dom2/input
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-symbol (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro symbol! [v V! & body]
  `(dom2/input
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-symbol (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro date [v waiting & body]
  `(dom2/input
     (dom/props {:type "date"})
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-date (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro date! [v V! & body]
  `(dom2/input
     (dom/props {:type "date"})
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-date (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro edn [v waiting & body]
  `(dom2/textarea
     (let [ic# (new InputController ~v ~waiting "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-edn (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro edn! [v V! & body]
  `(dom2/textarea
     (let [ic# (new InputController! ~v ~V! "input"
                 (partial -set-input-value! dom/node)
                 (comp parse-edn (partial -get-input-value! dom/node)))]
       ~@body ic#)))

(defmacro button [waiting & body]
  `(dom2/button
     (let [ic# (new InputController nil ~waiting
                 "click" nil (fn [e#] (doto e# .stopPropagation)))]
       ~@body ic#)))

(comment (when (ui/button false (dom/text "hi")) (println 'cliccckk!)))

(defmacro button! [V! & body]
  `(dom2/button
     (let [ic# (new InputController!
                 nil (p/fn [e#] (dom/props {:disabled true, :aria-busy true}) (new ~V!)) ; fix arity
                 "click" nil (fn [e#] (doto e# .stopPropagation)))]
       ~@body ic#)))

(comment (ui/button! (p/fn [] (p/server (println 'yo))) (dom/text "hi")))
