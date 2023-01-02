(ns dustin.y2023.pending-frame-nil-bug4
  #?(:cljs (:require-macros dustin.y2023.pending-frame-nil-bug4))
  (:import [hyperfiddle.photon Pending])
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    [hyperfiddle.photon-dom2 :as dom2]
    [hyperfiddle.photon-ui2 :as ui2]))

(p/defn InputController [controlled-value syncing event setter getter]
  (case (ui2/Focused?.)
    true (when-some [e (dom2/Event. event syncing)]
           #_(println 'InputController (getter e))
           (getter e))
    false (case syncing
            true nil ;(throw (Pending.)) -- only the transact callback should throw pending, nil is safe
            false (try ; double d-glitch
                    (when (p/Unglitch. controlled-value)
                      (setter controlled-value))
                    nil (catch Pending _ nil))
            (assert "InputController missing case, syncing: " syncing))))

(p/defn InputController! "callback version, no looped pending" [v V! event setter getter]
  (let [x (p/with-cycle [?v' nil]
            #_(println "?v' " ?v')
            (let [syncing (= ?v' ::p/pending)]
              (dom2/props {:style {:background-color (if syncing "yellow")}})
              (when-some [v' (new InputController v syncing event setter getter)]
                #_(println "input! v': " v')
                (try (new V! v') ; fmap V!, typically ought to return nil but we permit escape
                     (catch Pending _ ::p/pending)))))]
    (if (not= x ::p/pending)
      x (throw (Pending.)))))

#?(:cljs (defn -set-input-value! [node v] (set! (.-value node) v)))
#?(:cljs (defn -get-input-value! [node e] (.-target.value ^js e)))

(defmacro long! [v V! & body]
  `(dom2/input
     (dom2/props {:type "number"})
     ~@body
     ; todo - in firefox, clicking the arrows doesn't trigger focus event so the events are ignored
     (new InputController! ~v ~V! "input"
          (partial -set-input-value! dom/node)
          (comp parse-long (partial -get-input-value! dom/node)))))

(def !db (atom 1))

(p/defn Demo [] ; both 1. and 2. are needed to reproduce
  (p/server ; 1. wrap in server
    (p/client
      (long! 11 (p/fn [v'] (println 'a v')))
      (long! 11 (p/fn [v'] (println 'a v') (p/server (new (p/fn [] v')))))))) ; 2. wrap v' in frame
