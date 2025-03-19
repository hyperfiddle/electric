(ns hyperfiddle.electric3-contrib
  "Experimental operators under consideration for inclusion in Electric core"
  (:require [contrib.missionary-contrib :as mx]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m]))

(defmacro If [amb-test left right] `(if (e/Some? ~amb-test) ~left ~right))

#_(defmacro verse-if [test then else] `(case (e/as-vec ~test) [] ~else ~then))
#_(defmacro verse-if [test then else] `(e/$ (e/one (e/amb ({} test (e/fn [] ~then)) (e/fn [] ~else)))))

; L: I don't think we need e/one but we could have e/take and e/drop, then the one operator is just (e/take 1 ,,,)

(e/defn None? [xs] (zero? (e/Count xs)))
(e/defn Nothing [& args] (e/amb))
(e/defn Outputs [xs] (e/join (i/items (e/pure xs))))
#_(e/defn Seq [xs] (If xs xs nil))
#_(e/defn Some "return first non-nothing in order"
  [xs] (first (e/as-vec xs))) ; todo optimize

(e/defn Sleep
  ([ms x] (e/Task (m/sleep ms x)))
  ([ms] (Sleep ms nil)))

(e/defn Throttle [ms x] (e/input (mx/throttle ms (Outputs x))))

(e/defn Latch-stale [x]
  (let [!cache (atom [])]
    (when (e/Some? x) (reset! !cache (e/as-vec x)))
    (e/diff-by {} (e/watch !cache))))

(e/defn ^:deprecated Offload-reset "Deprecated. Promoted to `e/Offload-reset`." [f] (e/Offload-reset f))
(e/defn ^:deprecated Offload-latch "Deprecated. Promoted to `e/Offload-latch`." [f] (e/Offload-latch f))

#?(:clj (defonce *tap nil)) ; for repl & survive refresh
#?(:clj (def >tap (m/signal
                    (m/relieve {}
                      (m/observe
                        (fn [!]
                          (! *tap)
                          (let [! (fn [x] (def *tap x) (! x))]
                            (clojure.core/add-tap !)
                            #(clojure.core/remove-tap !))))))))

(e/defn Tap [] (e/server (e/input >tap)))

(e/defn UnglitchC [x else]
  (e/client
    (let [[x clock] (e/with-cycle [[p c] [::init 0]]
                      [x (if (= p x) c (inc c))])]
      (e/Reconcile (if (= clock (e/server (identity clock))) x else)))))

(e/defn UnglitchS [x else]
  (e/server
    (let [[x clock] (e/with-cycle [[p c] [::init 0]]
                          [x (if (= p x) c (inc c))])]
      (e/Reconcile (if (= clock (e/client (identity clock))) x else)))))
