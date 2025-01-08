(ns hyperfiddle.electric3-contrib
  (:require [hyperfiddle.electric3 :as e]
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

(defn task-status "
Task -> continuous flow. State is [] before task completion, [result] after.
" [t] (m/reductions conj (m/ap (m/? t))))

(defn offload "
Continuous flow of thunks -> incseq of 1 item containing result of the latest thunk executed via m/blk, or the empty incseq if it's still pending.
" [<f] (i/diff-by {} (m/cp (m/?< (task-status (m/via-call m/blk (m/?< <f)))))))

(e/defn Offload [f]
  (e/join (offload (e/join (i/items (e/pure f))))))

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
