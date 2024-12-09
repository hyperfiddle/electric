(ns hyperfiddle.electric3-contrib
  (:require [hyperfiddle.electric3 :as e]))

(defmacro If [amb-test left right] `(if (e/Some? ~amb-test) ~left ~right))

#_(defmacro verse-if [test then else] `(case (e/as-vec ~test) [] ~else ~then))
#_(defmacro verse-if [test then else] `(e/$ (e/one (e/amb ({} test (e/fn [] ~then)) (e/fn [] ~else)))))

; L: I don't think we need e/one but we could have e/take and e/drop, then the one operator is just (e/take 1 ,,,)

(e/defn None? [xs] (zero? (e/Count xs)))
(e/defn Nothing [& args] (e/amb))
