(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(tests
  (def <x (m/cp
            (let [!x (atom 0)
                  x (m/?< (m/watch !x))]
              (if (< x 10)
                (reset! !x (inc x)) x))))
  (def it (<x #() #()))
  @it := 10
  (it))
; ❌ FAIL in () (scratch.cljc:10)
;expected: (= (clojure.core/deref it) 10)
;  actual: (not (= 1 10))