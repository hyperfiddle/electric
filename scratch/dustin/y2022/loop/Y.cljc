(ns dustin.y2022.Y
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.Y)))

(hyperfiddle.rcf/enable!)

; https://blog.klipse.tech/lambda/2016/08/07/pure-y-combinator-clojure.html

(def Y
  (fn [g]
    ((fn [f] (f f))
     (fn [f]
       (g (fn [y]
            ((f f) y)))))))

(def fac-gen
  (fn [recur']
    (fn [n]
      (if (zero? n)
        1
        (* n (recur' (dec n)))))))

(tests
  ((Y fac-gen) 19)
  := 121645100408832000)