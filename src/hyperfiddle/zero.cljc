(ns hyperfiddle.zero
  (:require [missionary.core :as m]
            [hfdl.lang :as p]
            [hfdl.impl.runtime :refer [pending]])
  #?(:cljs (:require-macros [hyperfiddle.zero :refer [pick current target]])))

(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    (fn
      ([v] (reset! !state v))
      ([n t] (>state n t)))))

(def first-or (partial m/reduce (comp reduced {})))

(defmacro pick "head for flows. return first or nothing. Note that in Clojure you can't
return nothing (you return nil) but in flows nothing is different than nil." [>f]
  `(let [x# (m/? (first-or ::empty ~>f))]
     (case x# ::empty (m/amb>) x#)))

(defn fsm
  "Produce a continuous time impulse with value v which will soon be acknowledged at which point the
  impulse is cleared. the ack is a flow of anything, we ignore the value, detecting only that an
  event happened

          v -|       -------
   >y        |      |       |
          i -|------         -----------


         any -|              -----------
   >x         |             |
         any -|-------------
    " [i >x >y]
  (m/ap
    (loop []
      (m/amb> i
        (m/amb> (pick >y)
          (do (pick >x) (recur)))))))

(defmacro current [form]
  `(unquote (m/eduction (take 1) (var ~form))))

(defmacro target [value event]
  `(unquote (fsm nil (m/eduction (drop 1) (var ~value)) ~event)))

(def exports (p/vars state))