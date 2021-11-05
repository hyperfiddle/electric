(ns hyperfiddle.zero
  (:refer-clojure :exclude [empty?])
  (:require [missionary.core :as m]
            [hfdl.lang :as p]
            [hfdl.impl.runtime :refer [pending]])
  #?(:cljs (:require-macros [hyperfiddle.zero :refer [pick current instant]])))

(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    (fn
      ([v] (reset! !state v))
      ([n t] (>state n t)))))

(def first-or "A task completing with the value of the first successful transfer of given flow, or a provided value if
it completes without producing any value." (partial m/reduce (comp reduced {})))

(def empty? "A task completing with true on first successful transfer of given flow, or false if it completes without
producing any value." (partial m/reduce (constantly (reduced false)) true))

(defmacro pick "head for flows. return first or nothing. Note that in Clojure you can't
return nothing (you return nil) but in flows nothing is different than nil." [t]
  `(let [x# (m/? t)]
     (case x# ::empty (m/amb) x#)))

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
    " [i x y]
  (m/ap
    (loop []
      (m/amb i
        (if-some [e (m/? y)]
          (m/amb e (if (m/? x) (m/amb) (recur)))
          (m/amb))))))

(defmacro current [form]
  `(unquote (m/eduction (take 1) (var ~form))))

(defmacro instant [value event]
  `(unquote (fsm nil (empty? (m/eduction (drop 1) (var ~value))) (first-or nil ~event))))

(def exports (p/vars state))