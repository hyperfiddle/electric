(ns hyperfiddle.zero
  (:refer-clojure :exclude [empty?])
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p])
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
  "A continuous time impulse as a discreet flow. This is a state machine. It first
  emit `init`, then the first value of the `>values` discreet flow, called the
  impulse. The impulse is expected to be acknowledge soon by a new value in
  `>control`, at which point it restart emitting `init`.

   Start ———> 1. emit `init`
          |   2. listen to `>values`, wait for a value
          |
          |   3. emit first value of `>values`           |
          |    . stop listening to `>values`             | Toggles
          |    . listen to `>control`, wait for a value  |
          |
           —— 4. stop listening to `>control`
               . discard value
               . GOTO 1.

   Time ——————— 0 ———— 1 ———— 2 ————3——————————>
                |
               -|       ————————————
   >values      |      |            |
               -|——————              ——————————
               -|               —————————
   >control     |              |         |
               -|——————————————           —————
             v -|       ———————      ————
   result       |      |       |    |    |
          init -|——————         ————      —————
                |
  "
  [init >control >values]
  (m/ap
    (loop []
      (m/amb init
        (if-some [e (m/? >values)]
          (m/amb e (if (m/? >control) (m/amb) (recur)))
          (m/amb))))))

(defmacro instant
  "A state machine emitting `nil`, then one value of a discreet flow `>values`.
  Then waits for `control` to change and restarts from `nil`.
  To be used from a photon expression."
  [control >values]
  `(unquote (fsm nil (empty? (m/eduction (drop 1) (var ~control))) (first-or nil ~>values))))

(defmacro current [form]
  `(unquote (m/eduction (take 1) (var ~form))))

(def exports (p/vars state))