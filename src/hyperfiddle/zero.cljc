(ns hyperfiddle.zero
  "Experimental Photon core library, badly named, should likely be re-exported from photon namespace."
  (:refer-clojure :exclude [empty?])
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hyperfiddle.zero :refer [pick current impulse]])))

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

(defmacro impulse
  "Translates a discrete event stream `>xs` into an equivalent continuous signal impulse. The impulse signal will stay
   'up' until it is sampled and acknowledged by `ack`. (Thus the duration of the impulse depends on sampling rate.) Upon
   ack, the impulse restarts from nil. Useful for modeling events in Photon's continuous time model."
  [ack >xs]
  `(new (fsm nil
             (empty? (m/eduction (drop 1) (p/fn [] ~ack)))
             (first-or nil ~>xs))))

(defmacro current [form]
  `(new (m/eduction (take 1) (p/fn [] ~form))))

(comment
  "scratch related to continuous time events with slow consumers"
  (defn differences [rf]
    (let [state (volatile! 0)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [d (- x @state)]
           (assert (not (neg? d)))
           (vreset! state x)
           (rf r d))))))

  (tests
    "Five effects driven by CT counter where sampling is slow so discrete events were missed"
    (sequence differences [0 2 3 5]) := [0 2 1 2])

  (defn foreach-tick [<x f]
    (->> (m/ap
           (dotimes [x' (m/?> (m/eduction differences <x))]
             (f)))
         (m/reductions {} nil)                              ; run discrete flow for effect
         (m/relieve {})))

  (defmacro do-step [x & body]
    `(foreach-tick (p/fn [] ~x)
                   ~@body
                   #_(fn [] ~@body)))

  (def drive (comp differences (mapcat (fn [x] (repeat x nil))))) ; transducer
  (tests (sequence drive [0 2 3 5]) := [nil nil nil nil nil])

  (defn foreach-tick [<x f]
    (->> (m/ap
           (let [_ (m/?> (m/eduction z/drive <x))]
             (f)))
         (m/reductions {} nil)                              ; produce nil in discrete time for effect
         (m/relieve {}))))
