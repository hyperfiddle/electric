(ns hyperfiddle.zero
  "Photon standard library (experimental, idioms are not established yet).
  Badly named, should be moved into photon namespace."
  (:refer-clojure :exclude [empty? time])
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]])
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
  "Translates a discrete event stream `>xs` into an equivalent continuous signal of impulses. Each impulse will stay
   'up' until it is sampled and acknowledged by signal `ack`. (Thus the duration of the impulse depends on sampling
   rate.) Upon ack, the impulse restarts from nil.

   Useful for modeling discrete events in Photon's continuous time model."
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

#?(:cljs
   (deftype Clock [^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn                                                    ; cancel
     (-invoke [_]
       (if (zero? raf)
         (set! callback nil)
         (do (.cancelAnimationFrame js/window raf)
             (terminator))))
     IDeref                                                 ; sample
     (-deref [_]
       (if (nil? callback)
         (terminator)
         (set! raf (.requestAnimationFrame js/window callback)))
       ::tick)))

(def <clock "efficient logical clock.
  In CLJS, 60hz throttled by requestAnimationFrame (browser only).
  in CLJ, ~100hz throttled by m/sleep 10ms."
  #?(:cljs (fn [n t]
             (let [cancel (->Clock 0 nil t)]
               (set! (.-callback cancel)
                     (fn [_] (set! (.-raf cancel) 0) (n)))
               (n) cancel))

     ; Warning, m/sleep has a race condition currently
     :clj  (->> (m/ap (loop [] (m/amb (m/? (m/sleep 10 ::tick)) (recur))))
                (m/reductions {} ::tick)
                (m/relieve {}))))

(p/def clock #_"efficient logical clock" (new (identity <clock))) ; singleton

(tests
  "logical clock"
  (with (p/run (! clock)) [% % %] := [::tick ::tick ::tick]))

; Do we need a clock that ticks as fast as it is sampled? (setTimeout 0). Useful with z/impulse?

; This ticker will skew and therefore is not useful other than as an example
(p/def ticker (new (->> <clock
                        (m/eduction (map (constantly 1)))
                        (m/reductions + 0)
                        (m/relieve {}))))

(tests (with (p/run (! ticker)) [% % %] := [1 2 3]))

(defn system-time-ms [_] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

(p/def time #_"ms since Jan 1 1970" (new (m/latest system-time-ms <clock)))

(tests
  "millisecond time as a stable test"
  (with (p/run (! time))
    [% % %] := [_ _ _]
    (map int? *1) := [true true true]))
