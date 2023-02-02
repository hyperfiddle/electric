(ns hyperfiddle.zero
  "Photon standard library (experimental, idioms are not established yet).
  Badly named, should be moved into photon namespace."
  (:refer-clojure :exclude [empty? time])
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros [hyperfiddle.zero :refer [pick]])))

(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    (fn
      ([v] (reset! !state v))
      ([n t] (>state n t)))))



(defmacro pick "head for flows. return first or nothing. Note that in Clojure you can't
return nothing (you return nil) but in flows nothing is different than nil." [t]
  `(let [x# (m/? t)]
     (case x# ::empty (m/amb) x#)))

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

(tests
  (def !x (atom 0))
  (def !e (atom nil))
  (defn sub [!]
    (reset! !e !)
    #(reset! !e nil))
  (with
    (p/run
      (let [x (new (m/watch !x))]
        (tap (p/impulse x (m/observe sub)))))
    % := nil
    (@!e 1)
    % := 1
    @!e := nil
    (swap! !x inc)
    % := nil))
