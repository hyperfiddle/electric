(ns .
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))

(dom/h1 "a controlled input with looped state")
(let [a (p/with-cycle [a "hello world"]
          (dom/Input. a))]
  (dom/div a))

; loop/recur
(dom/h1 "a controlled input with looped state")
(let [a (loop [a "hello world"]
          (recur (dom/Input. a)))]
  (dom/div a))

(dom/h1 "controlled input - edn")
(let [a (loop [a :category/terminated-contract]
          (recur (-> (dom/Input. (pr-str a)) rosie/read-edn-str)))]
  (dom/div a))

; data State a = [Boolean, Option a]

(dom/h1 "controlled input with pending; disable input while waiting")
(let [a (:ret (p/with-cycle [{:keys [busy] :as s} {:busy false
                                                   :ret nil}]
                (println 's s)
                (try {:busy false
                      :ret (p/server (identity (p/client (p/with-cycle [a "hello world"]
                                                           (dom/pre (pr-str {:stale busy}))
                                                           (dom/Input. a #_{:disabled busy})))))}
                     (catch Pending _
                       (println 'pending)
                       (assoc s :busy true)))))] ; :ret is stale - latency
  (dom/div a))

; L: it will do something weird
; 1. reset atom
; 2. run in parallel and throw pending
; 3 print pending, reset busy true
; result is true, returned by reset
; frame continues to propoagate
; at the end of the propagation, another frame is run in response to !busy changing to true, which was changed by downwards which is a cycle.
;     reactor is about dags, so if you build a cycle artificially, it schedules a new propagation turn
;
(let [a (let [!busy (atom false) busy (p/watch !busy)]
          (println 'busy)
          (reset! !busy
                  (try (p/server (identity (p/client (p/with-cycle [a "hello world"]
                                                       (dom/pre (pr-str {:stale busy}))
                                                       (dom/Input. a #_{:disabled busy})))))
                       false
                       (catch Pending _
                         (println 'pending)
                         true))))]
  (dom/div a))

(let [a (loop [busy false]
          (println 'busy)
          (try (recur false) ; recur in non-tail position due to implicit do
               (p/server (identity (p/client (loop [a "hello world"]
                                               (dom/pre (pr-str {:stale busy}))
                                               (recur (dom/Input. a #_{:disabled busy}))))))
               (catch Pending _
                 (println 'pending)
                 (recur true))))] ; :ret is stale - latency
  (dom/div a))
; L: what does it mean to recur twice, these are simultaneous under FRP

; can it be factored into tail position?
; does it matter if it's in the tail position like it does for Clojure?
