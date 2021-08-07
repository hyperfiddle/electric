(ns hyperfiddle.client.examples.seven-guis.timer
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [devcards.core :as dc]
            [missionary.core :as m]
            [hyperfiddle.client.examples.card :refer [dom-node]])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.timer :refer [Timer]]))
  )

(def second-elapsed
  (m/ap (loop [x 100]
          (m/? (m/sleep 100))
          (m/amb=
           x
           (recur x)))))

(def initial-goal 1000)

(photon/defn Timer []
  (let [!time (atom 0)
        !goal (atom initial-goal)]
    (when (< ~(m/watch !time) ~(m/watch !goal))
      (swap! !time + ~second-elapsed))
    (dom/div
     (dom/div
      (dom/text "Elapsed Time: ")
      (dom/element "progress"
                   (dom/attribute "max" ~(m/watch !goal))
                   (dom/attribute "value" ~(m/watch !time))))
     (dom/text (-> ~(m/watch !time)
                   (/ 100)
                   (Math/floor)
                   (/ 10)
                   (str " s")))
     (dom/div
      (dom/text "Duration :")
      (reset! !goal (dom/input
                     (dom/attribute "type" "range")
                     (dom/attribute "min" "0")
                     (dom/attribute "max" "10000")
                     (dom/attribute "value" initial-goal)
                     ~(->> (dom/events dom/parent "input")
                           (m/eduction (map dom/event-target)
                                       (map dom/get-value))
                           (m/reductions {} initial-goal)
                           (m/relieve {})))))
     (dom/button
      (dom/text "Reset")
      (reset! !time ({}
                     ~(->> (dom/events dom/parent "click")
                           (m/reductions {} nil)
                           (m/relieve {}))
                     0))))))

(dc/defcard timer
  "# 4 — Timer

   Challenges: concurrency, competing user/signal interactions, responsiveness.

   ![](https://eugenkiss.github.io/7guis/static/timer.ed46b6b4.png)

   The task is to build a frame containing a gauge `G` for the elapsed time `e`,
   a label which shows the elapsed time as a numerical value, a slider `S` by
   which the duration `d` of the timer can be adjusted while the timer is running
   and a reset button `R`.

   - [x] Adjusting `S` must immediately reflect on `d` and not only when `S` is
   released.
   - [x] It follows that while moving `S` the filled amount of `G` will (usually)
   change immediately.
   - [x] When `e ≥ d` is true then the timer stops (and `G` will be full).
   - [x] If, thereafter, `d` is increased such that `d > e` will be true then the
   timer restarts to tick until `e ≥ d` is true again.
   - [x] Clicking R will reset `e` to zero.
"
  (dom-node
   (fn [_ node]
     (photon/run
       (photon/binding [dom/parent node]
         (photon/$ Timer))))))
