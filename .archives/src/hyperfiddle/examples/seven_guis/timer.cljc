(ns hyperfiddle.examples.seven-guis.timer
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.timer :refer [Timer]]))
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

#?(:cljs
   (photon/run
     (binding [dom/parent (dom/by-id "timer")]
       (photon/$ Timer))))
