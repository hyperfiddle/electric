(ns wip.missionary-clock
  (:require [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(defn clock []
  (->> (m/ap
         (loop []
           (m/amb (m/? (m/sleep 10 1))
                  (recur))))
       (m/reductions + 0)
       #_(m/reductions {} nil)
       #_(m/latest (fn [_]
                     #?(:clj  (System/currentTimeMillis)
                        :cljs (js/Date.now))))))

(tests
  (def >clock (clock))
  (def !it (>clock (fn [] (! ::notify))
                   (fn [] (! ::terminate))))
  % := ::notify
  @!it := 0
  % := ::notify
  ;✅✅❌
  ;expected: (= % :wip.demo-two-clocks/notify)
  ;actual: (not (= :hyperfiddle.rcf/timeout :wip.demo-two-clocks/notify))
  )
