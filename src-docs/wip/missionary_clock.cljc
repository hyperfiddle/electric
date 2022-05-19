(ns wip.missionary-clock
  (:require [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(hyperfiddle.rcf/enable!)

(defn clock []
  (->> (m/ap
         (loop []
           (m/amb (m/? (m/sleep 10 1))
                  (recur))))
       (m/reductions + 0)
       (m/relieve {})
       
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
  @!it := 1
  % := ::notify
  (!it))
