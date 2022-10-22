(ns dustin.y2022.threadlocal-x
  (:require [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))


(def tls (ThreadLocal.))
(do
  (.set tls 0)
  (= (m/? (m/join vector
                  (m/sp (m/? (m/sleep 2)) (. tls (get)))
                  (m/sp (m/? (m/sleep 1)) (. tls (set 1)) (. tls (get)))))
     [1 1]))

(tests
  (def r (ThreadLocal.))
  (.set tls 0)
  (m/? (m/join vector
               (m/sp (m/? (m/sleep 2)) (. tls (get)))
               (m/sp (m/? (m/sleep 1)) (. tls (set 1)) (. tls (get)))))
  := [1 1])


(tests
  (def r (ThreadLocal.))
  (.set r 0)
  (m/? (m/join vector
               (m/sp (m/? (m/sleep 2)) (. r (get)))
               (m/sp (m/? (m/sleep 1)) (. r (set 1)) (. r (get)))))
  := [1 1])


(defn TL-get [r] (m/sp (. r (get))))
(defn TL-set [r x] (m/sp (. r (set x))))

(tests
  (def r (ThreadLocal.))
  (m/? (TL-set r 0))
  (m/? (m/join
         vector
         (m/sp (m/? (m/sleep 2))
               (m/? (TL-get r)))

         (m/sp (m/? (m/sleep 1))
               (m/? (TL-set r 1))
               (m/? (TL-get r)))))
  := [1 1])