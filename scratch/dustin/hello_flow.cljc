(ns dustin.hello-flow
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars system]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hfdl.impl.util :as u]))

(declare table tr td thread)

(defnode hello-world-widgets [db e]
  (table
    (let [!e (thread (d/touch (d/entity db e)))]
      (for [[k v] !e]                                       ; macro too hard
        ~@(tr (td (pr-str k))
              (td (pr-str v)))))))

(defnode foo [z]
  (let [a (inc z)
        b (dec z)]
    (js/console.log ~@(+ a b))))

(declare upper-case)









































































    (defnode foo [xs]
      (js/console.log
        (for [x xs]
          (upper-case
            ~@(str (type (inc x)))))))



























































