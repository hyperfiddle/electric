(ns leo.scope
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars local2 node thread]]
            [hyperfiddle.photon :as photon]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [missionary.core :as m]
            [hyperfiddle.q2 :refer [hfql]]))



(defnode typeahead-select [options]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select (for [x ($ options needle)]
                    (dom/option x))))))

(defnode options)
(defnode typeahead-select []
  (dom/div
    (let [needle (dom/input "")]
      (dom/select (for [x (options needle)]
                    (dom/option x))))))

(binding [options _]
  (typeahead-select))


(defnode typeahead-select [options]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select (for [x ($ options needle)]
                    (dom/option x))))))

(defnode typeahead-select [parent0 options]
  (let [parent1 (dom/div parent0)
        parent2 (dom/input parent1 "")
        needle (dom/input-changes parent2)
        parent3 (dom/select parent2)
        parent4 (for [x ($ options needle)] (dom/option parent3 x))]))



(defnode foo [x]
  (node [] x))

(defnode foo [n]
  (case n
    0 (node [_] 0)
    (node [x] (* n x))))

(photon/main ($ (foo 1)))





(defnode f [x]
  (let [y (inc x)]
    (node g [z]
      (+ y z))))


(photon/main
  (let [db (datomic.api/db ...)]
    (typeahead-select (node [needle]
                        (datomic.api/q ... needle db)))))


(defnode reactive-call [g x] (g x))

(photon/main
  (let [x ~(m/watch !x)
        y ~(m/watch !y)
        f (node [needle] (+ y needle))        ; constant signal
        g ~(m/seed [(node [needle] (+ y needle))])]
    [(reactive-call f x)
     (reactive-call ~g x)]))                  ; ?