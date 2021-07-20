(ns dustin.scope
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars local2 node thread]]
            [hyperfiddle.photon :as photon]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [missionary.core :as m]
            [hyperfiddle.q2 :refer [hfql]]))


(defnode typeahead-select [v options]
  (dom/div
      (let [needle (dom/input "")]
        (dom/select v (for [x (options needle)]
                        (dom/option x))))))

(photon/main
  ~@(binding [hf/*$* (datomic.api/db ...)
              dom/parent (js/document.querySelector "#root")]
      (typeahead-select 42 (node [needle]
                             (datomic.api/q ... needle hf/*$*)))))

(try
  ((try (fn [] (/ 1 0))
        (catch ArithmeticException _)))
  (catch _ _))

;(fn [] (fn [] (fn [])))

; simpler

(defnode typeahead-select [f]
  (dom/div
    (let [needle (dom/input "")]
      (dom/div (let [x (f needle)]
                      (dom/option x))))))

(photon/main
  ~@(binding [hf/*$* (datomic.api/db ...)
              dom/parent (js/document.querySelector "#root")]
      (typeahead-select (node [needle]
                          (datomic.api/q ... needle hf/*$*)))))