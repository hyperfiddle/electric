(ns dustin.data
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]))

(defn abc []
  (->> (range 33 126) ; ascii ! through ~, which includes "password characters"
       (map (comp str char))))

(tests
  (count (abc)) := 93
  (nth (abc) 0) := "!"
  (nth (abc) (- 65 (int \!))) := "A"
  (nth (abc) (- 97 (int \!))) := "a"
  (nth (abc) (- 48 (int \!))) := "0"

  #_(apply str (repeatedly 10 #(rand-nth (abc))))
  )

(defn in-ns? [ns x]
  (= (str ns) (namespace (keyword x))))
