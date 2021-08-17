(ns hyperfiddle.client.examples.seven-guis.crud
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.crud :refer [CRUD]]))
  )

(def initial-goal 1000)

(def initial-state {:id nil, :name "", :surname ""})

(defn surname-matches [a person]
  (str/starts-with? (:surname person) a))

(defn log
  ([x] (prn x) x)
  ([m x] (prn m x) x))

(defn create [people !state !out _e]
  (let [{:keys [name surname]} @!state
        id                     (inc (apply max (map :id people)))]
    (reset! !state initial-state)
    (swap! !out conj {:id id, :name name, :surname surname})))

(defn update' [people !state !out _e]
  (let [{:keys [id] :as user} @!state]
    (reset! !state initial-state)
    (reset! !out (mapv (fn [p] (if (= id (:id p)) user p)) people))))

(defn delete [people !state !out _e]
  (let [id (:id @!state)]
    (reset! !state initial-state)
    (reset! !out (vec (remove #(= id (:id %)) people)))))

(defn select [!state p _e]
  (reset! !state p))

(photon/defn CRUD [{:keys [people]}]
  (let [!state   (atom initial-state)
        !out     (atom people)
        !filter  (atom "")
        filtered (filter (partial surname-matches ~(m/watch !filter)) people)]
    (dom/div
     ;; (dom/text (pr-str ~(m/watch !state)))
     ;; (dom/text (pr-str ~(m/watch !filter)))
     (dom/style {"width"            "480px"
                 "background-color" "#EEEEEE"
                 "display"          "grid"
                 "grid-gap"         "0.5rem"
                 "padding"          "1rem"
                 "border-radius"    "0.5rem"})
     (dom/span
      (dom/style {"grid-column" "1 / 2"})
      (dom/text "Filter prefix: "))
     (reset! !filter (dom/input
                      (dom/style {"grid-column" "3"
                                  "width"       "100px"})
                      ~(->> (dom/events dom/parent "input")
                            (m/eduction (map dom/target-value))
                            (m/reductions {} "")
                            (m/relieve {}))))
     (dom/ul
      (dom/style {"background-color" "white"
                  "margin"           "0"
                  "padding"          "0"
                  "grid-column"      "1 / 4"
                  "grid-row"         "2 / 6"
                  "list-style-type"  "none"
                  "max-height"       "5rem"
                  "overflow-y"       "scroll"})
      (photon/for [[idx {:keys [name surname] :as p}] (map-indexed vector filtered)]
        (photon/binding [dom/node-index idx]
          (dom/li (dom/text (str surname ", " name))
                  (dom/style {"cursor" "pointer"})
                  ~(->> (dom/events dom/parent "click")
                        (m/eduction (map (partial select !state p)))
                        (m/reductions {} nil)
                        (m/relieve {}))))))
     (dom/span
      (dom/style {"grid-column" "4" "grid-row" "2"})
      (dom/text "Name:"))
     (let [name ~(m/eduction (map :name) (dedupe) (m/watch !state))]
       (swap! !state assoc :name (dom/input
                                  (dom/attribute "value" name)
                                  (dom/style {"grid-column" "5", "grid-row" "2"})
                                  ~(->> (dom/events dom/parent "input")
                                        (m/eduction (map dom/target-value))
                                        (m/reductions {} name)
                                        (m/relieve {})))))
     (dom/span
      (dom/style {"grid-column" "4" "grid-row" "3"})
      (dom/text "Surname:"))
     (let [surname ~(m/eduction (map :surname) (dedupe) (m/watch !state))]
       (swap! !state assoc :surname (dom/input
                                     (dom/attribute "value" surname)
                                     (dom/style {"grid-column" "5", "grid-row" "3"})
                                     ~(->> (dom/events dom/parent "input")
                                           (m/eduction (map dom/target-value))
                                           (m/reductions {} surname)
                                           (m/relieve {})))))
     (dom/div
      (dom/style {"grid-column"    "1 / 5", "grid-row" "7"
                  "display"        "grid"
                  "grid-gap"       "0.5rem"
                  "grid-auto-flow" "column"})
      (dom/button (dom/text "Create")
                  ~(->> (dom/events dom/parent "click")
                        (m/eduction (map (partial create people !state !out)))
                        (m/reductions {} nil)
                        (m/relieve {})))
      (dom/button (dom/text "Update")
                  ~(->> (dom/events dom/parent "click")
                        (m/eduction (map (partial update' people !state !out)))
                        (m/reductions {} nil)
                        (m/relieve {})))
      (dom/button (dom/text "Delete")
                  ~(->> (dom/events dom/parent "click")
                        (m/eduction (map (partial delete people !state !out)))
                        (m/reductions {} nil)
                        (m/relieve {})))
      ~(m/watch !out)))))

(def people {:people [{:id 1, :name "John",   :surname "Doe"}
                      {:id 2, :name "Jenson", :surname "Jones"}
                      {:id 3, :name "Lukas",  :surname "Luna"}]})

(defn crud []
  (let [!state (atom people)]
    (photon/run
      (photon/binding [dom/parent (dom/by-id "crud")]
        (->> (photon/$ CRUD ~(m/eduction (dedupe) (m/watch !state)))
             (swap! !state assoc :people))))))

(crud)
