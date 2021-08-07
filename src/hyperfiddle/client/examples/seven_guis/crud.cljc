(ns hyperfiddle.client.examples.seven-guis.crud
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [devcards.core :as dc]
            [missionary.core :as m]
            [hyperfiddle.client.examples.card :refer [dom-node]]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.crud :refer [CRUD]]))
  )

(def initial-goal 1000)

(def initial-state {:id nil, :name "", :surname ""})

(defn surname-matches [a b]
  (str/starts-with? b a))

(defn log
  ([x] (prn x) x)
  ([m x] (prn m x) x))

(defn create [people !state _e]
  (let [{:keys [name surname]} @!state
        id                     (inc (apply max (map :id people)))]
    (reset! !state initial-state)
    (prn "45" id _e people)
    (concat people (list {:id id, :name name, :surname surname}))))

(defn update' [people !state _e]
  (let [{:keys [id] :as user} @!state]
    (reset! !state initial-state)
    (prn "38" id _e people)
    (map (fn [p] (if (= id (:id p)) user p)) people)))

(defn delete [people !state _e]
  (let [id (:id @!state)]
    (reset! !state initial-state)
    (prn "29" id _e people)
    (doto (remove #(= id (:id %)) people) (prn "30"))))

(photon/defn CRUD [{:keys [people]}]
  (let [!state (atom initial-state)
        !filter (atom "")
        #_#_filtered (filter (partial surname-matches "a") people)]
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
                  "list-style-type"  "none"})
      (photon/for [{:keys [name surname] :as p} people]
        (reset! !state ({} (dom/li (dom/text (str surname ", " name))
                                   ~(->> (dom/events dom/parent "click")
                                         (m/reductions {} nil)
                                         (m/relieve {})))
                        p))))
     (dom/span
      (dom/style {"grid-column" "4" "grid-row" "2"})
      (dom/text "Name:"))
     #_(swap! !state assoc :name) (dom/input
                                   (dom/attribute "value" (:name ~(m/watch !state)))
                                   (dom/style {"grid-column" "5", "grid-row" "2"})
                                   ~(->> (dom/events dom/parent "input")
                                         (m/eduction (map dom/target-value)
                                                     (map dom/get-value))
                                         (m/reductions {} "")
                                         (m/relieve {})))
     (dom/span
      (dom/style {"grid-column" "4" "grid-row" "3"})
      (dom/text "Surname:"))
     #_(swap! !state assoc :surname) (dom/input
                                      (dom/attribute "value" (:surname ~(m/watch !state)))
                                      (dom/style {"grid-column" "5", "grid-row" "3"})
                                      ~(->> (dom/events dom/parent "input")
                                            (m/eduction (map dom/target-value)
                                                        (map dom/get-value))
                                            (m/reductions {} "")
                                            (m/relieve {})))
     (dom/div
      (dom/style {"grid-column"    "1 / 5", "grid-row" "7"
                  "display"        "grid"
                  "grid-gap"       "0.5rem"
                  "grid-auto-flow" "column"})
      (reset! !state ({} (dom/button
                          (dom/text "Create")
                          ~(->> (dom/events dom/parent "click")
                                (m/eduction (map (partial create people !state)))
                                (m/reductions {} nil)
                                (m/relieve {})))
                      initial-state))
      ~(m/gather
       #'(or (dom/button (dom/text "Update")
                       ~(->> (dom/events dom/parent "click")
                             (m/eduction (map (partial update' people !state)))
                             (m/reductions {} nil)
                             (m/relieve {})))
           people)
       #'(log (or (dom/button (dom/text "Delete")
                            ~(->> (dom/events dom/parent "click")
                                  (m/eduction (map (partial delete people !state)))
                                  (m/reductions {} nil)
                                  (m/relieve {})))
                people)))))))

(dc/defcard crud
  "# 5 — CRUD

  ![](https://eugenkiss.github.io/7guis/static/crud.515ce94b.png)

The task is to build a frame containing the following elements: a textfield Tprefix, a pair of textfields Tname and Tsurname, a listbox L, buttons BC, BU and BD and the three labels as seen in the screenshot. L presents a view of the data in the database that consists of a list of names. At most one entry can be selected in L at a time. By entering a string into Tprefix the user can filter the names whose surname start with the entered prefix—this should happen immediately without having to submit the prefix with enter. Clicking BC will append the resulting name from concatenating the strings in Tname and Tsurname to L. BU and BD are enabled iff an entry in L is selected. In contrast to BC, BU will not append the resulting name but instead replace the selected entry with the new name. BD will remove the selected entry. The layout is to be done like suggested in the screenshot. In particular, L must occupy all the remaining space."
  (dom-node
   (fn [!state node]
     (photon/run
       (photon/binding [dom/parent node]
         (->> (photon/$ CRUD ~(m/eduction (dedupe) (m/watch !state)))
              (log "out")
              (swap! !state assoc :people))
         #_(doto (reset! !state ) prn)))))
  {:people [{:id 1, :name "John",   :surname "Doe"}
            {:id 2, :name "Jenson", :surname "Jones"}
            {:id 3, :name "Lukas",  :surname "Luna"}]}

  {:inspect-data true})
