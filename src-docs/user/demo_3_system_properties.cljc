(ns user.demo-3-system-properties
  (:require [clojure.string :as str]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-3-system-properties)))

(defn system-properties [?s]
  #?(:clj (->> (System/getProperties)
               (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
               (into {}))))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "System Properties"))
    (let [!filter (atom "") filter (p/watch !filter)]
      (ui/input {::dom/type :search ::dom/placeholder "java.home"
                 ::ui/input-event (p/fn [e] (reset! !filter (:value dom/node)))})
      (dom/div (dom/text (str "Input: " filter)))
      (dom/table
        (p/server
          (p/for [[k v] (sort-by key (system-properties filter))]
            (p/client
              (dom/tr
                (dom/td (dom/text k))
                (dom/td {:style {:white-space :nowrap}}
                  (dom/text v))))))))))
