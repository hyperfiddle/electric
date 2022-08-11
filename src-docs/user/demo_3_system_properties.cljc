(ns user.demo-3-system-properties
  (:require [clojure.string :as str]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.demo-3-system-properties)))

(defn system-properties [?s]
  #?(:clj (->> (System/getProperties)
               (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
               (into {}))))

(p/defn App []
  (dom/div
    (dom/h1 "System Properties")
    (let [!search (atom "") search (p/watch !search)]
      (ui/input {::dom/type :search
                 ::dom/placeholder "java.home"
                 ::ui/input-event (p/fn [e] (reset! !search (:value dom/node)))})
      (dom/div "Input: " search)
      (dom/table
        (p/server
          (p/for [[k v] (sort-by key (system-properties search))]
            (p/client
              (dom/tr
                (dom/td k)
                (dom/td {:style {:white-space :nowrap}} v)))))))))
