(ns user.demo-2-system-properties
  (:require [clojure.string :as str]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-2-system-properties)))

(defn system-properties [?s]
  #?(:clj (->> (System/getProperties)
               (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
               (into {}))))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "System Properties"))
    (let [filter (ui/input {:type :search, :placeholder "java.home"})]
      (dom/div (dom/text (str "Input: " filter)))
      (dom/table
        ~@(p/for [[k v] (sort-by key (system-properties filter))]
            ~@(dom/tr
                (dom/td (dom/text k))
                (dom/td (dom/text v) (dom/style {"white-space" "nowrap"}))))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/node (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
