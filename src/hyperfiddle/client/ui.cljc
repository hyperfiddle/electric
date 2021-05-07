(ns hyperfiddle.client.ui
  (:require [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]))

(defn render-cell [_k v]
  [:td v])

(defn render-row [row]
  (into [:tr] (map (fn [[k v]] (render-cell k v)) row)))

(defn render-table [rows props]
  (let [rows (if (map? rows) [rows] rows)]
    (dataflow
     (when (seq rows)
       (into [:table props]
             (map render-row rows))))))

(defn picklist [v {::hf/keys [options] :as props}]
  (dataflow
   (into [:select {:value v}]
         (for [opt options]
           [:option opt]))))
