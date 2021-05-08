(ns hyperfiddle.client.ui
  (:require [hfdl.lang :refer [#?@(:clj [dataflow vars])]]
            [hyperfiddle.api :as hf]
            [clojure.edn :as edn]
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.client.router :as router]))
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]])))

(def world #?(:clj "server" :cljs "client"))

(defn render-cell [_k v]
  [:td v])

(defn render-row [row]
  (into [:tr] (map (fn [[k v]] (render-cell k v)) row)))

(def render-table
  #?(:clj
     (fn [rows props]
       (let [rows (if (map? rows) [rows] rows)]
         (dataflow
           (when (seq rows)
             (into [:table props]
               (map render-row rows))))))))

(def picklist
  #?(:clj
     (fn [v {::hf/keys [options] :as props}]
       (dataflow
         (into [:select {:value v}]
           (for [opt options]
             [:option opt]))))))

(def >route #?(:cljs (m/watch router/!route)))
(def change-route! #?(:cljs (comp router/set-route! edn/read-string)))
(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(def exports (vars world render-cell render-row render-table picklist change-route! >route by-id))