(ns hyperfiddle.client.ui
  (:require [hfdl.lang :refer [#?@(:clj [dataflow vars])]]
            #?(:clj [hyperfiddle.api :as hf]
               :cljs [hyperfiddle.api :as hf :refer [Input]])
            [clojure.edn :as edn]
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.client.router :as router])
            [clojure.walk :as walk])
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

(defn hack [!needle]
  #?(:cljs (aset js/window "hack" !needle)
     :clj nil))

(def set-input!
  #?(:cljs (fn [!needle]
             (fn [^js event]
               (js/console.log "setting" !needle event)
               (reset! !needle (.. event -target -value))))))

(def *inputs (volatile! {}))

(defn get-input! [input]
  (get @*inputs (.-id input)))

(defn new-input! [initial-value onChange]
  (let [id    #?(:clj (java.util.UUID/randomUUID)
                 :cljs (random-uuid))
        input (hf/->Input id initial-value onChange)]
    #?(:cljs (vswap! *inputs assoc id input))
    input))

(def exports (vars world render-cell render-row render-table
                   picklist change-route! >route by-id
                   into map
                   hack new-input! set-input!))
