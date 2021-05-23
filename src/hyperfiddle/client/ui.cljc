(ns hyperfiddle.client.ui
  (:refer-clojure :exclude [meta time])
  (:require [clojure.set :as set]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.lang :refer [#?@(:clj [vars])]]
            [missionary.core :as m]
            #?(:cljs [clojure.edn :as edn])
            #?(:cljs [hyperfiddle.client.router :as router])
            )
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]])))

;; TODO drop
(def world #?(:clj "server" :cljs "client"))

;; TODO belongs here?
(def change-route! #?(:cljs (comp router/set-route! edn/read-string)))

;; -----------------------------------------------------------------------

(defn create-text-node [initial-value] #?(:cljs (.createTextNode js/document (str initial-value))))
(defn create-tag-node [tag] #?(:cljs (.createElement js/document (name tag))))
(defn by-id [id] #?(:cljs (js/document.getElementById id)))
(defn set-text-content! [elem text] #?(:cljs (set! (.-textContent elem) text)))

(defn text [>text]
  (let [elem (create-text-node "")]
    (m/stream! (m/latest #(set-text-content! elem %) >text))
    (m/ap elem)))

(defn append-childs [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-childs [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount [parent items]
  (m/observe
   (fn [!]
     (! (append-childs parent items))
     (fn []
       (remove-childs parent items)))))

(defn shadow-props [elem]
  (aget elem "hf-shadow-props"))

(defn set-prop! [elem k v]
  (let [sp     (shadow-props elem)
        actual (get sp k)]
    #_(js/console.log {:prop               k
                       :old                actual
                       :new                v
                       :equal?             (= v actual)
                       :will-write-to-dom? (not= v actual)})
    (when (not= v actual)
      (aset elem "hf-shadow-props" (assoc sp k v))
      (aset elem k v))))

(defn patch-properties! [elem props]
  (let [old-props (shadow-props elem)
        rets      (set/difference (set (keys old-props)) (set (keys props)))]
    (when (seq rets)
      (run! (fn [k] (.removeAttribute elem k)) rets))
    (run! (fn [[k v]]
            (set-prop! elem k v))
          props)))

(defn tag
  ([elem] (tag elem nil nil))
  ([elem >props] (tag elem >props nil))
  ([elem >props & >childs]
   (let [elem (create-tag-node elem)]
     (when >props
       (m/stream! (m/latest #(patch-properties! elem %) >props)))
     (when (seq (filter identity >childs))
       ;; if contains child -> replacechild
       ;; else appendChild
       (m/stream! (switch (apply m/latest #(mount elem %&) >childs))))
     (m/ap elem))))

(defn append-child! [parent >child]
  (m/stream! (switch (m/latest #(mount parent [%]) >child)))
  parent)

(defn mount-component-at-node! [id >component]
  (append-child! (by-id id) >component))

(def exports (vars world text tag change-route! by-id mount-component-at-node!))
