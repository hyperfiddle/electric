(ns hyperfiddle.client.ui
  (:refer-clojure :exclude [meta time])
  (:require [clojure.edn :as edn]
            #?(:clj [clojure.java.io :as io])
            [clojure.set :as set]
            [clojure.walk :as walk]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.lang :refer [#?@(:clj [dataflow vars])]]
            #?(:clj [hyperfiddle.api :as hf]
               :cljs [hyperfiddle.api :as hf :refer [Input]])
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.client.router :as router])
            )
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]]
                            [hyperfiddle.client.ui :refer [html gen-tags!]])))

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

;; ---------------------------------------------------

(defn create-element [tag]
  #?(:cljs (if (keyword? tag)
             (.createElement js/document (name tag))
             (.createTextNode js/document (str tag)))))

(defn set-text-content! [elem text] (set! (.-textContent elem) text))

(defn text   [>text]
  (let [elem (create-element "")]
    (m/stream! (m/latest (fn [text] (set! (.-textContent elem) text)) >text))
    (m/ap elem)))

;; (defn create-element' [tag]
;;   (m/ap (create-element tag)))

;; (defn text' [>text _]
;;   (dataflow (doto @(create-element' "")
;;               (set-text-content! @>text))))

(defn mount [parent items]
  (m/observe
   (fn [!]
     (! (run! #(.appendChild parent %) items))
     (fn []
       (run! #(.removeChild parent %) items)))))

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
   (let [elem (create-element elem)]
     (when >props
       (prn elem "props!" >props)
       (m/stream! (m/latest (fn [props]
                              (patch-properties! elem props))
                            >props)))
     (when (seq (filter identity >childs))
       (prn elem "childs!" (seq (filter identity >childs)))
       (m/stream! (switch (apply m/latest (fn [& childs]
                                            ;; if contains child -> replacechild
                                            ;; else appendChild
                                            (mount elem childs))
                                 >childs))))
     (m/ap elem))))

(defn append-child [elem >child]
  (m/stream! (switch (m/latest (fn [child]
                                 (mount elem [child]))
                               >child)))
  (m/ap elem))

(defn mount-component-at-node! [id elem]
  (append-child (by-id id) elem))

(defn hiccup? [x]
  (and (vector? x)
       (keyword? (first x))))

(defn props-setter [props keys & vals]
  (apply assoc props (interleave keys vals)))

(defn rewrite-props [props]
  (when props
    (let [{:keys [props keys vals]}
          (reduce-kv (fn [r k v]
                       (if (and (sequential? v)
                                (#{'cljs.core/unquote 'clojure.core/unquote} (first v)))
                         (-> (update r :keys conj k)
                             (update :vals conj (second v)))
                         (update r :props assoc k v)))
                     {:props {}
                      :keys  []
                      :vals  []}
                     props)]
      (if (empty? keys)
        `(m/ap ~props)
        `(m/latest (partial props-setter ~props ~keys) ~@vals)))))

(defmacro gen-tags! []
  (let [tags (:tags (edn/read-string (slurp (io/file "./resources/html_spec.edn"))))]
    (cons 'do (for [[tag data] tags] `(def ~(symbol tag) ~(:doc data) (partial tag ~tag))))))

(gen-tags!)

(defmacro html [body]
  (walk/prewalk (fn [x]
                  (if (hiccup? x)
                    (let [[tag props & children] x
                          props?                 (map? props)
                          props'                 (if props? props nil)
                          children               (filter some? (if-not props? (cons props children) children))]
                      (if props?
                        `(tag ~tag ~(rewrite-props props') ~@children)
                        `(tag ~tag nil ~@children)))
                    x))
                body))

(defn root [>route]
  (let [!title (atom "Hyperfiddle UI")
        >title (m/watch !title)]
    (html
     [:div {"style" "border: 1px gray solid; margin: 1rem; padding: 1rem"}
      [:h1 (text >title)]
      [:pre (text >route)]
      [:input {"type"        "text"
               "placeholder" "Change title"
               "className"   "hf-cm-input"
               "value"       ~>title
               "onkeyup"     #(reset! !title (.. % -target -value))}]])))


(defn hfql-ui [>hfql]
  (pre nil >hfql))

(def exports (vars world render-cell render-row render-table
                   picklist change-route! >route by-id
                   into map
                   hack new-input! set-input!
                   mount-component-at-node! create-element
                   root hfql-ui))
