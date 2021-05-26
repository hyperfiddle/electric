(ns hyperfiddle.client.ui
  (:refer-clojure :exclude [meta time])
  (:require [clojure.set :as set]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.lang :refer [#?@(:clj [vars])]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [clojure.test :as t])
            #?(:cljs [clojure.edn :as edn])
            #?(:cljs [cljs.test :as t])
            #?(:cljs [hyperfiddle.client.router :as router])
            #?(:cljs [goog.dom :as dom])
            #?(:cljs [goog.style :as sty]))
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]]
                            [hyperfiddle.rcf :refer [tests]])))

;; TODO belongs here?
(def change-route! #?(:cljs (comp router/set-route! edn/read-string)))

;; -----------------------------------------------------------------------

(defn create-text-node [initial-value] #?(:cljs (.createTextNode js/document (str initial-value))))
(defn create-tag-node [tag] #?(:cljs (.createElement js/document (name tag))))
(defn by-id [id] #?(:cljs (js/document.getElementById id)))
(defn set-text-content! [elem text] #?(:cljs (set! (.-textContent elem) text)))

(defn text [>text]
  (m/observe
   (fn [!]
     (let [elem        (create-text-node "")
           text-stream (m/stream! (m/latest #(set-text-content! elem %) >text))]
       (! elem)
       (fn []
         (text-stream))))))

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

(defn prop-name
  "See goog.dom.setProperties. https://github.com/google/closure-library/blob/master/closure/goog/dom/dom.js#L460"
  [k]
  (let [nom (name k)]
    (case nom
      "class"       "className"
      "for"         "htmlFor"
      "cellpadding" "cellPadding"
      "cellspacing" "cellSpacing"
      "colspan"     "colSpan"
      "frameborder" "frameBorder"
      "maxlength"   "maxLength"
      "rowspan"     "rowSpan"
      "usemap"      "useMap"
      "valign"      "vAlign"
      nom)))

(defn set-style! [elem styles]
  (let [old-props (shadow-props elem)
        old-style (:style old-props)
        rets      (set/difference (set (keys old-style)) (set (keys styles)))
        styles'   (reduce (fn [r k] (assoc r k nil)) styles rets)]
    (->> (assoc old-props :style styles')
         (aset elem "hf-shadow-props"))
    #?(:cljs (sty/setStyle elem (clj->js styles')))))

(defn set-prop! [elem k v]
  (let [sp     (shadow-props elem)
        k      (prop-name k)
        actual (get sp k)]
    #_(js/console.log {:prop               k
                       :old                actual
                       :new                v
                       :equal?             (= v actual)
                       :will-write-to-dom? (not= v actual)})
    (when (not= v actual)
      (if (= "style" (name k))
        (set-style! elem v)
        (do
          (aset elem "hf-shadow-props" (assoc sp k v))
          #?(:cljs (dom/setProperties elem (js-obj k v))))))))

(defn patch-properties! [elem props]
  (let [old-props (shadow-props elem)
        rets      (set/difference (set (keys old-props)) (set (keys props)))]
    (when (seq rets)
      (run! (fn [k]
              (let [k (prop-name k)]
                (aset elem "hf-shadow-props" (dissoc old-props k))
                #?(:cljs (.removeAttribute elem k))))
            rets))
    (run! (fn [[k v]]
            (set-prop! elem k v))
          props)))

;; f :: Flow {k a} -> (a -> Flow b) -> Flow {k b}
;; join-all :: Flow {k Flow a} -> Flow {k a}

;; switch :: Flow [Flow a] -> Flow [a] ; semi-lazy join
;; This impl is eager. Using this until we get cancellation on switch.
(defn switch' [>a]
  (m/ap (m/?< (m/?< >a))))

(defn tag [name >props & >childs]
  (m/observe
   (fn [!]
     (let [elem            (create-tag-node name)
           children-stream (when (seq (filter identity >childs))
                             ;; if contains child -> replacechild
                             ;; else appendChild
                             (m/stream! (switch' (apply m/latest #(mount elem %&) >childs))))
           props-stream    (when >props (m/stream! (m/latest #(patch-properties! elem %) >props)))]
       (! elem)
       (fn []
         (when children-stream
           (children-stream))
         (when props-stream
           (props-stream))
         (run! (fn [k]
                 #?(:cljs (.removeAttribute elem k)))
               (set (keys (shadow-props elem))))
         (aset elem "hf-shadow-props" nil))))))

(defn append-child! [parent >child]
  (m/observe
   (fn [!]
     (let [child-stream (m/stream! (switch' (m/latest #(mount parent [%]) >child)))]
       (! parent)
       (fn []
         (child-stream))))))

(defn mount-component-at-node! [id >component]
  (append-child! (by-id id) >component))

(def exports (vars text tag change-route! by-id mount-component-at-node!))

