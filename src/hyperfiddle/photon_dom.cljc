(ns hyperfiddle.photon-dom
  (:refer-clojure :exclude [for])
  (:require [hfdl.lang :as r :refer [defnode for]]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e]))
  #?(:cljs (:import goog.events.EventType))
  #?(:cljs (:require-macros
             [hyperfiddle.photon-dom :refer
              [mount! element-with-children fragment div span h1 table thead tbody tr td select option]])))

(r/def parent)

(defn append-children [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-children [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount-all [parent & items]
  (m/observe
    (fn [!]
      (! (append-children parent items))
      (fn []
        (remove-children parent items)))))

(defn mount [parent child]
  #?(:cljs
     (m/observe
       (fn [!]                                              ; fn is not yet supported in defnode
         (! nil)
         (d/appendChild parent child)
         #(d/removeNode child)))))

(defn element [type]
  #?(:cljs (d/createElement type)))

(defn set-fragment! [e f]
  ;; TODO
  )

(defn set-text-content! [e t]
  #?(:cljs (d/setTextContent e t)))

(defn set-attribute! [e k v]
  #?(:cljs (.setAttribute e k v)))

(defn get-attribute [e k]
  #?(:cljs (.getAttribute e k)))

(defn events [e t]
  #?(:cljs (m/observe (fn [!] (e/listen e t !) #(e/unlisten e t !)))))

(def input-event
  #?(:cljs (.-INPUT EventType)))

(defn event-target [e]
  #?(:cljs (.-target e)))

(defn get-value [e]
  #?(:cljs (.-value e)))

(r/defn mount! [child]
  ;(assert (bound? #'parent) "photon-dom/parent not in scope")
  (assert parent "photon-dom/parent not in scope")
  ~(mount parent child))

(r/defn element-with-children [type children]
  (let [e (element (name type))]
    (binding [parent e]
      (for [c children] (mount! c))) e))

;(defmacro element [type & body]
;  `(let [e (dom type ... )]
;    (r/binding [parent e]
;      ~@body)))

(with-out-str (println "a"))

(r/defn fragment [& children] (mount! (element-with-children :fragment children)))
(r/defn div [& children] (mount! (element-with-children :fragment children)))

(r/defn span [& children]
  (mount! (element-with-children :span children)))

(r/defn h1 [& children]
  (mount! (element-with-children :h1 children)))

(r/defn input [x]
  (let [el (doto (element "input")
             (set-attribute! "type" "text")
             (set-attribute! "value" x))]
    (mount! el)
    (-> ~(m/relieve {} (events el input-event))
        event-target get-value)))

(r/defn table [& children]
  (mount! (element-with-children :table children)))

(r/defn thead [& children]
  (mount! (element-with-children :thead children)))

(r/defn tbody [& children]
  (mount! (element-with-children :tbody children)))

(r/defn tr [& children]
  (mount! (element-with-children :tr children)))

(r/defn td [& children]
  (mount! (element-with-children :td children)))

;; TODO
(r/defn radio [value options]
  (for [[id text] options]
    [(doto (element "input")
       (set-attribute! "type" "radio"))
     (doto (element "label")
       (set-attribute! "for" id)
       (set-text-content! text))]))

(r/defn select [selected options]
  (mount!
    (binding [parent (element "select")]
      (mount! (for [[k v] options]
                   (doto (element "option")
                     (set-attribute! "value" k)
                     (set-text-content! v))))
      (set-attribute! parent "selected" selected)
      parent)))

(r/defn option [& body] (mount! (element-with-children :option children)))
