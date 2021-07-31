(ns hyperfiddle.photon-dom
  (:require [hfdl.lang :as p]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.style]))
  #?(:cljs (:import goog.events.EventType))
  #?(:cljs (:require-macros
             [hyperfiddle.photon-dom :refer
              [element fragment div span h1 table thead tbody select option]])))

(p/def parent)

(defn append-children [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-children [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount-all [parent & items]
  (m/observe
    (fn [!]
      (! (append-children parent items))
      (fn []
        (remove-children parent items)))))



(defn create-mount [parent type]
  #?(:cljs
     (m/observe
       (fn [!]
         (let [child (d/createElement type)]
           (d/appendChild parent child)
           (! child) #(d/removeNode child))))))

(defmacro element [type & body]
  `(p/binding [parent (unquote (create-mount parent ~(name type)))] ~@body))

(defn set-fragment! [e f]
  ;; TODO
  )

(defn set-text-content! [e t]
  #?(:cljs (d/setTextContent e t)))

(defn create-text [parent]
  #?(:cljs
     (m/observe
      (fn [!]
        (let [child (d/createTextNode "")]
          (d/appendChild parent child)
          (! child) #(d/removeNode child))))))

(defmacro text [text]
  `(set-text-content! (unquote (create-text parent)) ~text))

(defmacro div [& body]
  `(element :div ~@body))

(defmacro p [& body]
  `(element :p ~@body))

(defmacro attribute [k v]
  `(set-attribute! parent ~k ~v))

(defmacro property [k v]
  `(set-property! parent ~k ~v))

(defmacro class [value]
  `(attribute "class" ~value))

(defmacro strong [& body]
  `(element :strong ~@body))

(defmacro span [& body]
  `(element :span ~@body))

(defmacro h1 [& body]
  `(element :h1 ~@body))

(defmacro table [& body]
  `(element :table ~@body))

(defmacro thead [& body]
  `(element :thead ~@body))

(defmacro tbody [& body]
  `(element :tbody ~@body))

(defmacro ul [& body]
  `(element :ul ~@body))

(defmacro li [& body]
  `(element :li ~@body))

(defmacro code [& body]
  `(element :code ~@body))

(defn set-style! [parent style-map]
  #?(:cljs (goog.style/setStyle parent (clj->js style-map))))

(defmacro style [style-map]
  `(set-style! parent ~style-map))

(defn set-attribute! [e k v]
  #?(:cljs (.setAttribute e k v)))

(defn set-property! [e k v]
  #?(:cljs (d/setProperties e (clj->js {k v}))))

(defn get-attribute [e k]
  #?(:cljs (.getAttribute e k)))

(defn events [e t]
  #?(:cljs (m/observe (fn [!] (e/listen e t !) #(e/unlisten e t !)))))

(defn target-value [e] #?(:cljs (-> e .-target .-value)))

(def input-event
  #?(:cljs (.-INPUT EventType)))

(def click-event
  #?(:cljs (.-CLICK EventType)))

(def change-event
  #?(:cljs (.-CHANGE EventType)))

(defn log [e]
  #?(:cljs  (js/console.log e))
  e)

(defn event-target [e]
  #?(:cljs (.-target e)))

(defn get-value [e]
  #?(:cljs (.-value e)))

(defmacro fragment [& body] `(element :fragment ~@body))
(defmacro option [& body] `(element :option ~@body))

(defmacro input [& body] `(element :input ~@body))

;(p/defn input [x]
;  (let [el (doto (element "input")
;             (set-attribute! "type" "text")
;             (set-attribute! "value" x))]
;    (mount! el)
;    (-> ~(m/relieve {} (events el input-event))
;        event-target get-value)))
;
;;; TODO
;(p/defn radio [value options]
;  (p/for [[id text] options]
;    [(doto (element "input")
;       (set-attribute! "type" "radio"))
;     (doto (element "label")
;       (set-attribute! "for" id)
;       (set-text-content! text))]))
;
(defmacro select [#_selected & options]
 `(element :select ~@options)
 #_(set-attribute! parent "selected" selected))
;
;(tests
;  (options 2 (p/for [] (option id)))
;  )
