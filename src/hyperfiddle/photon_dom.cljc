(ns hyperfiddle.photon-dom
  (:refer-clojure :exclude [class])
  (:require [hfdl.lang :as p]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.style])
            [clojure.string :as str])
  #?(:cljs (:import (goog.events EventType KeyCodes)
                    (goog.dom.animationFrame)))
  #?(:cljs (:require-macros
             [hyperfiddle.photon-dom :refer
              [element fragment div span h1 table thead tbody select option context event assign!]])))

(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(p/def parent)
(p/def node-index nil)

(defn append-children [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-children [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount-all [parent & items]
  (m/observe
    (fn [!]
      (! (append-children parent items))
      (fn []
        (remove-children parent items)))))

(defn create-mount [parent position type]
  #?(:cljs
     (m/observe
       (fn [!]
         (let [child (d/createElement type)]
           (if (nil? position)
             (d/appendChild parent child)
             (d/insertChildAt parent child position))
           (! child) #(d/removeNode child))))))

(defmacro element [type & body]
  `(binding [parent (unquote (create-mount parent node-index ~(name type)))] ~@body))

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

(defmacro button [& body]
  `(element :button ~@body))

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

(defmacro table [& body] `(element :table ~@body))
(defmacro tr [& body] `(element :tr ~@body))
(defmacro td [& body] `(element :td ~@body))

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

(def keydown-event
  #?(:cljs (.-KEYDOWN EventType)))

(defn keycode [e]
  #?(:cljs (.-keyCode e)))

(def keycode-enter
  #?(:cljs (.-ENTER KeyCodes)))

(defn log [e]
  #?(:cljs  (js/console.log e))
  e)

(defn event-target [e]
  #?(:cljs (.-target e)))

(defn get-value [e]
  #?(:cljs (.-value e)))

(defn set-value! [e x]
  #?(:cljs (set! (.-value e) x)))

(defn get-checked [e]
  #?(:cljs (.-checked e)))

(defn set-checked! [e v]
  #?(:cljs (set! (.-checked e) (boolean v))))

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

(defmacro canvas [& body]
  `(element :canvas ~@body))

(defmacro context [type]
  `(.getContext parent ~(name type)))

(defmacro event [type]
  `(unquote (events parent ~(map symbol [(str ".-" (str/upper-case (name type))) "goog.events.EventType"]))))

(defmacro assign! [prop value]
  `(set! (~(symbol (str ".-" (name prop))) parent) value))

#?(:cljs
   (deftype Clock [^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn
     (-invoke [_]
       (if (zero? raf)
         (set! callback nil)
         (do (.cancelAnimationFrame js/window raf)
             (terminator))))
     IDeref
     (-deref [_]
       (if (nil? callback)
         (terminator)
         (set! raf (.requestAnimationFrame js/window callback)))
       (.now js/Date))))

(def ^:no-doc >clock
  #?(:cljs
     (fn [n t]
       (let [c (->Clock 0 nil t)]
         (set! (.-callback c)
           (fn [_] (set! (.-raf c) 0) (n)))
         (n) c))))

;; The number of milliseconds elapsed since January 1, 1970
(p/def clock ~>clock)

(def exports (p/vars click-event create-mount create-text events
                     set-attribute! set-style! set-text-content!))
