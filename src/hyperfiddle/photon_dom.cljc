(ns hyperfiddle.photon-dom
  (:refer-clojure :exclude [class for time])
  (:require [hyperfiddle.photon :as p]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.object :as o])
            #?(:cljs [goog.style])
            [clojure.string :as str])
  #?(:cljs (:require-macros
             [hyperfiddle.photon-dom :refer
              [element fragment div span h1 table thead tbody select option dl dt dd
               context event assign! for for-by]]))
  #?(:cljs (:import (goog.events EventType KeyCodes)
                    (goog.dom.animationFrame))))

(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(defn append-children [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-children [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn peer-error! [message]
  ;; To help us debug distribution
  (m/ap (throw (ex-info (str "Not available on this peer: " message) {}))))

(p/def parent)

(defn dom-element [type]
  #?(:cljs (d/createElement type)))

(defn text-node []
  #?(:cljs (d/createTextNode "")))

(defn before? [x y]
  (let [xl (count x)
        yl (count y)
        ml (min xl yl)]
    (loop [i 0]
      (if (< i ml)
        (let [xi (nth x i)
              yi (nth y i)]
          (if (== xi yi)
            (recur (inc i))
            (< xi yi)))
        (< xl yl)))))

(defn mount [parent child path]
  #?(:clj (peer-error! `mount)
     :cljs (m/observe
             (fn [!]
               (o/set child "--photon-path" path)
               (.insertBefore parent child
                 ;; TODO sublinear anchor search. skip list ?
                 (loop [anchor (.-firstChild parent)]
                   (when-not (nil? anchor)
                     (if (before? (o/get anchor "--photon-path") path)
                       (recur (.-nextSibling anchor)) anchor))))
               (! child) #(d/removeNode child)))))

(defmacro element [type & body]
  `(binding [parent (new (mount parent (dom-element ~(name type)) @p/path))] ~@body))

(defn set-fragment! [e f]
  ;; TODO
  )

(defn set-text-content! [e t]
  #?(:cljs (d/setTextContent e (str t))))

(defmacro text [text]
  `(set-text-content! (new (mount parent (text-node) @p/path)) ~text))

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
(defmacro th [& body] `(element :th ~@body))

(defmacro thead [& body]
  `(element :thead ~@body))

(defmacro tbody [& body]
  `(element :tbody ~@body))

(defmacro ul [& body]
  `(element :ul ~@body))

(defmacro li [& body]
  `(element :li ~@body))

(defmacro dl [& body] `(element :dl ~@body))
(defmacro dt [& body] `(element :dt ~@body))
(defmacro dd [& body] `(element :dd ~@body))

(defmacro code [& body]
  `(element :code ~@body))

(defn set-style! [parent style-map]
  #?(:cljs (goog.style/setStyle parent (clj->js style-map))))

(defmacro style [style-map]
  `(set-style! parent ~style-map))

(defn set-attribute! [e k v]
  #?(:cljs (js/requestAnimationFrame #(.setAttribute e k v))))

(defn set-property! [e k v]
  #?(:cljs (js/requestAnimationFrame #(d/setProperties e (clj->js {k v})))))

(defn get-attribute [e k]
  #?(:cljs (.getAttribute e k)))

(defn events [e t]
  #?(:cljs (m/observe (fn [!] (e/listen e t !) #(e/unlisten e t !)))))

(defn target-value [e] #?(:cljs (-> e .-target .-value)))

(defn target-checked [e] #?(:cljs (-> e .-target .-checked)))

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

(defn stop-event! [event]
  (.preventDefault event)
  (.stopPropagation event)
  event)

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
  `(new (events parent ~(map symbol [(str ".-" (str/upper-case (name type))) "goog.events.EventType"]))))

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

(defn ^:no-doc clock "browser-only clock throttled by requestAnimationFrame" []
  #?(:cljs
     (fn [n t]
       (let [c (->Clock 0 nil t)]
         (set! (.-callback c)
           (fn [_] (set! (.-raf c) 0) (n)))
         (n) c))))

;; The number of milliseconds elapsed since January 1, 1970
(p/def time (new (clock)))

(defmacro for-by [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(p/for-by ~kf [~s ~v]
       (binding [parent (do ~s parent)]
         (for-by ~kf ~bindings ~@body)))
    `(do ~@body)))

(defmacro for [bindings & body]
  `(for-by identity ~bindings ~@body))
