(ns hyperfiddle.photon-dom3
  (:refer-clojure :exclude [time])
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.object :as o])
            #?(:cljs [goog.style])
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.photon-dom3 #_#_:refer [#_[a abbr address area article aside audio b bdi bdo blockquote br button canvas cite code data datalist del details dfn dialog div dl em embed fieldset figure footer form h1 h2 h3 h4 h5 h6 header hgroup hr i iframe img input ins kbd label link main map mark math menu #_meta itemprop meter nav noscript object ol output p picture pre progress q ruby s samp script section select slot small span strong sub sup table template textarea #_time u ul var video wbr]]]))
  #?(:cljs (:import (goog.events KeyCodes))))

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
  #?(:cljs (m/observe
             (fn [!]
               (o/set child "--photon-path" path)
               (.insertBefore parent child
                 ;; TODO sublinear anchor search. skip list ?
                 (loop [anchor (.-firstChild parent)]
                   (when-not (nil? anchor)
                     (if (before? (o/get anchor "--photon-path") path)
                       (recur (.-nextSibling anchor)) anchor))))
               (! child) #(prn 'unmount (d/removeNode child))))))

(defn dom-element [tag] #?(:cljs (d/createElement tag)))

(p/def parent)

(defmacro element [type & body]
  `(binding [parent (new (mount parent (dom-element ~(name type)) @p/path))] 
     (do parent ~@body)))

(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(defn text-node [] #?(:cljs (d/createTextNode "")))
(defn set-text-content! [e t] #?(:cljs (d/setTextContent e (str t))))
(defmacro text [& strs] `(set-text-content! (new (mount parent (text-node) @p/path)) (str ~@strs)))

(defn set-style! [parent style-map]
  #?(:cljs (goog.style/setStyle parent (clj->js style-map))))

(defmacro style [style-map]
  `(set-style! parent ~style-map))

(defn set-properties! [e m] #?(:cljs (do (when-let [style (:style m)]
                                           (set-style! e style))
                                       (d/setProperties e (clj->js (dissoc m :style))))))
(defmacro props [m] `(set-properties! parent ~m))

(defn events* [e event-type & [xform init rf]]
  #?(:cljs (let [event-type (if (coll? event-type) (to-array event-type) event-type)]
             (cond->> (m/observe (fn [!] (e/listen e event-type !) #(e/unlisten e event-type !)))
               xform  (m/eduction xform)
               true   (m/reductions (or rf {}) init)))))

(defmacro events
  "Return a transduction of events as a discreet flow. See `clojure.core/transduce`.\n
   `event-type` can be a string like `\"click\"`, or a set of strings.
   
   ```clojure
   ;; count clicks
   (new (events \"click\" (map (constantly 1)) 0 +))

   ;; track focus state
   (new (events #{\"focus\" \"blur\"} 
        (comp (map event-type) (map {\"focus\" true, \"blur\" false})) 
        false))
   ```"
  ([event-type]               `(events* parent ~event-type nil    nil   nil))
  ([event-type xform]         `(events* parent ~event-type ~xform nil   nil))
  ([event-type xform init]    `(events* parent ~event-type ~xform ~init nil))
  ([event-type xform init rf] `(events* parent ~event-type ~xform ~init ~rf)))

(defn target-value [e] #?(:cljs (.. e -target -value)))
(defn event-type [e] #?(:cljs (.-type e)))

(defn focus-state [e]
  (events* e #{"focus" "blur"}
    (comp (map event-type)
      (map {"focus" true, "blur" false}))
    false))

#?(:cljs
   (deftype Clock [Hz
                   ^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn
     (-invoke [_]
              (if (zero? raf)
                (set! callback nil)
                (do (if (zero? Hz)
                      (.cancelAnimationFrame js/window raf)
                      (.clearTimeout js/window raf))
                  (terminator))))
     IDeref
     (-deref [_]
             (if (nil? callback)
               (terminator)
               (if (zero? Hz)
                 (set! raf (.requestAnimationFrame js/window callback))
                 (set! raf (.setTimeout js/window callback (/ 1000 Hz)))))
             (.now js/Date))))

(defn ^:no-doc clock* [Hz]
  #?(:cljs
     (fn [n t]
       (let [c (->Clock Hz 0 nil t)]
         (set! (.-callback c)
           (fn [_] (set! (.-raf c) 0) (n)))
         (n) c))))

;; The number of milliseconds elapsed since January 1, 1970, with custom frequency
(p/defn clock [Hz] (new (clock* Hz)))

(defmacro a [& body] `(element :a ~@body))
(defmacro abbr [& body] `(element :abbr ~@body))
(defmacro address [& body] `(element :address ~@body))
(defmacro area [& body] `(element :area ~@body))
(defmacro article [& body] `(element :article ~@body))
(defmacro aside [& body] `(element :aside ~@body))
(defmacro audio [& body] `(element :audio ~@body))
(defmacro b [& body] `(element :b ~@body))
(defmacro bdi [& body] `(element :bdi ~@body))
(defmacro bdo [& body] `(element :bdo ~@body))
(defmacro blockquote [& body] `(element :blockquote ~@body))
(defmacro br [& body] `(element :br ~@body))
(defmacro button [& body] `(element :button ~@body))
(defmacro canvas [& body] `(element :canvas ~@body))
(defmacro cite [& body] `(element :cite ~@body))
(defmacro code [& body] `(element :code ~@body))
(defmacro data [& body] `(element :data ~@body))
(defmacro datalist [& body] `(element :datalist ~@body))
(defmacro del [& body] `(element :del ~@body))
(defmacro details [& body] `(element :details ~@body))
(defmacro dfn [& body] `(element :dfn ~@body))
(defmacro dialog [& body] `(element :dialog ~@body))
(defmacro div [& body] `(element :div ~@body))
(defmacro dl [& body] `(element :dl ~@body))
(defmacro em [& body] `(element :em ~@body))
(defmacro embed [& body] `(element :embed ~@body))
(defmacro fieldset [& body] `(element :fieldset ~@body))
(defmacro figure [& body] `(element :figure ~@body))
(defmacro footer [& body] `(element :footer ~@body))
(defmacro form [& body] `(element :form ~@body))
(defmacro h1 [& body] `(element :h1 ~@body))
(defmacro h2 [& body] `(element :h2 ~@body))
(defmacro h3 [& body] `(element :h3 ~@body))
(defmacro h4 [& body] `(element :h4 ~@body))
(defmacro h5 [& body] `(element :h5 ~@body))
(defmacro h6 [& body] `(element :h6 ~@body))
(defmacro header [& body] `(element :header ~@body))
(defmacro hgroup [& body] `(element :hgroup ~@body))
(defmacro hr [& body] `(element :hr ~@body))
(defmacro i [& body] `(element :i ~@body))
(defmacro iframe [& body] `(element :iframe ~@body))
(defmacro img [& body] `(element :img ~@body))
(defmacro input [& body] `(element :input ~@body))
(defmacro ins [& body] `(element :ins ~@body))
(defmacro kbd [& body] `(element :kbd ~@body))
(defmacro label [& body] `(element :label ~@body))
(defmacro link [& body] `(element :link ~@body))
(defmacro main [& body] `(element :main ~@body))
#_(defmacro map [& body] `(element :map ~@body))
(defmacro mark [& body] `(element :mark ~@body))
(defmacro math [& body] `(element :math ~@body))
(defmacro menu [& body] `(element :menu ~@body))
(defmacro itemprop [& body] `(element :itemprop ~@body))
(defmacro meter [& body] `(element :meter ~@body))
(defmacro nav [& body] `(element :nav ~@body))
(defmacro noscript [& body] `(element :noscript ~@body))
(defmacro object [& body] `(element :object ~@body))
(defmacro ol [& body] `(element :ol ~@body))
(defmacro output [& body] `(element :output ~@body))
(defmacro p [& body] `(element :p ~@body))
(defmacro picture [& body] `(element :picture ~@body))
(defmacro pre [& body] `(element :pre ~@body))
(defmacro progress [& body] `(element :progress ~@body))
(defmacro q [& body] `(element :q ~@body))
(defmacro ruby [& body] `(element :ruby ~@body))
(defmacro s [& body] `(element :s ~@body))
(defmacro samp [& body] `(element :samp ~@body))
(defmacro script [& body] `(element :script ~@body))
(defmacro section [& body] `(element :section ~@body))
(defmacro select [& body] `(element :select ~@body))
(defmacro slot [& body] `(element :slot ~@body))
(defmacro small [& body] `(element :small ~@body))
(defmacro span [& body] `(element :span ~@body))
(defmacro strong [& body] `(element :strong ~@body))
(defmacro sub [& body] `(element :sub ~@body))
(defmacro sup [& body] `(element :sup ~@body))
(defmacro table [& body] `(element :table ~@body))
(defmacro td [& body] `(element :td ~@body))
(defmacro th [& body] `(element :th ~@body))
(defmacro tr [& body] `(element :tr ~@body))
(defmacro template [& body] `(element :template ~@body))
(defmacro textarea [& body] `(element :textarea ~@body))
(defmacro time [& body] `(element :time ~@body))
(defmacro u [& body] `(element :u ~@body))
(defmacro ul [& body] `(element :ul ~@body))
(defmacro var [& body] `(element :var ~@body))
(defmacro video [& body] `(element :video ~@body))
(defmacro wbr [& body] `(element :wbr ~@body))