(ns hyperfiddle.photon-dom2
  (:refer-clojure :exclude [time])
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.object :as o])
            #?(:cljs [goog.style])
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.photon-dom2 #_#_:refer [ #_[a abbr address area article aside audio b bdi bdo blockquote br button canvas cite code data datalist del details dfn dialog div dl em embed fieldset figure footer form h1 h2 h3 h4 h5 h6 header hgroup hr i iframe img input ins kbd label link main map mark math menu #_meta itemprop meter nav noscript object ol output p picture pre progress q ruby s samp script section select slot small span strong sub sup table template textarea #_time u ul var video wbr]]]))
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

(defn mount [node child path]
  #?(:cljs (m/observe
             (fn [!]
               (o/set child "--photon-path" path)
               (.insertBefore node child
                 ;; TODO sublinear anchor search. skip list ?
                 (loop [anchor (.-firstChild node)]
                   (when-not (nil? anchor)
                     (if (before? (o/get anchor "--photon-path") path)
                       (recur (.-nextSibling anchor)) anchor))))
               (! child) #(prn 'unmount (d/removeNode child))))))

(defn collect [items]
  #?(:cljs
     (into {} (mapcat (fn [x]
                        (cond
                          (instance? js/Element x) (seq (aget x "--photon-dom-signals"))
                          (map? x) (seq x))))
       items)))

(defn signals [el] (aget el "--photon-dom-signals"))

(defn dom-element [tag] #?(:cljs (d/createElement tag)))

(p/def node)

(defmacro element [tag & body]
  `(let [el# (dom-element ~(name tag))]
     (binding [node el#]
       (p/forget (let [collected# (collect (p/for [x# ~(cons `list body)]
                                             (cond
                                               (instance? js/Element x#) (when-let [flow# (aget x# "--photon-dom-flow")]
                                                                           (signals (new flow# el#)))

                                               :else x#)))]
                   (do collected#
                     (aset el# "--photon-dom-signals" (p/fn [] collected#)))))
       (doto el#
         (aset "--photon-dom-flow" (p/fn [p] (new (mount p el# @p/path))))))))

(defmacro root [node el] `(new (aget ~el "--photon-dom-flow") ~node))

(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(defn text-node [] #?(:cljs (d/createTextNode "")))
(defn set-text-content! [e t] #?(:cljs (d/setTextContent e (str t))))
(defmacro text [& strs] `(set-text-content! (new (mount node (text-node) @p/path)) (str ~@strs)))

(defn set-properties! [e m] #?(:cljs (d/setProperties e (clj->js m))))
(defmacro props [m] `(set-properties! node ~m))

(defn events* [e t]
  #?(:cljs (let [t (if (vector? t) (to-array t) t)]
             (m/observe (fn [!] (e/listen e t !) #(e/unlisten e t !))))))

(defmacro events [t] `(events* node ~t))

(defn target-value [e] #?(:cljs (.. e -target -value)))

(defn observe* "experimental" [e attr]
  #?(:cljs (m/observe (fn [!]
                        (let [observer (new js/MutationObserver
                                         (fn [mutation-list observer]
                                           (doseq [mutation (array-seq mutation-list)]
                                             (when (= attr (.-attributeName mutation))
                                               (! mutation)))))]
                          (.observe observer e #js{:attributes true})
                          (! (.getAttribute e attr))
                          #(.disconnect observer))))))

(defmacro observe "experimental" [attr] `(new (m/relieve {} (observe* node ~attr))))

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