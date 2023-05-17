(ns hyperfiddle.electric-dom2
  (:refer-clojure :exclude [time])
  (:require [contrib.missionary-contrib :as mx]
            #?(:cljs goog.dom)
            #?(:cljs goog.object)
            #?(:cljs goog.style)
            [hyperfiddle.electric :as e]
            [missionary.core :as m]
            [clojure.string :as str]
            [contrib.data :as data])
  (:import [hyperfiddle.electric Pending])
  #?(:cljs (:require-macros [hyperfiddle.electric-dom2 :refer [with]])))

(e/def node)
(def nil-subject (fn [!] (! nil) #()))
(e/def keepalive (new (m/observe nil-subject)))

(defn unsupported [& _]
  (throw (ex-info (str "Not available on this peer.") {})))

(def hook "See `with`"
  #?(:clj  unsupported
     :cljs (fn ([x] (some-> (.-parentNode x) (.removeChild x)))
             ([x y] (.insertBefore (.-parentNode x) x y))))) ; rotate siblings

(defmacro with
  "Attach `body` to a dom node, which will be moved in the DOM when body moves in the DAG.
  Given p/for semantics, `body` can only move sideways or be cancelled. If body is cancelled,
  the node will be unmounted. If body moves, the node will rotate with its siblings."
  [dom-node & body]
  `(binding [node ~dom-node]
     ; wrap body in a constant frame, so it can be moved as a block
     (new (e/hook hook node (e/fn [] keepalive ~@body))))) ; todo remove

#?(:cljs (defn by-id [id] (js/document.getElementById id)))

#?(:cljs
   (defn new-node [parent type]
     (let [el (case type
                :comment (.createComment js/document "")
                :text (goog.dom/createTextNode "")
                (goog.dom/createElement type))]
       (.appendChild parent el)
       el)))

(defmacro element [t & body]
  `(with (new-node node ~(name t))
     ; hack: speed up streamy unmount by removing from layout first
     ; it also feels faster visually
     (e/on-unmount #(set! (.. node -style -display) "none")) ; hack
     ~@body))

#?(:cljs (defn -googDomSetTextContentNoWarn [node str]
           ; Electric says :infer-warning Cannot infer target type in expression, fixme
           (goog.dom/setTextContent node str)))

(defmacro text [& strs]
  `(do (assert (not= (.-nodeType node) (.-TEXT_NODE node))
               "userland directed dom/text inside dom/text, which is illegal")
       ~@(map (fn [str]
                `(with (new-node node :text)
                   (-googDomSetTextContentNoWarn node ~str)))
           strs)))

(defmacro comment_ [& strs]
  (cons `do
    (map (fn [str] `(with (new-node node :comment)
                      (-googDomSetTextContentNoWarn node ~str)))
      strs)))

(def ^:const SVG-NS "http://www.w3.org/2000/svg")
(def ^:const XLINK-NS "http://www.w3.org/1999/xlink")

(def alias->ns {"svg" SVG-NS, "xlink" XLINK-NS})

(defn attr-alias [attr] (second (re-find #"^([^:]+):" (name attr))))

(defn resolve-attr-alias [attr]
  (let [attr (name attr)]
    (if-let [alias (attr-alias attr)]
      (let [attr (-> (str/replace-first attr alias "")
                   (str/replace-first #"^:" ""))]
        [(alias->ns alias) attr])
      [nil attr])))

#?(:cljs
   (defn set-attribute-ns
     ([node attr v]
      (let [[ns attr] (resolve-attr-alias attr)]
        (set-attribute-ns node ns attr v)))
     ([^js node ns attr v]
      (.setAttributeNS node ns attr v))))

#?(:cljs
   (defn set-property!
     ([node k v] (set-property! node (.-namespaceURI node) k v))
     ([node ns k v]
      (let [k (name k)
            v (clj->js v)]
        (if (and (nil? v) (.hasAttributeNS node nil k))
          (.removeAttributeNS node nil k)
          (case k
            "style" (goog.style/setStyle node v)
            "list"  (set-attribute-ns node nil k v) ; corner case, list (datalist) is setted by attribute and readonly as a prop.
            (if-let [k (goog.object/get goog.dom/DIRECT_ATTRIBUTE_MAP_ k)]
              (set-attribute-ns node k v)
              (if (= SVG-NS ns)
                (set-attribute-ns node k v)
                (if (goog.object/containsKey node k) ; is there an object property for this key?
                  (goog.object/set node k v)
                  (set-attribute-ns node k v))))))))))

#?(:cljs (defn unmount-prop [node k v]
           (m/observe (fn [!] (! nil) #(set-property! node k v)))))

(defmacro style [m]
  (if (map? m)
    `(do ~@(mapcat (fn [[k v]] [`(set-property! node "style" {~k ~v})
                                `(new (unmount-prop node "style" {~k nil}))]) m)
         nil) ; static keyset
    `(e/for-by first [sty# (vec ~m)]
       (set-property! node "style" {(key sty#) (val sty#)})
       (new (unmount-prop node {(key sty#) nil}))
       nil)))


(def LAST-PROPS
  "Due to a bug in both Webkit and FF, input type range's knob renders in the
  wrong place if value is set after `min` and `max`, and `max` is above 100.
  Other UI libs circumvent this issue by setting `value` last."
 [:value ::value])

(defn ordered-props "Sort props by key to ensure they are applied in a predefined order. See `LAST-PROPS`."
  [props-map]
  (let [props (apply dissoc props-map LAST-PROPS)]
    (concat (seq props) (seq (select-keys props-map LAST-PROPS)))))

(defn parse-class [xs]
  (cond (string? xs) (parse-class (str/split xs #"\s+"))
        (coll? xs)   (into [] (comp (filter string?) (remove str/blank?)) xs)
        :else        nil))

#?(:cljs
   (defn register-class! [^js node class]
     (let [refs (or (.-hyperfiddle_electric_dom2_class_refs node) {})]
       (.add (.-classList node) class)
       (set! (.-hyperfiddle_electric_dom2_class_refs node) (update refs class (fn [cnt] (inc (or cnt 0))))))))

#?(:cljs
   (defn unregister-class! [^js node class]
     (let [refs (or (.-hyperfiddle_electric_dom2_class_refs node) {})
           refs (if (= 1 (get refs class))
                  (do (.remove (.-classList node) class)
                      (dissoc refs class))
                  (update refs class dec))]
       (set! (.-hyperfiddle_electric_dom2_class_refs node) refs))))

(e/defn ClassList [node classes]
  (e/for [class classes]
    (new (m/relieve {} (m/observe (fn [!]
                                    (! nil)
                                    (register-class! node class)
                                    #(unregister-class! node class)))))))

;; TODO JS runtimes intern litteral strings, so call `name` on keywords at
;; macroexpension.
(defmacro props [m]
  (let [style? #{:style ::style}       ; TODO disambiguate
        class? #{:class ::class}]
    (if (map? m)
      `(do ~@(mapcat (fn [[k v]] (cond  ; static keyset
                                   (style? k) [`(style ~v)]
                                   (class? k) [`(new ClassList node (parse-class ~v))]
                                   :else      [`(set-property! node ~k ~v)
                                          `(new (unmount-prop node ~k nil))]))
               (ordered-props m))
           nil)
      `(e/for-by key [prop# (vec (ordered-props ~m))]
         (cond
           (~style? (key prop#)) (style (val prop#))
           (~class? (key prop#)) (new ClassList node (parse-class (val prop#)))
           :else                 (do (set-property! node (key prop#) (val prop#))
                                     (new (unmount-prop node (key prop#) nil))
                                     nil))))))

(defmacro on!
  "Call the `callback` clojure function on event.
   (on! \"click\" (fn [event] ...)) "
  ([event-name callback] `(on! node ~event-name ~callback))
  ([dom-node event-name callback] `(on! ~dom-node ~event-name ~callback nil))
  ([dom-node event-name callback options] 
   `(new (->> (e/listen> ~dom-node ~event-name ~callback ~options)
           (m/reductions {} nil)))))

(defmacro ^:deprecated ^:no-doc event "Deprecated, please use `on!`" [& args] `(on! ~@args))
(e/def ^:deprecated system-time-ms e/system-time-ms)
(e/def ^:deprecated system-time-secs e/system-time-secs)

(defmacro on
  "Run the given electric function on event.
  (on \"click\" (e/fn [event] ...))"
  ;; TODO add support of event options (see `event*`)
  ;(^:deprecated [typ]  `(new Event ~typ false)) ; use `on!` for local side effects
  ([typ F] `(on node ~typ ~F))
  ([node typ F] `(binding [node ~node]
                   (let [[state# v#] (e/for-event-pending-switch [e# (e/listen> node ~typ)] (new ~F e#))]
                     (case state#
                       (::e/init ::e/ok) v# ; could be `nil`, for backward compat we keep it
                       (::e/pending) (throw (Pending.))
                       (::e/failed)  (throw v#))))))

#?(:cljs (e/def visibility-state "'hidden' | 'visible'"
           (new (->> (e/listen> js/document "visibilitychange")
                  (m/reductions {} nil)
                  (m/latest #(.-visibilityState js/document))))))

(defmacro on-pending [pending-body & body] `(try (do ~@body) (catch Pending e# ~pending-body (throw e#))))

(e/defn Focused? "Returns whether this DOM `node` is focused."
  []
  (->> (mx/mix
         (e/listen> node "focus" (constantly true))
         (e/listen> node "blur" (constantly false)))
    (m/reductions {} (= node (.-activeElement js/document)))
    (m/relieve {})
    new))

#?(:cljs (defn set-val [node v] (set! (.-value node) (str v))))

(defmacro bind-value
  ([v]        `(bind-value ~v set-val))
  ([v setter] `(when-some [v# (when-not (new Focused?) ~v)]
                 (~setter node v#))))

(e/defn Hovered? "Returns whether this DOM `node` is hovered over."
  []
  (->> (mx/mix
         (e/listen> node "mouseenter" (constantly true))
         (e/listen> node "mouseleave" (constantly false)))
    (m/reductions {} false)
    (m/relieve {})
    new))

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
(defmacro dl "The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the <dt> element) and descriptions (provided by <dd> elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs)." [& body] `(element :dl ~@body))
(defmacro dt "The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a <dl> element. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element." [& body] `(element :dt ~@body))
(defmacro dd "The <dd> HTML element provides the description, definition, or value for the preceding term (<dt>) in a description list (<dl>)." [& body] `(element :dd ~@body))
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
(defmacro legend [& body] `(element :legend ~@body))
(defmacro li [& body] `(element :li ~@body))
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
(defmacro option [& body] `(element :option ~@body))
(defmacro optgroup [& body] `(element :optgroup ~@body))
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
(defmacro summary [& body] `(element :summary ~@body))
(defmacro sup [& body] `(element :sup ~@body))
(defmacro table [& body] `(element :table ~@body))
(defmacro tbody [& body] `(element :tbody ~@body))
(defmacro td [& body] `(element :td ~@body))
(defmacro th [& body] `(element :th ~@body))
(defmacro thead [& body] `(element :thead ~@body))
(defmacro tr [& body] `(element :tr ~@body))
(defmacro template [& body] `(element :template ~@body))
(defmacro textarea [& body] `(element :textarea ~@body))
(defmacro time [& body] `(element :time ~@body))
(defmacro u [& body] `(element :u ~@body))
(defmacro ul [& body] `(element :ul ~@body))
(defmacro var [& body] `(element :var ~@body))
(defmacro video [& body] `(element :video ~@body))
(defmacro wbr [& body] `(element :wbr ~@body))
