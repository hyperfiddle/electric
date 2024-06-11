(ns hyperfiddle.electric-dom3-props
  (:refer-clojure :exclude [class?])
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.electric-dom3 :as-alias dom]
   [hyperfiddle.electric-css3 :as css]
   [hyperfiddle.rcf :refer [tests]]
   [missionary.core :as m]
   #?(:cljs [goog.object])
   )
  #?(:cljs (:require-macros [hyperfiddle.electric-dom3-props])))

;;;;;;;;;;;;;;;;
;; Attributes ;;
;;;;;;;;;;;;;;;;

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
   (defn has-attribute-ns?
     ([node attr]
      (let [[ns attr] (resolve-attr-alias attr)]
        (has-attribute-ns? node ns attr)))
     ([^js node ns attr]
      (.hasAttributeNS node ns attr))))

#?(:cljs
   (defn remove-attribute-ns
     ([node attr]
      (let [[ns attr] (resolve-attr-alias attr)]
        (remove-attribute-ns node ns attr)))
     ([^js node ns attr]
      (.removeAttributeNS node ns attr))))

(def DIRECT-ATTRIBUTE-MAP
  "Map of attributes that should be set using element.setAttribute(key, val)
  instead of element[key] = val. Used by goog.dom.setProperties.
  Used by set-property!.
  From https://github.com/google/closure-library/blob/7818ff7dc0b53555a7fb3c3427e6761e88bde3a2/closure/goog/dom/dom.js#L563"
  {"cellpadding" "cellPadding"
   "cellspacing" "cellSpacing"
   "colspan"     "colSpan"
   "frameborder" "frameBorder"
   "height"      "height"
   "maxlength"   "maxLength"
   "nonce"       "nonce"
   "role"        "role"
   "rowspan"     "rowSpan"
   "type"        "type"
   "usemap"      "useMap"
   "valign"      "vAlign"
   "width"       "width"})

(def DIRECT-ATTRIBUTE (set (keys DIRECT-ATTRIBUTE-MAP)))

#?(:cljs
   (defn set-property!
     "Set "
     ([node k v] (set-property! node (.-namespaceURI node) k v))
     ([node ns k v]
      (let [k (name k)
            v (clj->js v)]
        (if (and (nil? v) (has-attribute-ns? node k))
          (remove-attribute-ns node k)
          (case k
            "list" (set-attribute-ns node nil k v) ; corner case, list (datalist) is setted by attribute and readonly as a prop.
            (if (or (= SVG-NS ns)
                  (DIRECT-ATTRIBUTE k))
              (set-attribute-ns node k v)
              (if (goog.object/containsKey node k) ; is there an object property for this key?
                (goog.object/set node k v)
                (set-attribute-ns node k v)))))))))

#?(:cljs
   (defn watch-attributes [node html-attributes]
     (let [html-attributes (set html-attributes)]
       (m/relieve {}
         (m/reductions into (into {} (map (juxt identity #(.getAttribute node %)) html-attributes))
           (m/observe
             (fn [!]
               (let [observer (js/MutationObserver. (fn [mutation-list _observer]
                                                     (! (filter (comp html-attributes first)
                                                          (map (fn [mutation]
                                                                 (let [attrName (.-attributeName mutation)]
                                                                   [attrName (.getAttribute node attrName)]))
                                                            mutation-list)))))]
                 (.observe observer node #js{:attributes true})
                 #(.disconnect observer)))))))))

(e/defn Attributes
  "
Take a collection of `attribute-names` and watch for attribute changes in
`node`. Return a map of {\"attribute-name\" attribute-value, ...}. Only DOM
attributes are watchable, not object properties. For instance, to watch an
input's value, use an event listener.
"
  [node attribute-names]
  (e/client (e/input (watch-attributes node attribute-names))))

(e/defn Attribute
  "
Watch an `attribute`'s value for a given DOM `node`. Only DOM attributes are
watchable, not object properties. For instance, to watch an input's value, use
an event listener. Use `Attributes` to watch multiple attributes at once.
If `value` is provided, reactively sets the correponding `node`'s `attribute`or
property to `value`.
On unmount:
  - if `attribute` defines an actual DOM attribute, remove `attribute` from `node`.
  - if `attribute` defines an object property, sets it to nil.
"
  ([node attribute] (get ($ Attributes node #{attribute}) (name attribute)))
  ([node attribute value]
   (e/client
     (set-property! node attribute value)
     (e/on-unmount #(set-property! node attribute nil)))))

;;;;;;;;;;;;;
;; Classes ;;
;;;;;;;;;;;;;

(defn parse-class [xs]
  (cond (or (string? xs) (keyword? xs) (symbol? xs)) (re-seq #"[^\s]+" (name xs))
        (or (vector? xs) (seq? xs) (list? xs) (set? xs)) (into [] (comp (mapcat parse-class) (distinct)) xs)
        (nil? xs) nil
        :else (throw (ex-info "don't know how to parse into a classlist" {:data xs}))))

(tests
  (parse-class "a") := ["a"]
  (parse-class :a) := ["a"]
  (parse-class 'a/b) := ["b"]
  (parse-class "a b") := ["a" "b"]
  (parse-class ["a"]) := ["a"]
  (parse-class ["a" "b" "a"]) := ["a" "b"]
  (parse-class ["a" "b"]) := ["a" "b"]
  (parse-class ["a b" "c"]) := ["a" "b" "c"]
  (parse-class [["a b"] '("c d") #{#{"e"} "f"}]) := ["a" "b" "c" "d" "e" "f"]
  (parse-class nil) := nil
  (parse-class "") := nil
  (parse-class " a") := ["a"]
  (try (parse-class 42) (throw (ex-info "" {}))
       (catch ExceptionInfo ex (ex-data ex) := {:data 42})))

#?(:cljs
   (defn build-class-signal [node clazz]
     (m/signal (m/observe (fn [!]
                            (! nil)
                            (.add (.-classList node) clazz)
                            #(.remove (.-classList node) clazz))))))

#?(:cljs
   (defn get-class-signal [node clazz]
     (let [k (js/Symbol.for (str "hyperfiddle.dom3.class-signal-" clazz))]
       (or (aget node k) (aset node k (build-class-signal node clazz))))))

(e/defn Clazz [node clazz] (e/client (e/input (get-class-signal node clazz))))

;; TODO find a better name for MapCSeq
;; TODO move MapCSeq to another ns
;; how to run an e/fn over a clojure sequence
(e/defn MapCSeq [Fn cseq] ; FIXME find the right name
  (e/cursor [[_ v] (e/diff-by first (map-indexed vector cseq))] ($ Fn v)))

;; Alternative style
#_(defmacro for-cseq [[b cseq] & body] `(e/cursor [[i# ~b] (e/diff-by first (map-indexed vector ~cseq))] ~@body))
#_(for-cseq [x xs] ($ Foo x))

(e/defn ; ^:hyperfiddle.electric.impl.lang-de2/print-clj-source
  Partial ;; TODO move to electric core
  ;; Impl is a mechanical 1 to 1 transaltion of clojure partial.
  ;; generated code is quite large but redundant, so it gzip to 903 bytes.
  ;; we could prune this impl to reduce code size (no clear benefit)
  ;; We keep this impl as a proof that our lambda abstraction is correct
  ;; We might optimise it later if there are perf issues.
  "Takes an Electric function F and fewer than the normal arguments to F, and
  returns a e/fn that takes a variable number of additional args. When
  called, the returned function calls F with args + additional args."
  ([F] F)
  ([F arg1]
   (e/fn
     ([] ($ F arg1))
     ([x] ($ F arg1 x))
     ([x y] ($ F arg1 x y))
     ([x y z] ($ F arg1 x y z))
     ([x y z & args] (e/apply F arg1 x y z args))))
  ([F arg1 arg2]
   (e/fn
     ([] ($ F arg1 arg2))
     ([x] ($ F arg1 arg2 x))
     ([x y] ($ F arg1 arg2 x y))
     ([x y z] ($ F arg1 arg2 x y z))
     ([x y z & args] (e/apply F arg1 arg2 x y z args))))
  ([F arg1 arg2 arg3]
   (e/fn
     ([] ($ F arg1 arg2 arg3))
     ([x] ($ F arg1 arg2 arg3 x))
     ([x y] ($ F arg1 arg2 arg3 x y))
     ([x y z] ($ F arg1 arg2 arg3 x y z))
     ([x y z & args] (e/apply F arg1 arg2 arg3 x y z args))))
  ([F arg1 arg2 arg3 & more]
   (e/fn [& args] (e/apply F arg1 arg2 arg3 (concat more args)))))

(e/defn ClassList [node classes]
  (e/client
    ($ MapCSeq ($ Partial Clazz node) (parse-class classes))))

;;;;;;;;;;;;;;;;;;;
;; Inline Styles ;;
;;;;;;;;;;;;;;;;;;;

(e/defn Styles [node kvs] ; TODO move to electric-css, blocked on MapCSeq
  (e/client
    ($ MapCSeq (e/fn [[property value]] ($ css/Style node property value)) kvs)
    kvs))

;;;;;;;;;;;;;;;;;;;
;; Generic Props ;;
;;;;;;;;;;;;;;;;;;;

(def LAST-PROPS
  "Due to a bug in both Webkit and FF, input type range's knob renders in the
  wrong place if value is set after `min` and `max`, and `max` is above 100.
  Other UI libs circumvent this issue by setting `value` last."
  [:value ::value])

(defn ordered-props "Sort props by key to ensure they are applied in a predefined order. See `LAST-PROPS`."
  [props-map]
  (let [props (apply dissoc props-map LAST-PROPS)]
    (concat (seq props) (seq (select-keys props-map LAST-PROPS)))))

(def ^:private style? #{:style ::style}) ; Unnamespaced is allowed for simpler code examples
(def ^:private class? #{:class ::class}) ; But we recommend the namespaced variant

(e/defn Property
  "Set a DOM `node`'s attribute or property to `value`"
  [node name value]
  (e/client
    (cond
      (style? name) ($ Styles node value)
      (class? name) ($ ClassList node value)
      :else         ($ Attribute node name value))))

(e/defn Properties ; NOTE Leo: this is an anti pattern, we already have e/amb or e/diff-by, no need for an implicit diff.
  "Take a map of attribute or property name to value and sets each onto `node`. Return nil."
  [node kvs]
  (e/client
    ($ MapCSeq (e/fn [[name value]] ($ Property node name value)) (ordered-props kvs))))

(defmacro props
  "
Take a map of HTML attributes to values and reactively sets each of them onto
a given DOM `node`. Default `node` is the one in scope.

Example:
```clojure
  (dom/div (dom/props {:id \"my-div\", :class [\"foo\"], :style {:background-color :red}}))
```

- A value of `nil` will remove the attribute.
- Attribute names are case-insensitive, like in HTML.
- Attribute inherits the `node`'s namespace (e.g. SVG vs HTML attributes)
- `:class`, setting the CSS class, can be a string or a collection of strings.
- `:style`, setting inline CSS styles, supports setting CSS variables (e.g. {:--my-color :red})
  - for more complex styles (e.g. pseudo-classes, pseudo-elements, keyframes) use Electric-CSS.

Note `props` will decide if an attribute is set as an HTML attribute or as a DOM
object property. For instance:
- An input's `:value` is set through the `node.value` property.
- `:list` (input's datalist) can only be set by attribute, as the corresponding property is readonly.
- `:class` doesn't set the \"class\" HTML attribute, but efficiently manipulates the node's `.classList` property.
- `:style` doesn't set the \"style\" HTML attribute, but efficiently manipulates the CSSStyleDeclaration object under the `.style` property.
- etc.
  "
  ([m] `(props dom/node ~m))
  ([node m]
   (if (map? m)
     `(do ~@(map (fn [[k v]] (cond  ; static keyset + saves on a conditional
                               (style? k) `($ Styles ~node ~v)
                               (class? k) `($ ClassList ~node ~v)
                               :else      `($ Property ~node ~k ~v)))
              (ordered-props m))
          nil)
     `($ Properties ~node ~m))))
