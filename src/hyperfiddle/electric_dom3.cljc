;; * DONE Replace dom3 by dom3_efn
;;   G: diffed both files, LGTM
;; * DONE move event handling to separate ns
;;   So we can think clearly
;;   We can always merge back later
;; * TODO Implement dom/text
;; * TODO Implement dom/comment
;; * TODO Implement dom/div
;; * TODO Implement dom/div nesting
;; * TODO Implement setting attributes
;; * TODO Implement setting class
;; * TODO Implement setting inline style
;; * TODO Implement event handling

(ns hyperfiddle.electric-dom3
  (:refer-clojure :exclude [time class?])
  (:require
   [clojure.string :as str]
   [contrib.assert :as ca]
   [contrib.debug]
   #?(:cljs goog.dom)
   #?(:cljs goog.object)
   #?(:cljs goog.style)
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.electric.impl.lang-de2 :as lang]
   [hyperfiddle.incseq :as i]
   [hyperfiddle.rcf :as rcf :refer [tests]]
   [missionary.core :as m])
  #?(:clj (:import [clojure.lang ExceptionInfo]))
  #?(:cljs (:require-macros [hyperfiddle.electric-dom3])))

(def node)

#?(:cljs (defn node? [v] (when v (= 1 (.-nodeType v)))))

#?(:cljs (defn appending> [elem parent]
           (ca/is parent node? "DOM node parent is not an HTML Node. Maybe dom/node is unbound?" {:parent parent})
           (m/observe (fn [!] (.appendChild parent elem) (! elem) #(.remove elem)))))

(e/defn With [elem Body] (binding [node (e/input (appending> elem node))] node ($ Body)))

#?(:cljs (defn -googDomSetTextContentNoWarn [node str]
           ;; Electric says :infer-warning Cannot infer target type in expression, fixme
           (goog.dom/setTextContent node str)))

#?(:cljs (defn ->text-node [] (goog.dom/createTextNode "")))

#?(:cljs (defn text-node? [nd] (= (.-nodeType nd) (.-TEXT_NODE nd))))
#?(:cljs (defn ensure-not-in-text-node! [nd] (ca/is nd (complement text-node?) "Cannot nest dom/text or text nodes in other text nodes")))

(e/defn Text [str] ($ With (->text-node) (e/fn [] (-googDomSetTextContentNoWarn node str))))

(defmacro text [& strs] `(do (ensure-not-in-text-node! node) ~@(for [s strs] `($ Text ~s))))

(e/defn Comment [str]
  ($ With (.createComment js/document "") (e/fn [] (-googDomSetTextContentNoWarn node str))))

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

#?(:cljs (defn- css-var? [k] (str/starts-with? k "--")))
#?(:cljs (defn set-style> [node k v]
           (let [k (clj->js k), v (clj->js v)
                 setter (if (css-var? k) #(.setProperty (.-style node) k %) #(goog.style/setStyle_ node % k))]
             (m/observe (fn [!] (setter v) (! v) #(setter nil))))))

#?(:cljs (defn set-property>
           ([node k v] (set-property> node (.-namespaceURI node) k v))
           ([node ns k v]
            (let [k (name k), v (clj->js v)
                  setter (case k
                           "list" ; corner case, list (datalist) is set by attribute and readonly as a prop.
                           #(set-attribute-ns node nil k %)
                           (if (or (= SVG-NS ns) (some? (goog.object/get goog.dom/DIRECT_ATTRIBUTE_MAP_ k)))
                             #(set-attribute-ns node k %)
                             (if (goog.object/containsKey node k) ; is there an object property for this key?
                               #(goog.object/set node k %)
                               #(set-attribute-ns node k %))))]
              (m/observe (fn [!] (setter v) (! v) #(setter nil)))))))

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

#?(:cljs
   (defn- manage-class> [node class]
     (m/relieve {}
       (m/observe (fn [!]
                    (! nil)
                    (register-class! node class)
                    #(unregister-class! node class))))))

(e/defn ClassList [node classes]
  (e/client
    (e/input (manage-class> node (e/diff-by identity (parse-class classes))))))

(e/defn Style [node k v] (e/client (e/input (set-style> node k v))))

(e/defn Styles [node kvs]
  (e/client
    (e/cursor [[k v] (e/diff-by first kvs)]
      ($ Style node k v))))

(defmacro style [m]
  (if (map? m)                          ; map = static keyset, no need to diff, cheaper
    `(do ~@(map (fn [[k v]] `($ Style node ~k ~v)) m))
    `($ Styles node ~m)))

(e/defn Attribute [node k v] (e/client (e/input (set-property> node k v))))

(def ^:private style? #{:style ::style})       ; TODO disambiguate
(def ^:private class? #{:class ::class})

(e/defn Property [node k v]
  (e/client
    (cond (style? k) ($ Styles node v)
          (class? k) ($ ClassList node v)
          :else      ($ Attribute node k v))))

(e/defn Properties [node kvs]
  (e/client
    (let [[k v] (e/diff-by key (ordered-props kvs))]
      ($ Property node k v))))

(defmacro props [m]
  (if (map? m)                          ; map = static keyset, no need to diff, cheaper
    `(do ~@(eduction (map (fn [[k v]] `($ Property node ~k ~v)))
             (ordered-props m)))
    `(do (let [[k# v#] (e/diff-by key (ordered-props ~m))]
           ($ Property node k# v#))
         nil)))

#?(:cljs (defn ->elem [t] (goog.dom/createElement t)))

(defmacro a {:style/indent 0} [& body] `($ With (->elem "a") (e/fn [] ~@body)))
(defmacro abbr {:style/indent 0} [& body] `($ With (->elem "abbr") (e/fn [] ~@body)))
(defmacro address {:style/indent 0} [& body] `($ With (->elem "address") (e/fn [] ~@body)))
(defmacro area {:style/indent 0} [& body] `($ With (->elem "area") (e/fn [] ~@body)))
(defmacro article {:style/indent 0} [& body] `($ With (->elem "article") (e/fn [] ~@body)))
(defmacro aside {:style/indent 0} [& body] `($ With (->elem "aside") (e/fn [] ~@body)))
(defmacro audio {:style/indent 0} [& body] `($ With (->elem "audio") (e/fn [] ~@body)))
(defmacro b {:style/indent 0} [& body] `($ With (->elem "b") (e/fn [] ~@body)))
(defmacro bdi {:style/indent 0} [& body] `($ With (->elem "bdi") (e/fn [] ~@body)))
(defmacro bdo {:style/indent 0} [& body] `($ With (->elem "bdo") (e/fn [] ~@body)))
(defmacro blockquote {:style/indent 0} [& body] `($ With (->elem "blockquote") (e/fn [] ~@body)))
(defmacro br {:style/indent 0} [& body] `($ With (->elem "br") (e/fn [] ~@body)))
(defmacro button {:style/indent 0} [& body] `($ With (->elem "button") (e/fn [] ~@body)))
(defmacro canvas {:style/indent 0} [& body] `($ With (->elem "canvas") (e/fn [] ~@body)))
(defmacro cite {:style/indent 0} [& body] `($ With (->elem "cite") (e/fn [] ~@body)))
(defmacro code {:style/indent 0} [& body] `($ With (->elem "code") (e/fn [] ~@body)))
(defmacro colgroup {:style/indent 0} [& body] `($ With (->elem "colgroup") (e/fn [] ~@body)))
(defmacro col {:style/indent 0} [& body] `($ With (->elem "col") (e/fn [] ~@body)))
(defmacro data {:style/indent 0} [& body] `($ With (->elem "data") (e/fn [] ~@body)))
(defmacro datalist {:style/indent 0} [& body] `($ With (->elem "datalist") (e/fn [] ~@body)))
(defmacro del {:style/indent 0} [& body] `($ With (->elem "del") (e/fn [] ~@body)))
(defmacro details {:style/indent 0} [& body] `($ With (->elem "details") (e/fn [] ~@body)))
(defmacro dfn {:style/indent 0} [& body] `($ With (->elem "dfn") (e/fn [] ~@body)))
(defmacro dialog {:style/indent 0} [& body] `($ With (->elem "dialog") (e/fn [] ~@body)))
(defmacro div {:style/indent 0} [& body] `($ With (->elem "div") (e/fn [] ~@body)))
(defmacro dl "The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the <dt> element) and descriptions (provided by <dd> elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs)." {:style/indent 0} [& body] `($ With (->elem "dl") (e/fn [] ~@body)))
(defmacro dt "The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a <dl> element. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element." {:style/indent 0} [& body] `($ With (->elem "dt") (e/fn [] ~@body)))
(defmacro dd "The <dd> HTML element provides the description, definition, or value for the preceding term (<dt>) in a description list (<dl>)." {:style/indent 0} [& body] `($ With (->elem "dd") (e/fn [] ~@body)))
(defmacro em {:style/indent 0} [& body] `($ With (->elem "em") (e/fn [] ~@body)))
(defmacro embed {:style/indent 0} [& body] `($ With (->elem "embed") (e/fn [] ~@body)))
(defmacro fieldset {:style/indent 0} [& body] `($ With (->elem "fieldset") (e/fn [] ~@body)))
(defmacro figure {:style/indent 0} [& body] `($ With (->elem "figure") (e/fn [] ~@body)))
(defmacro footer {:style/indent 0} [& body] `($ With (->elem "footer") (e/fn [] ~@body)))
(defmacro form {:style/indent 0} [& body] `($ With (->elem "form") (e/fn [] ~@body)))
(defmacro h1 {:style/indent 0} [& body] `($ With (->elem "h1") (e/fn [] ~@body)))
(defmacro h2 {:style/indent 0} [& body] `($ With (->elem "h2") (e/fn [] ~@body)))
(defmacro h3 {:style/indent 0} [& body] `($ With (->elem "h3") (e/fn [] ~@body)))
(defmacro h4 {:style/indent 0} [& body] `($ With (->elem "h4") (e/fn [] ~@body)))
(defmacro h5 {:style/indent 0} [& body] `($ With (->elem "h5") (e/fn [] ~@body)))
(defmacro h6 {:style/indent 0} [& body] `($ With (->elem "h6") (e/fn [] ~@body)))
(defmacro header {:style/indent 0} [& body] `($ With (->elem "header") (e/fn [] ~@body)))
(defmacro hgroup {:style/indent 0} [& body] `($ With (->elem "hgroup") (e/fn [] ~@body)))
(defmacro hr {:style/indent 0} [& body] `($ With (->elem "hr") (e/fn [] ~@body)))
(defmacro i {:style/indent 0} [& body] `($ With (->elem "i") (e/fn [] ~@body)))
(defmacro iframe {:style/indent 0} [& body] `($ With (->elem "iframe") (e/fn [] ~@body)))
(defmacro img {:style/indent 0} [& body] `($ With (->elem "img") (e/fn [] ~@body)))
(defmacro input {:style/indent 0} [& body] `($ With (->elem "input") (e/fn [] ~@body)))
(defmacro ins {:style/indent 0} [& body] `($ With (->elem "ins") (e/fn [] ~@body)))
(defmacro kbd {:style/indent 0} [& body] `($ With (->elem "kbd") (e/fn [] ~@body)))
(defmacro label {:style/indent 0} [& body] `($ With (->elem "label") (e/fn [] ~@body)))
(defmacro legend {:style/indent 0} [& body] `($ With (->elem "legend") (e/fn [] ~@body)))
(defmacro li {:style/indent 0} [& body] `($ With (->elem "li") (e/fn [] ~@body)))
(defmacro link {:style/indent 0} [& body] `($ With (->elem "link") (e/fn [] ~@body)))
(defmacro main {:style/indent 0} [& body] `($ With (->elem "main") (e/fn [] ~@body)))
#_(defmacro map {:style/indent 0} [& body] `($ With (->elem "map") (e/fn [] ~@body)))
(defmacro mark {:style/indent 0} [& body] `($ With (->elem "mark") (e/fn [] ~@body)))
(defmacro math {:style/indent 0} [& body] `($ With (->elem "math") (e/fn [] ~@body)))
(defmacro menu {:style/indent 0} [& body] `($ With (->elem "menu") (e/fn [] ~@body)))
(defmacro itemprop {:style/indent 0} [& body] `($ With (->elem "itemprop") (e/fn [] ~@body)))
(defmacro meter {:style/indent 0} [& body] `($ With (->elem "meter") (e/fn [] ~@body)))
(defmacro nav {:style/indent 0} [& body] `($ With (->elem "nav") (e/fn [] ~@body)))
(defmacro noscript {:style/indent 0} [& body] `($ With (->elem "noscript") (e/fn [] ~@body)))
(defmacro object {:style/indent 0} [& body] `($ With (->elem "object") (e/fn [] ~@body)))
(defmacro ol {:style/indent 0} [& body] `($ With (->elem "ol") (e/fn [] ~@body)))
(defmacro option {:style/indent 0} [& body] `($ With (->elem "option") (e/fn [] ~@body)))
(defmacro optgroup {:style/indent 0} [& body] `($ With (->elem "optgroup") (e/fn [] ~@body)))
(defmacro output {:style/indent 0} [& body] `($ With (->elem "output") (e/fn [] ~@body)))
(defmacro p {:style/indent 0} [& body] `($ With (->elem "p") (e/fn [] ~@body)))
(defmacro picture {:style/indent 0} [& body] `($ With (->elem "picture") (e/fn [] ~@body)))
(defmacro pre {:style/indent 0} [& body] `($ With (->elem "pre") (e/fn [] ~@body)))
(defmacro progress {:style/indent 0} [& body] `($ With (->elem "progress") (e/fn [] ~@body)))
(defmacro q {:style/indent 0} [& body] `($ With (->elem "q") (e/fn [] ~@body)))
(defmacro ruby {:style/indent 0} [& body] `($ With (->elem "ruby") (e/fn [] ~@body)))
(defmacro s {:style/indent 0} [& body] `($ With (->elem "s") (e/fn [] ~@body)))
(defmacro samp {:style/indent 0} [& body] `($ With (->elem "samp") (e/fn [] ~@body)))
(defmacro script {:style/indent 0} [& body] `($ With (->elem "script") (e/fn [] ~@body)))
(defmacro section {:style/indent 0} [& body] `($ With (->elem "section") (e/fn [] ~@body)))
(defmacro select {:style/indent 0} [& body] `($ With (->elem "select") (e/fn [] ~@body)))
(defmacro slot {:style/indent 0} [& body] `($ With (->elem "slot") (e/fn [] ~@body)))
(defmacro small {:style/indent 0} [& body] `($ With (->elem "small") (e/fn [] ~@body)))
(defmacro span {:style/indent 0} [& body] `($ With (->elem "span") (e/fn [] ~@body)))
(defmacro strong {:style/indent 0} [& body] `($ With (->elem "strong") (e/fn [] ~@body)))
(defmacro sub {:style/indent 0} [& body] `($ With (->elem "sub") (e/fn [] ~@body)))
(defmacro summary {:style/indent 0} [& body] `($ With (->elem "summary") (e/fn [] ~@body)))
(defmacro sup {:style/indent 0} [& body] `($ With (->elem "sup") (e/fn [] ~@body)))
(defmacro table {:style/indent 0} [& body] `($ With (->elem "table") (e/fn [] ~@body)))
(defmacro tbody {:style/indent 0} [& body] `($ With (->elem "tbody") (e/fn [] ~@body)))
(defmacro td {:style/indent 0} [& body] `($ With (->elem "td") (e/fn [] ~@body)))
(defmacro th {:style/indent 0} [& body] `($ With (->elem "th") (e/fn [] ~@body)))
(defmacro thead {:style/indent 0} [& body] `($ With (->elem "thead") (e/fn [] ~@body)))
(defmacro tr {:style/indent 0} [& body] `($ With (->elem "tr") (e/fn [] ~@body)))
(defmacro template {:style/indent 0} [& body] `($ With (->elem "template") (e/fn [] ~@body)))
(defmacro textarea {:style/indent 0} [& body] `($ With (->elem "textarea") (e/fn [] ~@body)))
(defmacro time {:style/indent 0} [& body] `($ With (->elem "time") (e/fn [] ~@body)))
(defmacro u {:style/indent 0} [& body] `($ With (->elem "u") (e/fn [] ~@body)))
(defmacro ul {:style/indent 0} [& body] `($ With (->elem "ul") (e/fn [] ~@body)))
(defmacro var {:style/indent 0} [& body] `($ With (->elem "var") (e/fn [] ~@body)))
(defmacro video {:style/indent 0} [& body] `($ With (->elem "video") (e/fn [] ~@body)))
(defmacro wbr {:style/indent 0} [& body] `($ With (->elem "wbr") (e/fn [] ~@body)))

