(ns hyperfiddle.electric-dom3
  (:refer-clojure :exclude [time class?])
  (:require
   [clojure.string :as str]
   [contrib.assert :as ca]
   #?(:cljs goog.dom)
   #?(:cljs goog.object)
   #?(:cljs goog.style)
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.rcf :as rcf :refer [tests]]
   [missionary.core :as m])
  #?(:clj (:import [clojure.lang ExceptionInfo]))
  #?(:cljs (:require-macros [hyperfiddle.electric-dom3])))

(def node)

;; used as a speed hack during unmount
#?(:cljs (defn ^:no-doc hide [node] (set! (.. node -style -display) "none")))

#?(:cljs (defn appending> [elem parent]
           (ca/is parent some? "DOM node parent cannot be nil. Maybe dom/node is unbound?")
           (m/observe (fn [!] (.appendChild parent elem) (! elem) #(.remove elem)))))

;; TODO this should be a simple `binding` but the observer doesn't unmount that way
(defmacro with [elem & body] `(binding [node (e/input (appending> ~elem node))] ~@body node))

#?(:cljs (defn -googDomSetTextContentNoWarn [node str]
           ;; Electric says :infer-warning Cannot infer target type in expression, fixme
           (goog.dom/setTextContent node str)))

#?(:cljs (defn ->text-node [] (goog.dom/createTextNode "")))

#?(:cljs (defn text-node? [nd] (= (.-nodeType nd) (.-TEXT_NODE nd))))
#?(:cljs (defn ensure-not-in-text-node! [nd] (ca/is nd (complement text-node?) "Cannot nest dom/text or text nodes in other text nodes")))

(defmacro text [& strs]
  `(do #_(ensure-not-in-text-node! node) ; TODO adding this breaks unmounting
       ~@(eduction (map (fn [str]
                          `(with (->text-node)
                             (-googDomSetTextContentNoWarn node ~str))))
           strs)))

(defmacro comment_ [& strs]
  `(do ~@(eduction (map (fn [str]
                          `(with (.createComment js/document "")
                             (-googDomSetTextContentNoWarn node ~str))))
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
    (let [[k v] (e/diff-by first kvs)]
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

#?(:cljs
   (defn listen> [nd typ f opts]
     (m/observe (fn [!]
                  (! nil)
                  (let [! (comp ! f), opts (clj->js opts)]
                    (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts)))))

(defmacro listen
  ([typ] `(listen ~typ identity))
  ([typ f] `(listen ~typ ~f node))
  ([nd typ f] `(listen ~nd ~typ ~f nil))
  ([nd typ f opts] `(listen> ~nd ~typ ~f ~opts)))

#?(:cljs (defn ->elem [t] (goog.dom/createElement t)))
(defmacro element {:style/indent 1} [t & body] `(with (->elem ~(name t)) ~@body))

(defmacro a {:style/indent 0} [& body] `(element :a ~@body))
(defmacro abbr {:style/indent 0} [& body] `(element :abbr ~@body))
(defmacro address {:style/indent 0} [& body] `(element :address ~@body))
(defmacro area {:style/indent 0} [& body] `(element :area ~@body))
(defmacro article {:style/indent 0} [& body] `(element :article ~@body))
(defmacro aside {:style/indent 0} [& body] `(element :aside ~@body))
(defmacro audio {:style/indent 0} [& body] `(element :audio ~@body))
(defmacro b {:style/indent 0} [& body] `(element :b ~@body))
(defmacro bdi {:style/indent 0} [& body] `(element :bdi ~@body))
(defmacro bdo {:style/indent 0} [& body] `(element :bdo ~@body))
(defmacro blockquote {:style/indent 0} [& body] `(element :blockquote ~@body))
(defmacro br {:style/indent 0} [& body] `(element :br ~@body))
(defmacro button {:style/indent 0} [& body] `(element :button ~@body))
(defmacro canvas {:style/indent 0} [& body] `(element :canvas ~@body))
(defmacro cite {:style/indent 0} [& body] `(element :cite ~@body))
(defmacro code {:style/indent 0} [& body] `(element :code ~@body))
(defmacro colgroup {:style/indent 0} [& body] `(element :colgroup ~@body))
(defmacro col {:style/indent 0} [& body] `(element :col ~@body))
(defmacro data {:style/indent 0} [& body] `(element :data ~@body))
(defmacro datalist {:style/indent 0} [& body] `(element :datalist ~@body))
(defmacro del {:style/indent 0} [& body] `(element :del ~@body))
(defmacro details {:style/indent 0} [& body] `(element :details ~@body))
(defmacro dfn {:style/indent 0} [& body] `(element :dfn ~@body))
(defmacro dialog {:style/indent 0} [& body] `(element :dialog ~@body))
(defmacro div {:style/indent 0} [& body] `(element :div ~@body))
(defmacro dl "The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the <dt> element) and descriptions (provided by <dd> elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs)." {:style/indent 0} [& body] `(element :dl ~@body))
(defmacro dt "The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a <dl> element. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element." {:style/indent 0} [& body] `(element :dt ~@body))
(defmacro dd "The <dd> HTML element provides the description, definition, or value for the preceding term (<dt>) in a description list (<dl>)." {:style/indent 0} [& body] `(element :dd ~@body))
(defmacro em {:style/indent 0} [& body] `(element :em ~@body))
(defmacro embed {:style/indent 0} [& body] `(element :embed ~@body))
(defmacro fieldset {:style/indent 0} [& body] `(element :fieldset ~@body))
(defmacro figure {:style/indent 0} [& body] `(element :figure ~@body))
(defmacro footer {:style/indent 0} [& body] `(element :footer ~@body))
(defmacro form {:style/indent 0} [& body] `(element :form ~@body))
(defmacro h1 {:style/indent 0} [& body] `(element :h1 ~@body))
(defmacro h2 {:style/indent 0} [& body] `(element :h2 ~@body))
(defmacro h3 {:style/indent 0} [& body] `(element :h3 ~@body))
(defmacro h4 {:style/indent 0} [& body] `(element :h4 ~@body))
(defmacro h5 {:style/indent 0} [& body] `(element :h5 ~@body))
(defmacro h6 {:style/indent 0} [& body] `(element :h6 ~@body))
(defmacro header {:style/indent 0} [& body] `(element :header ~@body))
(defmacro hgroup {:style/indent 0} [& body] `(element :hgroup ~@body))
(defmacro hr {:style/indent 0} [& body] `(element :hr ~@body))
(defmacro i {:style/indent 0} [& body] `(element :i ~@body))
(defmacro iframe {:style/indent 0} [& body] `(element :iframe ~@body))
(defmacro img {:style/indent 0} [& body] `(element :img ~@body))
(defmacro input {:style/indent 0} [& body] `(element :input ~@body))
(defmacro ins {:style/indent 0} [& body] `(element :ins ~@body))
(defmacro kbd {:style/indent 0} [& body] `(element :kbd ~@body))
(defmacro label {:style/indent 0} [& body] `(element :label ~@body))
(defmacro legend {:style/indent 0} [& body] `(element :legend ~@body))
(defmacro li {:style/indent 0} [& body] `(element :li ~@body))
(defmacro link {:style/indent 0} [& body] `(element :link ~@body))
(defmacro main {:style/indent 0} [& body] `(element :main ~@body))
#_(defmacro map {:style/indent 0} [& body] `(element :map ~@body))
(defmacro mark {:style/indent 0} [& body] `(element :mark ~@body))
(defmacro math {:style/indent 0} [& body] `(element :math ~@body))
(defmacro menu {:style/indent 0} [& body] `(element :menu ~@body))
(defmacro itemprop {:style/indent 0} [& body] `(element :itemprop ~@body))
(defmacro meter {:style/indent 0} [& body] `(element :meter ~@body))
(defmacro nav {:style/indent 0} [& body] `(element :nav ~@body))
(defmacro noscript {:style/indent 0} [& body] `(element :noscript ~@body))
(defmacro object {:style/indent 0} [& body] `(element :object ~@body))
(defmacro ol {:style/indent 0} [& body] `(element :ol ~@body))
(defmacro option {:style/indent 0} [& body] `(element :option ~@body))
(defmacro optgroup {:style/indent 0} [& body] `(element :optgroup ~@body))
(defmacro output {:style/indent 0} [& body] `(element :output ~@body))
(defmacro p {:style/indent 0} [& body] `(element :p ~@body))
(defmacro picture {:style/indent 0} [& body] `(element :picture ~@body))
(defmacro pre {:style/indent 0} [& body] `(element :pre ~@body))
(defmacro progress {:style/indent 0} [& body] `(element :progress ~@body))
(defmacro q {:style/indent 0} [& body] `(element :q ~@body))
(defmacro ruby {:style/indent 0} [& body] `(element :ruby ~@body))
(defmacro s {:style/indent 0} [& body] `(element :s ~@body))
(defmacro samp {:style/indent 0} [& body] `(element :samp ~@body))
(defmacro script {:style/indent 0} [& body] `(element :script ~@body))
(defmacro section {:style/indent 0} [& body] `(element :section ~@body))
(defmacro select {:style/indent 0} [& body] `(element :select ~@body))
(defmacro slot {:style/indent 0} [& body] `(element :slot ~@body))
(defmacro small {:style/indent 0} [& body] `(element :small ~@body))
(defmacro span {:style/indent 0} [& body] `(element :span ~@body))
(defmacro strong {:style/indent 0} [& body] `(element :strong ~@body))
(defmacro sub {:style/indent 0} [& body] `(element :sub ~@body))
(defmacro summary {:style/indent 0} [& body] `(element :summary ~@body))
(defmacro sup {:style/indent 0} [& body] `(element :sup ~@body))
(defmacro table {:style/indent 0} [& body] `(element :table ~@body))
(defmacro tbody {:style/indent 0} [& body] `(element :tbody ~@body))
(defmacro td {:style/indent 0} [& body] `(element :td ~@body))
(defmacro th {:style/indent 0} [& body] `(element :th ~@body))
(defmacro thead {:style/indent 0} [& body] `(element :thead ~@body))
(defmacro tr {:style/indent 0} [& body] `(element :tr ~@body))
(defmacro template {:style/indent 0} [& body] `(element :template ~@body))
(defmacro textarea {:style/indent 0} [& body] `(element :textarea ~@body))
(defmacro time {:style/indent 0} [& body] `(element :time ~@body))
(defmacro u {:style/indent 0} [& body] `(element :u ~@body))
(defmacro ul {:style/indent 0} [& body] `(element :ul ~@body))
(defmacro var {:style/indent 0} [& body] `(element :var ~@body))
(defmacro video {:style/indent 0} [& body] `(element :video ~@body))
(defmacro wbr {:style/indent 0} [& body] `(element :wbr ~@body))
