(ns hyperfiddle.electric-css31
  "Dom3 compatible electric-css
   - Experimental — Use it at your own risk.
   - Partial at-rules support (only @keyframes ATM)
  "
  (:require [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.dom31 :as-alias dom]
            [missionary.core :as m]
            [clojure.string :as str]
            #?(:cljs [goog.style]))
  #?(:cljs (:require-macros [hyperfiddle.electric-css31])))

(defprotocol StyledElement
  "Define an object containing CSS rules"
  (sheet [this])
  (css-rules [this])
  (find-rule [this rule])
  (add-rule [this rule] [this rule index])
  (delete-rule [this rule]))

(defn rule-index "Find the rule index in the node sheet's CSSRuleList" [styled-element target-rule]
  (let [rules (css-rules styled-element)
        len (.-length rules)]
    (loop [i 0]
      (if (< i len)
        (if (= target-rule (aget rules i))
          i
          (recur (inc i)))
        -1))))

#?(:cljs
   (extend-protocol StyledElement
     js/HTMLStyleElement
     (sheet [^js this] (.-sheet this))
     (css-rules [^js this] (css-rules (sheet this)))
     (find-rule [this rule] (rule-index this rule))
     (add-rule
       ([this rule] (add-rule (sheet this) rule))
       ([this rule index] (add-rule (sheet this) rule index)))
     js/CSSStyleSheet
     (sheet [this] this)
     (css-rules [^js this] (.-cssRules this))
     (find-rule [this rule] (rule-index this rule))
     (add-rule
       ([this rule] (add-rule this rule (count (css-rules this)))) ; add at the end
       ([^js this rule index] (.insertRule this rule index)))
     (delete-rule [^js this rule]
       (let [idx (find-rule this rule)]
         (when (> idx -1)
           (.deleteRule this idx))))
     js/CSSGroupingRule
     (sheet [this] this)
     (css-rules [^js this] (.-cssRules this))
     (find-rule [this rule] (rule-index this rule))
     (add-rule
       ([this rule] (add-rule this rule (count (css-rules this)))) ; add at the end
       ([^js this rule index] (.insertRule this rule index)))
     (delete-rule [^js this rule]
       (let [idx (find-rule this rule)]
         (when (> idx -1)
           (.deleteRule this idx))))
     js/CSSKeyframesRule ; not a subclass of CSSGroupingRule
     (sheet [this] this)
     (css-rules [^js this] (.-cssRules this))
     (find-rule [^js this rule] (.findRule this (.-keyText rule))) ; identity?
     (add-rule
       ([this rule] (add-rule this rule nil)) ; no support for index-based insert
       ([^js this rule _] (.appendRule this rule)))
     (delete-rule [^js this rule] (.deleteRule this (.-keyText rule)))))

(defprotocol StyleRule
  "Interface over a CSS rule"
  (set-property [this key value]))

(defn to-str [x]
  (cond (string? x)  x
        (keyword? x) (name x)
        (symbol? x)  (str x)
        :else        (str x)))

#?(:cljs
   (extend-protocol StyleRule
     js/HTMLElement
     (set-property [^js this key value] (set-property (.-style this) key value))
     js/CSSStyleRule
     (set-property [^js this key value] (set-property (.-style this) key value))
     js/CSSKeyframeRule ; not a subclass of CSSStyleRule
     (set-property [^js this key value] (set-property (.-style this) key value))
     js/CSSStyleDeclaration
     (set-property [^js this key value]
       (let [key (to-str key)]
         (if (str/starts-with? key "--") ; CSS variable
           (.setProperty this key value)
           (when-let [property (goog.style/getVendorJsStyleName_ this key)] ; normalize property names
             (.setProperty this property value)))))))

#?(:cljs
   (defn make-rule "Create a rule in node's stylesheet, return the created rule." [styled-element selector]
     (let [sheet (sheet styled-element)
           index (.-length (css-rules sheet))]
       (add-rule sheet (str (to-str selector) " {}") index)
       (aget (css-rules sheet) index))))

#?(:cljs
   (defn make-rule< "Create and emit a rule for `selector` on mount, remove the rule on unmount." [styled-element selector]
     (m/relieve (m/observe (fn [!]
                             (let [rule (make-rule styled-element selector)]
                               (! rule)
                               #(delete-rule styled-element rule)))))))

(defn rule* [styled-element selector declarations]
  (when (seq declarations)
    `(doto (e/input (make-rule< ~styled-element ~selector))
       ~@(map (fn [[key value]] `(set-property ~key ~value)) declarations))))

(def selector "")

(defn concat-selectors [selectorA selectorB]
  (if (empty? selectorA)
    selectorB
    (str selectorA
      (let [selectorB (str/trim selectorB)]
        (if (str/starts-with? selectorB "&")
          (str/replace-first selectorB "&" "")
          (str " " selectorB))))))

(def scope "")

(defn scoped [scope selector]
  (if-not (empty? scope)
    (concat-selectors (str "." scope)  selector)
    selector))

(defmacro rule [selector & declarations]
  (let [[selector declarations] (if (map? selector) ["&" (cons selector declarations)] [selector declarations])]
    `(binding [selector (scoped scope (concat-selectors selector ~selector))]
       ~@(map #(rule* `dom/node `selector %) (filter map? declarations))
       ~@(remove map? declarations))))

(comment
  (rule {:color :red})
  (rule "foo" {:color :red :height 2} {:width 1})
  (rule "foo" {:color :red}
        (rule "&.bar" {:color :blue}))
  )

(defmacro keyframes "Create an @keyframes <animation-name> rule group. Note @keyframes are always
  global, even if defined in a scoped style. Can only contain `keyframe` rules."
  [animation-name & keyframes]
  `(binding [dom/node (e/input (make-rule< dom/node ~(str "@keyframes " animation-name)))]
     ~@keyframes))

(defmacro keyframe
  "Take a `stop` string (e.g. \"from\", \"to\", \"0%\", \"50%\", etc...) and a map of css declarations to apply at the given `stop`.
   Will add the animation stop to the current `keyframes`. Can only be used in a `keyframes` block.
  Note that adding or removing a `keyframe` at runtime resets running animations, but changing a keyframe's content doesn't. "
  [stop declarations]
  (rule* `dom/node stop declarations))

(def stylesheet<  "Mount a singleton stylesheet in the documents's <head> to gather all CSS rules"
  #?(:cljs
     (m/signal ; We only need one top-level stylesheet into which we inject rules and manage their lifecycle.
       ;; We could use `document.adoptedStyleSheets`, but:
       ;; - Safari support is still young.
       ;; - no clear advantage over the current approach.
       ;;   - only advantage seem to be saving on a `<style>` element in head
       ;;   - perf should be equivalent since we mutates the `<style>`'s stylesheet CSSOM directly.
       (m/observe (fn [!]
                    (let [style (.createElement js/document "style")]
                      (.add (.-classList style) (str "hyperfiddle_electric-css"))
                      (.appendChild style (.createComment js/document "This node may contain dynamic styles. Inspect them with $(\"style.hyperfiddle_electric-css\").sheet.cssRules ."))
                      (.appendChild (.-head js/document) style)
                      (! style)
                      #(.removeChild (.-parentElement style) style)))))))

(defmacro style
  "Usage:
  (dom/div
    (dom/props {:class \"my-div\"})
    (css/style
      (css/rule \".my-div\" {:color :red})
      (css/rule \".my-div:hover\" {:color :blue})))
  "
  [& body]
  `(binding [dom/node (e/input (identity stylesheet<))]
     ~@body))

(defmacro scoped-style [& body]
  `(binding [dom/node (e/input (identity stylesheet<))
             scope (str (munge (gensym "class_")))]
     ~@body
     scope))

(defn grid-template
  "Convenient way to build a string compatible with CSS grid-template rule.
  Takes a collection of rows definition [dimention1 … dimentionN] or [[[area1 … areaN] dimention] …]
  And an optional collection of columns dimentions [dimention1 … dimentionN].

  E.g. (grid-template [[[:logo   :title   :user-info] :auto]
                       [[:menu   :content :content  ] :auto]
                       [[:footer :footer  :footer   ] \"1fr\"]]
        [\"20rem\" :auto])
   := \"\"logo title user-info\" auto \"menu content content\" auto \"footer footer footer\" 1fr / 20rem auto\"
  "
  ([rows] (grid-template rows nil))
  ([rows columns]
   (let [row           (fn [row]
                         (if (coll? row)
                           (if (vector? (first row))
                             (let [[area dimention] row
                                   dim              (some-> dimention name)]
                               (str (pr-str (str/join " " (map name area))) (some->> dim (str " "))))
                             (name (or (first row) "")))
                           (name row)))
         rows          (remove nil? rows)
         template+rows (str/join " " (map row rows))]
     (if-not columns
       template+rows
       (str template+rows " / " (str/join " " (map name (remove nil? columns))))))))

(comment
  (grid-template []) := ""
  (grid-template [:auto]) := "auto"
  (grid-template [:auto :auto "1fr"]) := "auto auto 1fr"
  (grid-template [:auto :auto "1fr"] [:auto :auto]) := "auto auto 1fr / auto auto"
  (grid-template [[[:first-row]  :auto]
                  [[:second-row] :auto]
                  [[:third-row]  "1fr"]])
   := "\"first-row\" auto \"second-row\" auto \"third-row\" 1fr"
  (grid-template [[[:first-row]  :auto]
                  [[:second-row] :auto]
                  [[:third-row]  "1fr"]]
    [:auto :auto])
   := "\"first-row\" auto \"second-row\" auto \"third-row\" 1fr / auto auto"

  (grid-template
    [[[:search :search]   :min-content]
     [[:refs    :log]     "1fr"]
     [[:details :details] #_:auto]]
    [:auto "1fr"])
   := "\"search search\" min-content \"refs log\" 1fr \"details details\" / auto 1fr"
  )

(e/defn Style
  "Set a style `property` name to `value` on a `styled` object (e.g. DOM node, CSS Stylesheet, CSS Rule etc.)."
  ;; Multiple calls to Style on the same node and same property will race.
  ;; They won't stack or restore previous value.
  [styled property value]
  (e/client
    (set-property styled property value)
    (e/on-unmount (partial set-property styled property nil))
    value))
