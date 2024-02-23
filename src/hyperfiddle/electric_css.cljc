(ns hyperfiddle.electric-css
  "- Experimental — Use it at your own risk.
   - No support for at-rules yet."
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.electric-css])))

(defn find-rule-index "Find the rule index in the node sheet's CSSRuleList" [node target-rule]
   (let [rules (.-cssRules (.-sheet node))
         len (.-length rules)]
     (loop [i 0]
       (if (< i len)
         (if (= target-rule (aget rules i))
           i
           (recur (inc i)))
         -1))))

#?(:cljs
   (defn make-rule "Create a rule in node's stylesheet, return the created rule." [node selector]
     (let [sheet (.-sheet node)
           index (.-length (.-cssRules sheet))]
       (.insertRule sheet (str selector " {}") index)
       (aget (.-cssRules sheet) index))))

(defn delete-rule "Remove a given rule from node's stylesheet" [node rule]
  (let [idx (find-rule-index node rule)]
    (when (> idx -1)
      (.deleteRule (.-sheet node) idx))))

#?(:cljs
   (defn make-rule< "Create and emit a rule for `selector` on mount, remove the rule on unmount." [node selector]
     (m/relieve (m/observe (fn [!]
                             (let [rule (make-rule node selector)]
                               (! rule)
                               #(delete-rule node rule)))))))

(defn set-property [rule key value] (.setProperty rule (dom/to-str key) (dom/to-str value)))

(e/def scope "")

(defn scoped [scope selector]
  (if-not (empty? scope)
    (str "." scope " " selector)
    selector))

(defn rule*
  ([selector declarations] (rule* `dom/node selector declarations))
  ([node selector declarations]
   (when (seq declarations)
     `(doto (.-style (new (make-rule< ~node (scoped scope ~selector))))
        ~@(map (fn [[key value]] `(set-property ~key ~value)) declarations)))))

(defmacro rule
  ([declarations] (rule* "" declarations))
  ([selector declarations] (rule* selector declarations))
  ([node selector declarations] (rule* node selector declarations)))

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
                      (.add (.-classList style) (str "hyperfiddle_electric-css-"))
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
  `(binding [dom/node (new (identity stylesheet<))]
     ~@body))

(defmacro scoped-style [& body]
  `(binding [dom/node (new (identity stylesheet<))
             scope ~(str (munge (gensym "class_")))]
     ~@body
     scope))
