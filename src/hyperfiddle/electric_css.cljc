(ns hyperfiddle.electric-css
  "Experimental. Use it at your own risk."
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.electric-css])))

(defn get-rule "get a css rule in the node's stylesheet by index" [node index]
  (aget (.-cssRules (.-sheet node)) index))

#?(:cljs
   (defn make-rule "return the created rule index" [node selector]
     (let [sheet (.-sheet node)]
       (.insertRule sheet (str selector " {}")))))

(defn delete-rule "Delete a rule by index in a node's stylesheet" [node index]
  (.deleteRule (.-sheet node) index))

#?(:cljs
   (defn make-rule< [node selector]
     (m/relieve (m/observe (fn [!]
                             (let [idx (make-rule node selector)]
                               (! (get-rule node idx))
                               #(delete-rule node idx)))))))

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

(defmacro style
  "Usage:
  (dom/div
    (dom/props {:class \"my-div\"})
    (css/style
      (css/rule \".my-div\" {:color :red})
      (css/rule \".my-div:hover\" {:color :blue})))
  "
  [& body]
  `(dom/element "style" ~@body))

(defmacro scoped-style [& body]
  `(style
     (binding [scope ~(str (munge (gensym "class_")))]
       ~@body
       scope)))
