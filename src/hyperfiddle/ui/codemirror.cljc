(ns hyperfiddle.ui.codemirror
  (:require
   [hfdl.lang :as p]
   [hfdl.impl.runtime]
   #?(:cljs [clojure.pprint :as pprint])
   #?(:cljs ["@codemirror/fold" :as fold])
   #?(:cljs ["@codemirror/gutter" :refer [lineNumbers]])
   #?(:cljs ["@codemirror/highlight" :as highlight])
   #?(:cljs ["@codemirror/history" :refer [history historyKeymap]])
   #?(:cljs ["@codemirror/state" :refer [EditorState]])
   #?(:cljs ["@codemirror/view" :as view :refer [EditorView]])
   #?(:cljs [nextjournal.clojure-mode :as cm-clj])
   #?(:cljs [missionary.core :as m]))
  #?(:cljs (:require-macros [hyperfiddle.ui.codemirror :refer [CodeMirror]])))

#?(:cljs
   (def theme
     (.theme EditorView (clj->js {".cm-content"             {:white-space "pre-wrap"
                                                             :padding     "10px 0"}
                                  "&.cm-focused"            {:outline "none"}
                                  ".cm-line"                {:padding     "0 0.5rem"
                                                             :line-height "1.6"
                                                             :font-size   "16px"
                                                             :font-family "var(--code-font)"}
                                  ".cm-matchingBracket"     {:border-bottom "1px solid var(--teal-color)"
                                                             :color         "inherit"}
                                  ".cm-gutters"             {:background "transparent"
                                                             :border     "none"}
                                  ".cm-gutterElement"       {:margin-left "5px"}
                                      ;; only show cursor when focused
                                  ".cm-cursor"              {:visibility "hidden"}
                                  "&.cm-focused .cm-cursor" {:visibility "visible"}}))))

#?(:cljs
   (defonce inline-extensions
     [theme
      (history)
      highlight/defaultHighlightStyle
      (view/drawSelection #js{:cursorBlinkRate 0})
      cm-clj/default-extensions
      (.of view/keymap cm-clj/complete-keymap)
      (.of view/keymap historyKeymap)]))

#?(:cljs
   (defn make-state [props ^string doc, on-update]
     (.create EditorState
              #js{:doc        doc
                  :extensions (into-array
                               (cond-> inline-extensions
                                 (not (:inline props)) (concat [(lineNumbers) (fold/foldGutter)])
                                 true (concat [(.. EditorView -updateListener (of (fn [^js view-update]
                                                                                    (when (.-docChanged view-update)
                                                                                      (on-update view-update))
                                                                                    true)))])))})))

#?(:cljs (defn make-editor [props on-change]
           (new EditorView #js{:parent (:parent props) :state (make-state props "" on-change)})))

#?(:cljs (def set-editor-value!
           (fn [^js view edn]
             (js/console.log "set on view" edn view)
             (let [str (with-out-str (pprint/pprint edn))]
               (.dispatch view #js{:changes #js {:from   0
                                                 :to     (.. view -state -doc -length)
                                                 :insert str}})))))

#?(:cljs (defn codemirror [props]
           (let [on-change! (atom (constantly nil))
                 ^js view   (make-editor props (fn [^js view-update]
                                                 (@on-change! (.. view-update -state -doc (toString)))))]
             [view (m/observe (fn [!]
                                (! nil)
                                (reset! on-change! !)
                                #(.destroy view)))])))

#_{:clj-kondo/ignore [:unused-binding]}
(p/defn CodeMirror [props value]
  #?(:cljs
     (let [[view >value'] (codemirror props)]
       (set-editor-value! view value)
       ~>value')))