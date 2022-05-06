(ns hyperfiddle.ui.codemirror
  #?(:clj  (:require clojure.edn
                     clojure.pprint
                     [hyperfiddle.photon :as p]
                     hyperfiddle.photon-dom
                     [hyperfiddle.photon-xp :as xp]
                     [missionary.core :as m]
                     [clojure.edn :as edn]
                     [clojure.pprint :as pprint]
                     [hyperfiddle.dev.logger :as log])
     :cljs (:require
             [clojure.edn :as edn]
             [clojure.pprint :as pprint]
             [hyperfiddle.dev.logger :as log]
             [hyperfiddle.photon :as p]
             hyperfiddle.photon-dom
             [hyperfiddle.photon-xp :as xp]
             [missionary.core :as m]
             ["@codemirror/fold" :as fold]
             ["@codemirror/gutter" :refer [lineNumbers]]
             ["@codemirror/highlight" :as highlight]
             ["@codemirror/history" :refer [history historyKeymap]]
             ["@codemirror/state" :refer [EditorState]]
             ["@codemirror/view" :as view :refer [EditorView]]
             [nextjournal.clojure-mode :as cm-clj]))
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
                                                                                    (on-update view-update)
                                                                                    true)))])))})))

#?(:cljs (defn make-editor [props on-change]
           (new EditorView #js{:parent (:parent props) :state (make-state props "nil" on-change)})))

(def set-editor-value!
  #?(:cljs
     (fn [^js view, new-value]
       (let [change #js {:from   0
                         :to     (.. view -state -doc -length)
                         :insert (str new-value)}]
         (.dispatch view #js {:changes change})))))

(def ^{:doc "Return a pair of `[view >value]` :      
  - `view` is an Editor instance.
  - `>value` is a discreet flow of the editor content (string)."}
  codemirror
  #?(:cljs (fn [props]
             (let [on-change! (atom (constantly nil))
                   ^js view   (make-editor props (fn [^js view-update]
                                                   (when (and (.. view-update -view -hasFocus) ;; user manual action
                                                              (.-docChanged view-update))                                                     (prn "CM - Change!" (.. view-update -state -doc (toString)))
                                                         (@on-change! (.. view-update -state -doc (toString))))))]
               [view (m/observe (fn [!]
                                  (reset! on-change! !)
                                  #(.destroy view)))]))))


(p/defn CodeMirror [props readf writef value]
  (let [[view >value] (codemirror props)]
    (set-editor-value! view (writef value))
    (new (->> >value
           (m/eduction (map readf))
           (xp/newest (p/fn [] value))
           (xp/continuous)))))

(defn read-edn [edn-str]
  (try (edn/read-string edn-str)
       (catch #?(:clj Throwable :cljs :default) t
         (hyperfiddle.dev.logger/error t)
         nil)))

(defn write-edn [edn] (with-out-str (pprint/pprint edn)))

(p/defn edn [v] (new CodeMirror {:parent hyperfiddle.photon-dom/parent} read-edn write-edn v))
