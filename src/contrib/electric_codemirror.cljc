(ns contrib.electric-codemirror
  #?(:cljs (:require-macros contrib.electric-codemirror))
  (:require
    [clojure.edn :as edn]
    [clojure.pprint :as pprint]
    #?(:clj clojure.tools.logging)
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [missionary.core :as m]
    [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
    #?@(:cljs [["@codemirror/language" :as language]
               ["@codemirror/state" :refer [EditorState]]
               ["@codemirror/commands" :refer [history historyKeymap]]
               ["@codemirror/view" :as view :refer [EditorView lineNumbers]]
               [nextjournal.clojure-mode :as cm-clj]])))

#?(:cljs
   (def theme
     (.theme EditorView (clj->js {#_#_".cm-content"             {:white-space "pre-wrap"
                                                             :padding     "10px 0"}
                                  "&.cm-focused"            {:outline "none"}
                                  #_#_".cm-line"                {:padding     "0 0.5rem"
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
     (list
       theme
       (history)
       (language/syntaxHighlighting language/defaultHighlightStyle)
       (view/drawSelection #js{:cursorBlinkRate 0})
       ;; cm-clj/default-extensions
       ;; (.of view/keymap cm-clj/complete-keymap)
       (cm-clj/syntax)
       (.of view/keymap historyKeymap)
       )))

#?(:cljs
   (defn make-state [props ^string doc, on-update]
     (.create EditorState
       #js{:doc        doc
           :extensions (into-array
                         (cond->> inline-extensions
                           (:theme props) (cons (.theme EditorView (clj->js (:theme props))))
                           (:readonly props) (cons (.. EditorState -readOnly (of true)))
                           (not (:inline props)) (into (list (lineNumbers) (language/foldGutter)))
                           true (cons (.. EditorView -updateListener (of (fn [^js view-update]
                                                                           (on-update view-update)
                                                                           true))))))})))

#?(:cljs
   (defn make-cm! [props on-change]
     (new EditorView #js{:parent (:parent props) :state (make-state props "nil" on-change)})))

#?(:cljs
   (defn cm-set! [^js !cm v]
     (.dispatch !cm #js {:changes #js {:insert (str v)
                                       :from 0 :to (.. !cm -state -doc -length)}})))

#?(:cljs
   (defn codemirror [props]
     (let [!hook (atom nil)
           >cm-v (m/observe
                   (fn [!]
                     #_(println 'cm-mount)
                     (let [^js !cm (make-cm! props (fn [^js cm-view-update]
                                                     (when (and (.. cm-view-update -view -hasFocus) ;; user manual action
                                                             (.-docChanged cm-view-update))
                                                       (let [v (.. cm-view-update -state -doc (toString))]
                                                         (assert (some? v))
                                                         (! v)))))]
                       (reset! !hook !cm) ; ref escapes
                       #(do #_(println 'cm-unmount) (.destroy !cm)))))]
       (m/cp [(m/?< (m/watch !hook)) ; cm ref escapes
              >cm-v])))) ; this is discrete. Don't accidentally damage this by giving it a nil initial value

(e/defn CodeMirror [props readf writef controlled-value]
  (when-some [[!cm >cm-v] (new (codemirror props))] ; stable through cv changes
    (some-> !cm (cm-set! (writef controlled-value))) ; guard "when true" bug causing NPE in certain tutorials
    (doto (new (m/relieve {} (m/reductions #(readf %2) controlled-value >cm-v))) ; reduction rebuilt if cv changes, which is fine
      #_(as-> $ (println 'cm-v (hash $))))))

(defn read-edn [edn-str]
  (try (edn/read-string edn-str)
       (catch #?(:clj Throwable :cljs :default) t
         #?(:clj (clojure.tools.logging/error t)
            :cljs (js/console.warn t)) nil)))

(defn write-edn [edn] (with-out-str (pprint/pprint edn)))

(e/defn edn [v] (new CodeMirror {:parent dom/node} read-edn write-edn v))
(e/defn string [v] (new CodeMirror {:parent dom/node} identity identity v))

#_
(tests "cm/string"
  (def discard (e/run (binding [dom/node js/document.body]
                        (tap (new string "hi")))))
  ;; (def line (.querySelector js/document ".cm-line"))
  ;; (def content (.querySelector js/document ".cm-line"))
  ;; (.dispatchEvent line (js/Event. "mousedown"))
  ;; (.dispatchEvent content (js/Event. "mousedown"))
  ;; (uit/focus line)
  ;; (uit/focus content)
  ;; (set! (.-innerText line) "there")

  % := "hi"
  ;; TODO I see this works when trying out in the REPL and interacting with the browser manually
  ;; but I can't seem to trigger the user-like typing behavior from the test.
  ;; Exposing the EditorView doesn't help because the `on-change` handler
  ;; checks if the action was a user action.
  ;; % := "there"
  ;; % := "buddy"
  (discard)
  )
