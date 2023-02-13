(ns hyperfiddle.client.edn-view
  (:require [clojure.string :as str]
            #?(:clj [hfdl.lang :refer [vars]])
            #?(:cljs [hyperfiddle.client.edn-view.linter :refer [Linter]])
            #?(:cljs [hyperfiddle.client.edn-view.diff :refer [patcher]])
            #?(:cljs [hyperfiddle.client.edn-view.extentions :refer [ExtentionsPlugin]])
            #?(:cljs [hyperfiddle.client.edn-view.spec-tooltip :refer [spec-tooltip]])
            #?(:cljs [hyperfiddle.client.router :as router])
            #?(:cljs [nextjournal.clojure-mode :as cm-clj])
            #?(:cljs ["@codemirror/fold" :as fold])
            #?(:cljs ["@codemirror/gutter" :refer [lineNumbers]])
            #?(:cljs ["@codemirror/highlight" :as highlight])
            #?(:cljs ["@codemirror/history" :refer [history historyKeymap]])
            #?(:cljs ["@codemirror/state" :refer [EditorState EditorSelection]])
            #?(:cljs ["@codemirror/view" :as view :refer [EditorView]])
            #?(:cljs [cljs.reader :as edn])
            #?(:cljs [hyperfiddle.client.ui :as ui])
            #?(:cljs [hyperfiddle.client.pprint :as pprint]))
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]])))

#?(:cljs
   (defn debounce
     ([f] (debounce f 600))
     ([f delay-ms]
      (let [timeout (volatile! nil)]
        (fn [& args]
          (when-let [t @timeout]
            (js/clearTimeout t))
          (vreset! timeout (js/setTimeout #(apply f args) delay-ms)))))))

#_
(def set-route!
  (debounce
   (fn [!result text]
     (try
       (let [edn (edn/read-string text)]
         (router/set-route! edn)
         (-> (ws/send! {:type :set-route!, :data edn})
             (p/then (fn [result]
                       (reset! !result (:data result))))
             (p/catch (fn [err]
                        (js/console.error err))))
         edn)
       (catch js/Error e
         (js/console.warn (ex-message e) (clj->js (ex-data e))))))))


(def theme
  #?(:cljs
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
   (defonce ^js extensions
     #js[theme
         (history)
         highlight/defaultHighlightStyle
         (view/drawSelection #js{:cursorBlinkRate 0})
         (lineNumbers)
         (fold/foldGutter)
         (.. EditorState -allowMultipleSelections (of true))
         ;; (.. EditorView -editable (of false))
         #_(if false
             ;; use live-reloading grammar
             #js[(cm-clj/syntax live-grammar/parser)
                 (.slice cm-clj/default-extensions 1)]
             cm-clj/default-extensions)
         cm-clj/default-extensions
         (.of view/keymap cm-clj/complete-keymap)
         (.of view/keymap historyKeymap)
         spec-tooltip
         ExtentionsPlugin
         Linter]))

(def make-state
  #?(:cljs
     (fn [^js extensions, ^string doc, on-update]
       (let [[doc ranges] (->> (re-seq #"\||<[^>]*?>|[^<>|]+" doc)
                            (reduce (fn [[^string doc ranges] match]
                                      (cond (= match "|")
                                            [doc (conj ranges (.cursor EditorSelection (count doc)))]

                                            (str/starts-with? match "<")
                                            [(str doc (subs match 1 (dec (count match))))
                                             (conj ranges (.range EditorSelection
                                                            (count doc)
                                                            (+ (count doc) (- (count match) 2))))]
                                            :else
                                            [(str doc match) ranges])) ["" []]))
             #_#_on-update (debounce on-update)]
         (.create EditorState
           #js{:doc        doc
               :selection  (if (seq ranges)
                             (.create EditorSelection (to-array ranges))
                             js/undefined)
               :extensions (let [exts #js[(.. EditorState -allowMultipleSelections (of true))
                                          (.. EditorView -updateListener (of (fn [^js view]
                                                                               (when (.-docChanged view)
                                                                                 (on-update (.. view -state -doc (toString))))
                                                                               true)))]]
                             (if extensions
                               (do (.push exts extensions)
                                   exts)
                               exts))})))))

(def editor
  #?(:cljs
     (fn [parent on-change]
       (new EditorView #js{:parent parent :state (make-state #js[extensions] "" on-change)}))))

(def set-editor-value!
  #?(:cljs
     (fn [^js view edn]
       ;; TODO slow, avoid parse and pprint
       (let [actual (edn/read-string (.. view -state -doc (sliceString 0)))]
         (when (not= edn actual)
           (pprint/pprint-async edn (fn [str]
                                      (.dispatch view #js{:changes #js {:from   0
                                                                        :to     (.. view -state -doc -length)
                                                                        :insert str}}))))))))

(def exports (vars editor set-editor-value!))

#_
(defn edn-view! [input-element, output-element, !route, !result]
  #?(:cljs
     (let [>route (m/watch !route)
           >result (m/watch !result)
           ^js route-view (editor input-element (fn [text] (set-route! !result text)))
           ^js result-view (editor output-element (fn [_]))
           route-patch! (patcher)
           ;; result-patch!   (patcher)
           route-editor (m/relieve {} (m/latest #(route-patch! route-view %) >route))
           result-editor (m/relieve {} (m/latest #(do #_(result-patch! result-view %)
                                                    (set-editor-value! result-view (pprint-str %))) >result))]
       ;; TODO call (.destroy editor) when >result is done, if ever.
       (route-editor #(prn :route-ready %) #(prn :route-done %))
       (result-editor #(prn :result-ready %) #(prn :result-done %)))))
