(ns hyperfiddle.client.edn-view
  (:require ["./edn_view/hoverTooltip" :refer [wordHover]]
            ["@codemirror/fold" :as fold]
            ["@codemirror/gutter" :refer [lineNumbers]]
            ["@codemirror/highlight" :as highlight]
            ["@codemirror/history" :refer [history historyKeymap]]
            ["@codemirror/state" :refer [EditorState EditorSelection]]
            ["@codemirror/view" :as view :refer [EditorView]]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [hyperfiddle.client.edn-view.linter :refer [Linter]]
            [hyperfiddle.client.edn-view.diff :refer [patcher]]
            [hyperfiddle.client.edn-view.links :refer [LinksPlugin]]
            [hyperfiddle.client.router :as router]
            [missionary.core :as m]
            [nextjournal.clojure-mode :as cm-clj]
            [hyperfiddle.client.ws :as ws]
            [promesa.core :as p]))

(def theme (.theme EditorView (clj->js {".cm-content"             {:white-space "pre-wrap"
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
                                        "&.cm-focused .cm-cursor" {:visibility "visible"}})))

(defonce ^js extensions #js[theme
                            (history)
                            highlight/defaultHighlightStyle
                            (view/drawSelection)
                            (lineNumbers)
                            (fold/foldGutter)
                            (.. EditorState -allowMultipleSelections (of true))
                            #_(if false
                                ;; use live-reloading grammar
                                #js[(cm-clj/syntax live-grammar/parser)
                                    (.slice cm-clj/default-extensions 1)]
                                cm-clj/default-extensions)
                            cm-clj/default-extensions
                            (.of view/keymap cm-clj/complete-keymap)
                            (.of view/keymap historyKeymap)
                            wordHover
                            LinksPlugin
                            Linter])

(defn make-state [^js extensions, ^string doc, on-update]
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
                                          [(str doc match) ranges])) ["" []]))]
    (.create EditorState
             #js{:doc        doc
                 :selection  (if (seq ranges)
                               (.create EditorSelection (to-array ranges))
                               js/undefined)
                 :extensions (let [exts #js[(.. EditorState -allowMultipleSelections (of true))
                                            (.. EditorView -updateListener (of (fn [^js view]
                                                                                 (when (.-docChanged view)
                                                                                   (js/setTimeout #(on-update (.. view -state -doc (toString))) 1))
                                                                                 true)))]]
                               (if extensions
                                 (do (.push exts extensions)
                                     exts)
                                 exts))})))

(defn set-editor-value! [^js view text]
  (.dispatch view #js{:changes #js {:from   0
                                    :to     (.. view -state -doc -length)
                                    :insert text}}))

(defn pprint-str [x]
  (with-out-str (pprint x)))

(defn debounce
  ([f] (debounce f 600))
  ([f delay-ms]
   (let [timeout (volatile! nil)]
     (fn [& args]
       (when-let [t @timeout]
         (js/clearTimeout t))
       (vreset! timeout (js/setTimeout #(apply f args) delay-ms))))))

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

(defn edn-view! [input-element, output-element, !route, !result]
  (let [>route          (m/watch !route)
        >result         (m/watch !result)
        ^js route-view  (new EditorView #js{:state  (make-state #js[extensions] "" (fn [text]
                                                                                     (set-route! !result text)))
                                            :parent input-element})
        ^js result-view (new EditorView #js{:state  (make-state #js[extensions] "" (fn [_text] ))
                                            :parent output-element})
        route-patch!    (patcher)
        ;; result-patch!   (patcher)
        route-editor    (m/relieve {} (m/latest #(route-patch! route-view %) >route))
        result-editor   (m/relieve {} (m/latest #(do #_(result-patch! result-view %)
                                                     (set-editor-value! result-view (pprint-str %))) >result))]
    ;; TODO call (.destroy editor) when >result is done, if ever.
    (route-editor #(prn :route-ready %) #(prn :route-done %))
    (result-editor #(prn :result-ready %) #(prn :result-done %))))

(defn ^:export boot! []
  (edn-view! (js/document.getElementById "hf-edn-view-route")
             (js/document.getElementById "hf-edn-view-output")
             router/!route
             (atom nil)))
