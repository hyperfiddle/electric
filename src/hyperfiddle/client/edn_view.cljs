(ns hyperfiddle.client.edn-view
  (:require ["./hoverTooltip" :refer [wordHover]]
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
            [missionary.core :as m]
            [nextjournal.clojure-mode :as cm-clj]))

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

(def edn '{(submissions "") [#:dustingetz{:email  "alice@example.com"
                                          :gender {:db/ident                       :dustingetz/female
                                                   (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                                    #:db{:ident :dustingetz/womens-medium}
                                                                                    #:db{:ident :dustingetz/womens-large}]}}
                             #:dustingetz{:email  "bob@example.com"
                                          :gender {:db/ident                       :dustingetz/male
                                                   (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                    #:db{:ident :dustingetz/mens-medium}
                                                                                    #:db{:ident :dustingetz/mens-large}]}}
                             #:dustingetz{:email  "charlie@example.com"
                                          :gender {:db/ident                       :dustingetz/male
                                                   (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                    #:db{:ident :dustingetz/mens-medium}
                                                                                    #:db{:ident :dustingetz/mens-large}]}}]
           (genders)        [#:db{:ident :dustingetz/male}
                             #:db{:ident :dustingetz/female}]})

(defonce !edn (atom edn))
(defonce >edn (m/watch !edn))

(defn ^:export set-edn! [str]
  (try
    (reset! !edn (edn/read-string str))
    (catch js/Error e
      (js/console.warn (ex-message e) (clj->js (ex-data e))))))

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
                            ;; wordHover
                            (.. EditorView -updateListener (of (fn [^js view]
                                                                 (js/setTimeout #(set-edn! (.. view -state -doc (toString)))
                                                                                1)
                                                                 true)))
                            Linter])

(defn make-state [^js extensions, ^string doc]
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
                 :extensions (let [exts #js[(.. EditorState -allowMultipleSelections (of true))]]
                               (if extensions
                                 (do (.push exts extensions)
                                     exts)
                                 exts))})))

(defn pprint-str [x]
  (with-out-str (pprint x)))

(defn edn-view! [dom-element, >edn]
  (let [^js view (new EditorView #js{:state  (make-state #js[extensions] "")
                                     :parent dom-element})
        patch!   (patcher)]
    ;; TODO call (.destroy editor) when >edn is done, if ever.
    (->> >edn
         (m/latest (fn [edn]
                     (patch! view edn)
                     edn))
         (m/relieve {}))))

(defn ^:export render []
  ((edn-view! (js/document.getElementById "hf-edn-view-output")
              >edn)
   #(prn :ready)
   #(prn :done)))
