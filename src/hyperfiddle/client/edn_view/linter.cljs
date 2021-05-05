(ns hyperfiddle.client.edn-view.linter
  (:require ["./spec" :refer [explain]]
            ["@codemirror/lint" :refer [linter]]
            [clojure.spec.alpha :as s]
            [edamame.core :refer [parse-string]]))

(defn locate [^js state, err]
  (js/console.log state)
  (let [{:keys [:row :col]} (ex-data err)
        from-line           (.. state -doc (line row) -from)
        ^js node            (.. state -tree (resolve (+ from-line col)))]
    {:from (.-from node)
     :to   (.-to node)}))

(defn err->diagnostics [^js state, err]
  (let [{:keys [:type :expr
                :edamame/expected-delimiter
                :edamame/opened-delimiter]} (ex-data err)
        message                             (ex-message err)
        {:keys [:from :to]} (locate state err)]
    [#js{:from     from
         :to       to
         :severity (case type
                     :edamame/error "error")
         :source   "EDN Linter"
         :message  (cond
                     expected-delimiter (str "Expected " expected-delimiter
                                             " to match " opened-delimiter)
                     :else message)}]))

(defn lint [^js state]
  (try
    (parse-string (.. state -doc (toString)))
    []
    (catch :default e
      (doto (err->diagnostics state e)
        js/console.log))))


(defn explainf [sexpr]
  (try
    (let [edn    (parse-string sexpr)
          result (cond
                   (list? edn) (let [[f & args] edn]
                                 (when (symbol? f)
                                   (when-let [args-spec (:args (s/get-spec f))]
                                     (s/explain-str args-spec args))))
                   :else       nil)]
      (when-not (= "Success!\n" result)
        result))
    (catch :default _ nil)))

(defonce Linter (linter (fn [^js view]
                          (let [lint-errors (lint (.-state view))
                                spec-errors (array-seq (explain (.-state view) explainf))]
                            (into-array (concat lint-errors spec-errors))))))
