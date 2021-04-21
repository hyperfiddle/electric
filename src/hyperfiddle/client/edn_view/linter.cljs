(ns hyperfiddle.client.edn-view.linter
  (:require ["@codemirror/lint" :refer [linter]]
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
    #js[#js{:from     from
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
    #js[]
    (catch :default e
      (doto (err->diagnostics state e)
        js/console.log))))


(defonce Linter (linter (fn [^js view]
                          (lint (.-state view)))))
