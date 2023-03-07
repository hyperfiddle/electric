(ns wip.demo-explorer2
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj clojure.java.io)
            [clojure.spec.alpha :as s]
            [contrib.datafy-fs #?(:clj :as :cljs :as-alias) fs]
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as router]
            [hyperfiddle.hfql-tree-grid :as ttgui]))

(def unicode-folder "\uD83D\uDCC2") ; 📂

(e/defn DirectoryExplorer-HFQL []
  (ttgui/with-gridsheet-renderer
    (binding [hf/db-name "$"]
      (dom/style {:grid-template-columns "repeat(5, 1fr)"})
      (e/server
        (binding [hf/*nav!*   (fn [db e a] (a (datafy e))) ;; FIXME db is specific, hfql should be general
                  hf/*schema* (constantly nil)] ;; FIXME this is datomic specific, hfql should be general
          (let [path (fs/absolute-path "node_modules")]
            (hf/hfql {(props (fs/list-files (props path {::dom/disabled true})) ;; FIXME forward props
                             {::hf/height 30})
                      [(props ::fs/name #_{::hf/render (e/fn [{::hf/keys [Value]}]
                                                       (let [v (Value.)]
                                                         (case (::fs/kind m)
                                                           ::fs/dir (let [absolute-path (::fs/absolute-path m)]
                                                                      (e/client (router/Link. [::fs/dir absolute-path] v)))
                                                           (::fs/other ::fs/symlink ::fs/unknown-kind) v
                                                           v #_(e/client (router/Link. [::fs/file x] v)))))})

                       ;; TODO add links and indentation

                       (props ::fs/modified {::hf/render (e/fn [{::hf/keys [Value]}]
                                                           (e/client
                                                             (dom/text
                                                               (-> (e/server (Value.))
                                                                   .toISOString
                                                                   (.substring 0 10)))))})
                       ::fs/size
                       (props ::fs/kind {::hf/render (e/fn [{::hf/keys [Value]}]
                                                       (let [v (Value.)]
                                                         (e/client
                                                           (case v
                                                             ::fs/dir (dom/text unicode-folder)
                                                             (dom/text (some-> v name))))))})
                       ]})))))))
