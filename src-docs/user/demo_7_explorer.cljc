(ns user.demo-7-explorer
  #?(:cljs (:require-macros [user.demo-7-explorer :refer [absolute-path]]))
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj clojure.java.io)
            [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.router :as router]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs]))

(defmacro absolute-path [path & paths]
  #?(:clj (str (.toAbsolutePath (java.nio.file.Path/of ^String path (into-array String paths))))
     :cljs (throw (js/Error. "Unsupported operation."))))

(defn list-files [path]
  #?(:clj (let [m (datafy (clojure.java.io/file path))]
            (nav m ::fs/children (::fs/children m)))))

(s/fdef list-files :args (s/cat :file any?) :ret (s/coll-of any?))

(def unicode-folder "\uD83D\uDCC2") ; 📂

(p/defn App []
  (ttgui/with-gridsheet-renderer
    (binding [hf/db-name "$"]
      (dom/style {:grid-template-columns "repeat(5, 1fr)"})
      (p/server
        (binding [hf/*nav!*   (fn [db e a] (a (datafy e))) ;; FIXME db is specific, hfql should be general
                  hf/*schema* (constantly nil)] ;; FIXME this is datomic specific, hfql should be general
          (let [path (absolute-path "node_modules")]
            (hf/hfql {(props (list-files (props path {::dom/disabled true})) ;; FIXME forward props
                             {::hf/height 30})
                      [(props ::fs/name #_{::hf/render (p/fn [{::hf/keys [Value]}]
                                                       (let [v (Value.)]
                                                         (case (::fs/kind m)
                                                           ::fs/dir (let [absolute-path (::fs/absolute-path m)]
                                                                      (p/client (router/Link. [::fs/dir absolute-path] v)))
                                                           (::fs/other ::fs/symlink ::fs/unknown-kind) v
                                                           v #_(p/client (router/Link. [::fs/file x] v)))))})

                       ;; TODO add links and indentation

                       (props ::fs/modified {::hf/render (p/fn [{::hf/keys [Value]}]
                                                           (p/client
                                                             (dom/text
                                                               (-> (p/server (Value.))
                                                                   .toISOString
                                                                   (.substring 0 10)))))})
                       ::fs/size
                       (props ::fs/kind {::hf/render (p/fn [{::hf/keys [Value]}]
                                                       (let [v (Value.)]
                                                         (p/client
                                                           (case v
                                                             ::fs/dir (dom/text unicode-folder)
                                                             (dom/text (some-> v name))))))})
                       ]})))))))

;; Previous impl

;; (p/defn Dir [x]
;;   (binding
;;     [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]]
;;     (let [m (datafy x)
;;           xs (nav m ::fs/children (::fs/children m))]
;;       (Explorer. (::fs/absolute-path m) (explorer/tree-lister xs ::fs/children #(explorer/includes-str? (::fs/name %) %2))
;;                  {::dom/style {:height "calc((20 + 1) * 24px)"}
;;                   ::explorer/page-size 20
;;                   ::explorer/row-height 24
;;                   ::gridsheet/grid-template-columns "auto 8em 5em 3em"}))))

;(p/defn File [x]
;  (binding
;    [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
;     explorer/Search? (p/fn [m s] (includes-str? (::fs/name m) s))]
;    (let [m (datafy x)
;          xs [m]]
;      (Explorer. (::fs/absolute-path m) xs
;                 {::dom/style {:height "calc((20 + 1) * 24px)"}
;                  ::explorer/page-size 20
;                  ::explorer/row-height 24
;                  ::gridsheet/grid-template-columns "auto 8em 5em 3em"}))))

;; (p/defn PrevApp []
;;   (p/client
;;     (dom/h1 "Explorer")
;;     (dom/link {:rel :stylesheet, :href ".archives/user_demo_explorer.css"})
;;     (dom/div {:class "photon-demo-explorer"}
;;       (p/server
;;         (binding [explorer/Format (p/fn [m a]
;;                                     (let [v (a m)]
;;                                       (case a
;;                                         ::fs/name (case (::fs/kind m)
;;                                                     ::fs/dir (let [absolute-path (::fs/absolute-path m)]
;;                                                                (p/client (router/Link. [::fs/dir absolute-path] v)))
;;                                                     (::fs/other ::fs/symlink ::fs/unknown-kind) v
;;                                                     v #_(p/client (router/Link. [::fs/file x] v)))
;;                                         ::fs/modified (p/client (some-> v .toLocaleDateString))
;;                                         ::fs/kind (case (::fs/kind m)
;;                                                     ::fs/dir unicode-folder
;;                                                     (some-> v name))
;;                                         (str v))))]
;;           (let [[page fs-path] (or (p/client (::hf/route hf/route))
;;                                  [::fs/dir (absolute-path "node_modules") ])]
;;             (case page
;;                                         ;::fs/file (File. (clojure.java.io/file fs-path))
;;               ::fs/dir (Dir. (clojure.java.io/file fs-path)))))))))
;;                                         ; Improvements
;;                                         ; Native search
;;                                         ; lazy folding/unfolding directories (no need for pagination)
;;                                         ; forms (currently table hardcoded with recursive pull)
;;                                         ; useful ::fs/file route
