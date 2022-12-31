(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            contrib.ednish
            clojure.edn
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs]
            #?(:cljs [hyperfiddle.router :as router]))
  #?(:cljs (:require-macros [user.demo-7-explorer :refer [absolute-path]])))

(defmacro absolute-path [path & paths]
  #?(:clj (str (.toAbsolutePath (java.nio.file.Path/of ^String path (into-array String paths))))
     :cljs (throw (js/Error. "Unsupported operation."))))

(p/defn Dir [x]
  (binding
    [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]]
    (let [m (datafy x)
          xs (nav m ::fs/children (::fs/children m))]
      (Explorer. (::fs/absolute-path m) (explorer/tree-lister xs ::fs/children #(explorer/includes-str? (::fs/name %) %2))
                 {::dom/style {:height "calc((20 + 1) * 24px)"}
                  ::explorer/page-size 20
                  ::explorer/row-height 24
                  ::gridsheet/grid-template-columns "auto 8em 5em 3em"}))))

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

(def unicode-folder "\uD83D\uDCC2") ; 📂

(p/defn App []
  (p/client
    (binding []
      (dom/h1 "Explorer")
      (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
      (dom/div {:class "photon-demo-explorer"}
        (p/server
          (binding [explorer/Format (p/fn [m a]
                                      (let [v (a m)]
                                        (case a
                                          ::fs/name (case (::fs/kind m)
                                                      ::fs/dir (p/client (router/Link. [::fs/dir (::fs/absolute-path m)] v))
                                                      (::fs/other ::fs/symlink ::fs/unknown-kind) v
                                                      v #_(p/client (router/Link. [::fs/file x] v)))
                                          ::fs/modified (p/client (some-> v .toLocaleDateString))
                                          ::fs/kind (case (::fs/kind m)
                                                      ::fs/dir unicode-folder
                                                      (some-> v name))
                                          (str v))))]
            (let [[page fs-path] (p/client (or hf/route [::fs/dir (absolute-path "node_modules")]))]
              (case page
                ;::fs/file (File. (clojure.java.io/file fs-path))
                ::fs/dir (Dir. (clojure.java.io/file fs-path))))))))))

; Improvements
; Native search
; lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
; useful ::fs/file route
