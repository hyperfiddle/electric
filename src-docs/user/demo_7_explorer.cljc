(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            contrib.ednish
            clojure.edn
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs]
            [user.util :refer [includes-str? pprint-str]]
            #?(:cljs [hyperfiddle.router :as router])
            [missionary.core :as m])
  #?(:cljs (:require-macros [user.demo-7-explorer :refer [absolute-path]])))

(p/def !path (p/client (m/mbx)))

(defmacro absolute-path [path & paths]
  #?(:clj (str (.toAbsolutePath (java.nio.file.Path/of ^String path (into-array String paths))))
     :cljs (throw (js/Error. "Unsupported operation."))))

(defn encode-path [route] (->> route pr-str contrib.ednish/encode (str "/")))
(defn decode-path [path] {:pre [(string? path)]}
  (if-not (= path "/")
    (-> path (subs 1) contrib.ednish/decode clojure.edn/read-string)
    [::fs/dir (absolute-path "node_modules")]))

(p/defn Nav-link [route label]
  (p/client
    (let [path (encode-path route)]
      (ui/element dom/a {::dom/href path
                         ::ui/click-event (p/fn [e]
                                            (.preventDefault e)
                                            (router/pushState! !path path))} label))))

(p/defn Dir [x]
  (binding
    [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
     explorer/Children (p/fn [m] (::fs/children m))
     explorer/Search? (p/fn [m s] (includes-str? (::fs/name m) s))]
    (let [m (datafy x)
          xs (nav m ::fs/children (::fs/children m))]
      (Explorer. (::fs/absolute-path m) xs
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
                                                      ::fs/dir (Nav-link. [::fs/dir (::fs/absolute-path m)] v)
                                                      (::fs/other ::fs/symlink ::fs/unknown-kind) v
                                                      v #_(Nav-link. [::fs/file x] v))
                                          ::fs/modified (p/client (some-> v .toLocaleDateString))
                                          ::fs/kind (case (::fs/kind m)
                                                      ::fs/dir unicode-folder
                                                      (some-> v name))
                                          (str v))))]
            (let [[page fs-path] (p/client (decode-path (new (router/path> !path))))]
              (case page
                ;::fs/file (File. (clojure.java.io/file fs-path))
                ::fs/dir (Dir. (clojure.java.io/file fs-path))))))))))

; Improvements
; Native search
; lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
; useful ::fs/file route