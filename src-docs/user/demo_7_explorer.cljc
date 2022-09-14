(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

; route on the server means we can pass references in the route
; "node_modules"
(def !route #?(:clj (atom [::fs/dir (clojure.java.io/file "src")])))
(comment
  (reset! !route [::fs/dir (clojure.java.io/file "src")])
  (reset! !route [::fs/dir (clojure.java.io/file "node_modules")])
  (-> @!route second datafy))

(p/def Navigate!)

(p/defn Nav-link [x label]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e]
                                          (.preventDefault e)
                                          (p/server (Navigate!. x)))} label)))

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

(p/defn File [x]
  (binding
    [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
     explorer/Search? (p/fn [m s] (includes-str? (::fs/name m) s))]
    (let [m (datafy x)
          xs [m]]
      (Explorer. (::fs/absolute-path m) xs
                 {::dom/style {:height "calc((20 + 1) * 24px)"}
                  ::explorer/page-size 20
                  ::explorer/row-height 24
                  ::gridsheet/grid-template-columns "auto 8em 5em 3em"}))))

(def unicode-folder "\uD83D\uDCC2") ; 📂

(p/defn App []
  (binding [Navigate! (p/fn [x] (reset! !route x))]
    (p/client
      (dom/h1 "Explorer")
      (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
      (dom/div {:class "photon-demo-explorer"}
        (p/server
          (binding [explorer/Format (p/fn [m a v]
                                      (let [x (:clojure.datafy/obj (meta m))]
                                        (case a
                                          ::fs/name (case (::fs/kind m)
                                                      ::fs/dir (Nav-link. [::fs/dir x] v)
                                                      (::fs/other ::fs/symlink ::fs/unknown-kind) v
                                                      (Nav-link. [::fs/file x] v))
                                          ::fs/modified (p/client (some-> v .toLocaleDateString))
                                          ::fs/kind (case (::fs/kind m)
                                                      ::fs/dir unicode-folder
                                                      (some-> v name))
                                          (str v))))]
            (let [[page x] (p/watch !route)]
              (case page
                ::fs/file (File. x)
                ::fs/dir (Dir. x)))))))))

; Improvements
; Native search
; reduce amount of dom nodes (avoid p/hook)
; lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
