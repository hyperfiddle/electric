(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs])
  #?(:cljs (:require-macros user.demo-7-explorer)))

(def !target #?(:clj (atom (clojure.java.io/file "src")) ; "node_modules"
                :cljs nil))

(p/defn Navigate! [x] (reset! !target x))

(p/defn Nav-link [label x]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e]
                                          (.preventDefault e)
                                          (p/server (Navigate!. x)))} label)))

(p/defn App []
  (dom/div
    (dom/h1 "Explorer")
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    (p/server
      (let [h (p/watch !target)]
        (binding
          [explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
           explorer/children ::fs/children
           explorer/search-attr ::fs/name
           explorer/Format (p/fn [x m a v]
                             (p/client
                               (case a
                                 ::fs/name (p/server (case (::fs/kind m)
                                                       ::fs/dir (Nav-link. v x)
                                                       v))
                                 ::fs/modified (.toLocaleDateString v)
                                 ::fs/kind (name v)
                                 (str v))))]
          (let [m (datafy h)
                xs (nav m ::fs/children (::fs/children m))]
            (Explorer. xs (::fs/absolute-path m))))))))

; Improvements
; Native search
; reduce amount of dom nodes (avoid p/hook)
;   grid instead of table
;   lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
