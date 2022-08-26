(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs])
  #?(:cljs (:require-macros user.demo-7-explorer)))

(def !target #?(:cljs (atom "src") :clj nil))

(p/defn App []
  (dom/div
    (dom/h1 "Explorer")
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    #_(dom/div "Folder: " (ui/button {::ui/click-event (p/fn [e] (reset! !target "node_modules"))} "node_modules"))
    (p/server
      (let [h (clojure.java.io/file (p/client (p/watch !target)))]
        (binding
          [;explorer/Render-row (p/fn [m])
           explorer/cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
           ;explorer/children ::fs/children
           explorer/search-attr ::fs/name
           explorer/Format (p/fn [k v]
                             (p/client
                               (case k
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
