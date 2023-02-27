(ns user.demo-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj clojure.java.io)
            [contrib.data :refer [treelister]]
            [contrib.datafy-fs #?(:clj :as :cljs :as-alias) fs]
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as router]
            [contrib.gridsheet :as gridsheet :refer [Explorer]]))

(def unicode-folder "\uD83D\uDCC2") ; 📂

(e/defn Render-cell [m a]
  (let [v (a m)]
    (case a
      ::fs/name (case (::fs/kind m)
                  ::fs/dir (let [absolute-path (::fs/absolute-path m)]
                             (e/client (router/link absolute-path (dom/text v))))
                  (::fs/other ::fs/symlink ::fs/unknown-kind) (e/client (dom/text v))
                  (e/client (dom/text v)))
      ::fs/modified (e/client (some-> v .toLocaleDateString dom/text))
      ::fs/kind (case (::fs/kind m)
                  ::fs/dir (e/client (dom/text unicode-folder))
                  (e/client (some-> v name dom/text)))
      (e/client (dom/text (str v))))))

(e/defn Dir [x]
  (let [m (datafy x)
        xs (nav m ::fs/children (::fs/children m))]
    (e/client (dom/h1 (dom/text (e/server (::fs/absolute-path m)))))
    (Explorer.
      (treelister xs ::fs/children #(contrib.str/includes-str? (::fs/name %) %2))
      {::dom/style {:height "calc((20 + 1) * 24px)"}
       ::gridsheet/page-size 20
       ::gridsheet/row-height 24
       ::gridsheet/Format Render-cell
       ::gridsheet/columns [::fs/name ::fs/modified ::fs/size ::fs/kind]
       ::gridsheet/grid-template-columns "auto 8em 5em 3em"})))

(e/defn DirectoryExplorer []
  (e/client
    (dom/link (dom/props {:rel :stylesheet, :href "user/gridsheet-optional.css"}))
    (dom/div (dom/props {:class "user-gridsheet-demo"})
      (binding [router/build-route (fn [[self state local-route] local-route']
                                     ; root local links through this entrypoint
                                     `[DirectoryExplorer ~state ~local-route'])]
        (e/server
          (let [[self s route] (e/client router/route)
                fs-path (or route (fs/absolute-path "./"))]
            (e/client
              (router/router 1 ; focus state slot, todo: fix IndexOutOfBounds exception
                (e/server
                  (Dir. (clojure.java.io/file fs-path)))))))))))

; Improvements
; Native search
; lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
