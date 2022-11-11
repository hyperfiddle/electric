(ns wip.hfql-explorer
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
            [hyperfiddle.hfql2 :refer [hfql]]
            [hyperfiddle.api :as hf]
            [clojure.spec.alpha :as s]
            #?(:cljs [hyperfiddle.router :as router])
            [missionary.core :as m])
  #?(:cljs (:require-macros [wip.hfql-explorer :refer [absolute-path]])))


(p/defn Email [V]
  (let [v (V.)]
    (p/client
      (dom/a {:href (str "mailto:" v)} (dom/text v))
      nil)))

(p/defn Dir []
  (binding
      [explorer/cols [:k :v]
       hf/db         hf/*$*
       hf/*schema*   wip.orders-datascript/schema
       hf/*nav!*     wip.orders-datascript/nav!]
    (let [!needle (atom "")
          needle  (p/watch !needle)
          xs      (new (hf/Sequence. (hf/TreeToRows. (hfql {(wip.orders-datascript/orders needle)
                                                            [:db/id
                                                             (props :order/email {::hf/render Email})
                                                             {:order/gender [:db/ident]}
                                                             :order/tags
                                                             {:order/shirt-size [:db/ident]}]}))))]
      (Explorer. "HFQL paginated"
        (fn [needle] (reset! !needle needle) xs)
        {::dom/style                       {:height "calc((10 + 1) * 24px)"}
         ::explorer/page-size              10
         ::explorer/row-height             24
         ::gridsheet/grid-template-columns "auto auto"}))))

(def unicode-folder "\uD83D\uDCC2") ; 📂

(p/defn App []
  (p/client
    (binding []
      (dom/h1 "Explorer")
      (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
      (dom/div {:class "photon-demo-explorer"}
        (p/server
          (binding [explorer/Format (p/fn [M a]
                                      (let [row (M.)]
                                        (some-> (get row (case a :k 0 :v 1))
                                          (new)
                                          (pr-str))))]
            (Dir.)))))))


(comment
  (do (datascript.core/transact! dev/conn [[:db/add 9 :order/email "alice@example.com"]])
      nil)
  )





; Improvements
; Native search
; lazy folding/unfolding directories (no need for pagination)
; forms (currently table hardcoded with recursive pull)
; useful ::fs/file route
