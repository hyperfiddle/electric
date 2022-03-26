(ns user.hytradboi
  (:require clojure.edn
            clojure.pprint
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui6 :as ui]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.q9 :as q9 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders submission shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :refer [router]]
            [hyperfiddle.ui.codemirror :as codemirror])
  #?(:cljs (:require-macros [hyperfiddle.hfql.router :refer [router]]
                            [hyperfiddle.q9 :refer [hfql]]
                            [user.hytradboi :refer [view App render-input render-select render-table]]
                            [user.gender-shirt-size :refer [submissions]])))

(p/defn render-table [>v props]
  (let [xs (p/$ hf/render >v {})]
    ['table
     ['thead (keys (first xs))]
     ['tbody (p/for [x xs]
               ['tr x])]]))

(p/defn render-input [v props]
  ['input ~v])

(p/defn render-select [v props]
  ['select
   ((or (::hf/option-label props) identity) (p/$ hf/render v {}))
   (p/for [option ~(::hf/options props)]
     ((or (::hf/option-label props) identity) option))])

(p/defn App []
  (binding [hf/render ui/render]
    (hyperfiddle.q9/hfql
      {(submissions .)
       [:db/id
        :dustingetz/email
        {(props :dustingetz/gender {::hf/options      (genders)
                                    ::hf/option-label :db/ident
                                    #_#_::hf/render ui/typeahead})
         [:db/ident]}
        {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes :dustingetz/male .)
                                        #_#_::hf/render ui/typeahead
                                        ::hf/option-label :db/ident})
         [:db/ident]}]})))

(p/defn view []
  (p/$ codemirror/edn ~@(ui/with-spec-render (p/$ App))))

(def exports (p/vars))
