(ns wip.hfql-links-page
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.ui :as ui]
            [hyperfiddle.hfql.router :as r]
            [wip.orders :refer [genders one-order orders shirt-sizes]])
  #?(:cljs (:require-macros [hyperfiddle.hfql.router :refer [router]]
                            [wip.hfql-links-page])))

(p/defn App [route]
  (r/router route
   {(one-order sub) [:db/id :order/email]}
   {(orders .) [(props :db/id {::hf/link one-order})
                :order/email
                {(props :order/gender
                        {::hf/options      (genders)
                         ::hf/option-label :db/ident
                         ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
                {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                           ::hf/option-label :db/ident
                                           ::hf/render ui/typeahead}) [:db/ident]}]}))
