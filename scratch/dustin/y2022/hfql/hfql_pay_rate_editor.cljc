(ns dustin.y2022.hfql.hfql-pay-rate-editor
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-pay-rate-editor)))

(s/fdef school-base-pay-rates :args (s/cat :school (s/and some? ref?)))
(s/fdef set-base-pay-rate :args (s/cat :pay-rate-id . :base-pay-rate .))

(defmethod hf/tx-meta `set-base-pay-rate
  [schema [f e & _]]
  {::hf/tx-identifier  e
   ::hf/tx-cardinality ::hf/one
   ::hf/tx-conflicting? (fn [[f' e' & _]] (and (= f f') (= e e')))})

(p/defn School-base-pay-rates [school]
  (hf/hfql
    {{(school-base-pay-rates {school {:options (school-picklist search)
                                      :option-select (hf/replace-route! [`School-base-pay-rates school])}})
      [:school/name :school/id]}

     [:db/id
      ; this is a weird edge case where we want inline edits ? but where is the confirm button?
      ; need an edit pay rate button imo
      {:pay-rate/amount {:label "school invoice amount"
                         :render (ui/bigdec (p/server (bigdec/to-precision 2 pay-rate/amount)))
                         :inline-write (set-base-pay-rate db/id pay-rate/amount)}}
      pay-to-sub
      :pay-rate/interval
      :pay-rate/currency
      :pay-rate/type]}))
