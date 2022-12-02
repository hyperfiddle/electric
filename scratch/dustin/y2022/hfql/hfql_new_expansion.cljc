(ns dustin.y2022.hfql.hfql-new-expansion
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-new-expansion)))


(comment
  (macroexpand
    '(hfql {(orders "") [{:order/gender [:db/ident]}
                         :order/shirt-size
                         (props :order/shirt-size {::hf/render Select-option-renderer
                                                   ::hf/options (shirt-sizes db/ident "")})]}))

  )

(tests
  (hfql {alice [{:order/gender [:db/ident]}]})
  := {9 {:order/gender {:db/ident :order/female}}})



(comment

  (macroexpand
    '(hfql [{:order/gender [:db/ident]}
            {(props :order/shirt-size {::hf/render (p/fn [.]
                                                     (dom/div ... v ...)

                                                     (Select-option-renderer. x))

                                       ::hf/render (ui/options {:options (shirt-sizes order/gender "")
                                                                :option-label (p/fn [x] (dom/span (pr-str (p/server (name (d/entity db x))))))})


                                       ::hf/options (shirt-sizes order/female "")})
             [:db/ident]}]))
  )
