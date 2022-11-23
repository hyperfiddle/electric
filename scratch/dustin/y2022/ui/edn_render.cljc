(ns dustin.y2022.edn-render
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p])
  #?(:cljs (:require-macros [dustin.y2022.edn-render :refer [edn-input edn-select edn-table]])))

(p/defn edn-table [>v props]
  (let [xs (p/$ hf/render >v {})]
    ['table
     ['thead (keys (first xs))]
     ['tbody (p/for [x xs]
               ['tr x])]]))

(p/defn edn-input [v props]
  ['input ~v])

(p/defn edn-select [v props]
  ['select
   ((or (::hf/option-label props) identity) (p/$ hf/render v {}))
   (p/for [option ~(::hf/options props)]
     ((or (::hf/option-label props) identity) option))])
