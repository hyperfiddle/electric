(ns dustin.y2022.edn-render
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p])
  #?(:cljs (:require-macros [dustin.y2022.edn-render :refer [render-input render-select render-table]])))

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
