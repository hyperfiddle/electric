(ns dustin.hfql.hfql-renderquery2
  (:require [clojure.pprint :as pprint]
            [hfdl.impl.trace :refer [system debug]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]
            [minitest :refer [tests]]
            [datascript.core :as d]))

(defn query-email [db val]
  (m/ap (:dustingetz/email (d/entity db val))))

(defn render-email [>val]
  (dataflow (cljs.pprint/pprint ~@(d/q … ~@>val))))

(defn render-form [>db >val]
  (dataflow
   [:div @(render-email ~@@(query-email @>db @>val))]))


(tests
 (def !id (atom 9))
 (def >id (m/watch !id))
 (def !db (atom hyperfiddle.api/*$*))
 (def >db (m/watch !db))
 (def dag (dataflow (render-form >db >id)))
 ((system (debug sampler dag)) prn prn)
 @sampler
 )
