(ns hyperfiddle.client.edn-view.spec-tooltip
  (:require ["./specTooltip" :refer [specTooltip]]
            [clojure.spec.alpha :as spec]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]))

(defn drop-ns [sym]
  (symbol (name sym)))

(def obvious-ns? #{"cljs.spec.alpha" "cljs.core"})

(defn- unalias [form]
  (walk/prewalk (fn [x]
                  (if (and (symbol? x) (obvious-ns? (namespace x)))
                    (drop-ns x)
                    x))
                form))

(defn- resolve* [sym-or-kw, type]
  (with-out-str
    (some-> (spec/get-spec (case type
                             "Symbol"  (symbol sym-or-kw)
                             "Keyword" (keyword (subs sym-or-kw 1))))
            (spec/form)
            (unalias)
            (pprint))))

(def spec-tooltip (specTooltip resolve*))
