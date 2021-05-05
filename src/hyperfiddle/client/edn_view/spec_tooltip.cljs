(ns hyperfiddle.client.edn-view.spec-tooltip
  (:require ["./specTooltip" :refer [specTooltip]]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as spec]
            [clojure.walk :as walk]
            [edamame.core :as edn]))

(defn drop-ns [sym]
  (symbol (name sym)))

(def obvious-ns? #{"cljs.spec.alpha" "cljs.core"})

(defn- unalias [form]
  (walk/prewalk (fn [x]
                  (if (and (symbol? x) (obvious-ns? (namespace x)))
                    (drop-ns x)
                    x))
                form))

(defn- resolve* [sym-or-kw-text]
  (with-out-str
    (some-> (try
              (edn/parse-string sym-or-kw-text)
              (catch :default _ nil))
            (spec/get-spec)
            (spec/form)
            (unalias)
            (pprint))))

(def spec-tooltip (specTooltip resolve*))
