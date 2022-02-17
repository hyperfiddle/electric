(ns hyperfiddle.client.edn-view.spec-tooltip
  (:require ["./specTooltip" :refer [specTooltip]]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as spec]
            [clojure.string :as str]
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

(defn cat->vec [[_ & pairs]]
  (into [] (map vec (partition 2 pairs))))

(defn args [fn-sym]
  (when-let [form (some-> (spec/get-spec fn-sym)
                          (:args)
                          (spec/describe))]
    (let [[type & pairs :as args] form]
      (case type
        cat [(cat->vec args)]
        alt (->> (partition 2 pairs)
                 (mapv (fn [[_ cat]] (cat->vec cat))))))))

(defn- resolve-arg [sym, pos]
  (prn sym pos)
  (some-> (try
            (edn/parse-string sym)
            (catch :default _ nil))
          (args)
          (doto prn)
          (->> (map (fn [arity] (get arity (dec pos))))
               (filter some?)
               (map (fn [[nom pred]] (str nom " " pred)))
               (str/join "\n"))))

(def spec-tooltip (specTooltip resolve* resolve-arg))


