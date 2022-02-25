(ns hyperfiddle.spec
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.rcf :refer [tests]]))

(def parse parser/parse)

(defn cardinality
  "Guess the cardinality of a speced function or keyword."
  [spec]
  (when-let [type (:type (if (qualified-keyword? spec)
                           (parse spec)
                           (:ret (parse spec))))]
    (if (= ::coll type)
      ::many ::one)))

(tests
  (cardinality `(s/fspec :ret (s/coll-of any?))) := ::many ; can parse a spec definition
  (s/fdef foo :ret (s/coll-of any?))             := `foo ; put it in global registry
  (cardinality `foo)                             := ::many ; look up the registry
  )

(defn args [spec]
  (->> (parse spec)
       (:args)
       (:children)))

;; TODO WIP, account for mulitple arities
(defn arg "Given an fn spec, try to find spec for arg"
  [spec arg]
  (->> (args spec)
       (filter (comp #{(keyword arg)} :name))
       (first)))

(def types
  ;;  pred     type                           valueType
  [[`boolean? :hyperfiddle.spec.type/boolean :db.type/boolean]
   [`double?  :hyperfiddle.spec.type/double  :db.type/double]
   [`float?   :hyperfiddle.spec.type/float   :db.type/float]
   [`decimal? :hyperfiddle.spec.type/bigdec  :db.type/bigdec]
   [`inst?    :hyperfiddle.spec.type/instant :db.type/instant]
   [`keyword? :hyperfiddle.spec.type/keyword :db.type/keyword]
   [`string?  :hyperfiddle.spec.type/string  :db.type/string]
   [`uri?     :hyperfiddle.spec.type/uri     :db.type/uri]
   [`uuid?    :hyperfiddle.spec.type/uuid    :db.type/uuid]
   [`map?     :hyperfiddle.spec.type/ref     :db.type/ref]
   [`ref?     :hyperfiddle.spec.type/ref     :db.type/ref]
   [`symbol?  :hyperfiddle.spec.type/symbol  :db.type/symbol]
   [`integer? :hyperfiddle.spec.type/long    :db.type/long]
   [`number?  :hyperfiddle.spec.type/long    :db.type/long]
   [`nat-int? :hyperfiddle.spec.type/long    :db.type/long]
   [`int?     :hyperfiddle.spec.type/long    :db.type/long]
   [`pos-int? :hyperfiddle.spec.type/long    :db.type/long]])

(def pred->type (zipmap (map first types) (map second types)))
(def valueType->type (zipmap (map #(get % 2) types) (map second types)))
(def valueType->pred (zipmap (map #(get % 2) types) (map first types)))

(defn type-of
  ([spec] (pred->type (:predicate (parse spec))))
  ([spec argument] (pred->type (:predicate (arg spec argument)))))

(defn valueType-of [schema attr]
  (valueType->type (:db.valueType (get schema attr))))
