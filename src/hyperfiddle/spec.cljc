(ns hyperfiddle.spec
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.rcf :refer [tests]]))

(def parse parser/parse)

(defn cardinality
  "Guess the cardinality of a speced function."
  [fsym]
  (when-let [ret (:ret (parse fsym))]
    (if (= ::coll (:type ret))
      ::many ::one)))

(tests
  (cardinality `(s/fspec :ret (s/coll-of any?))) := ::many ; can parse a spec definition
  (s/fdef foo :ret (s/coll-of any?))             := `foo ; put it in global registry
  (cardinality `foo)                             := ::many ; look up the registry
  )

;; TODO WIP, account for mulitple arities
(defn arg "Given an fn spec, try to find spec for arg"
  [spec arg]
  (->> (parse spec)
       (:args)
       (:children)
       (filter (comp #{(keyword arg)} :name))
       (first)))

(def pred->type
  {`boolean? :hyperfiddle.spec.type/boolean
   `double?  :hyperfiddle.spec.type/double
   `float?   :hyperfiddle.spec.type/float
   `decimal? :hyperfiddle.spec.type/bigdec
   `inst?    :hyperfiddle.spec.type/instant
   `keyword? :hyperfiddle.spec.type/keyword
   `string?  :hyperfiddle.spec.type/string
   `uri?     :hyperfiddle.spec.type/uri
   `uuid?    :hyperfiddle.spec.type/uuid
   `map?     :hyperfiddle.spec.type/ref
   `ref?     :hyperfiddle.spec.type/ref
   `symbol?  :hyperfiddle.spec.type/symbol
   `integer? :hyperfiddle.spec.type/long
   `number?  :hyperfiddle.spec.type/long
   `nat-int? :hyperfiddle.spec.type/long
   `int?     :hyperfiddle.spec.type/long
   `pos-int? :hyperfiddle.spec.type/long})

(def valueType->type
  {:db.type/boolean :hyperfiddle.spec.type/boolean
   :db.type/double  :hyperfiddle.spec.type/double
   :db.type/float   :hyperfiddle.spec.type/float
   :db.type/bigdec  :hyperfiddle.spec.type/bigdec
   :db.type/instant :hyperfiddle.spec.type/instant
   :db.type/keyword :hyperfiddle.spec.type/keyword
   :db.type/string  :hyperfiddle.spec.type/string
   :db.type/uri     :hyperfiddle.spec.type/uri
   :db.type/uuid    :hyperfiddle.spec.type/uuid
   :db.type/ref     :hyperfiddle.spec.type/ref
   :db.type/symbol  :hyperfiddle.spec.type/symbol
   :db.type/long    :hyperfiddle.spec.type/long})

(defn type-of
  ([spec] (pred->type (:predicate (parse spec))))
  ([spec argument] (pred->type (:predicate (arg spec argument)))))

(defn valueType-of [schema attr]
  (valueType->type (:db.valueType (get schema attr))))
