(ns geoffrey.meta-keyword)

(deftype MetaKeyword [^clojure.lang.Keyword kw meta]
  Object
  (equals [_ o] (= o kw))
  (hashCode [_] (.hashCode kw))
  (toString [_] (.toString kw))

  Comparable
  (compareTo [_ o] (.compareTo kw o))

  clojure.lang.IHashEq
  (hasheq [_] (.hasheq kw))

  clojure.lang.Named
  (getNamespace [_] (.getNamespace kw))
  (getName [_] (.getName kw))

  clojure.lang.IObj
  (withMeta [_ meta] (MetaKeyword. kw meta))
  clojure.lang.IMeta
  (meta [_] meta)

  clojure.lang.IFn
  (call [_] (.throwArity kw 0))
  (invoke [_] (.throwArity kw 0))
  (invoke [_ obj] (.invoke kw obj))
  (invoke [_ obj not-found] (.invoke kw obj not-found)))

(defn ->MetaKeyword [sym]
  (MetaKeyword. sym nil))

(compare :a (->MetaKeyword :a)) ;; FAIL
(keyword? (->MetaKeyword :a))   ;; FAIL

(defmethod print-method MetaKeyword [^MetaKeyword o w] (.write w (.toString o)))


(require '[clojure.tools.reader :as reader])
(alter-meta! #'reader/starting-line-col-info assoc :private false)
(alter-meta! #'reader/read* assoc :private false)


(defn read-meta
  "Read metadata and return the following object with the metadata applied"
  [rdr _ opts pending-forms]
  (clojure.tools.reader.reader-types/log-source rdr
              (let [[line column] (clojure.tools.reader/starting-line-col-info rdr)
                    m (clojure.tools.reader.impl.utils/desugar-meta
                       (reader/read* rdr true nil opts pending-forms))]
                (when-not (map? m)
                  (clojure.tools.reader.impl.errors/throw-bad-metadata rdr m))
                (let [o (reader/read* rdr true nil opts pending-forms)
                      o (if (keyword? o) (->MetaKeyword o) o)]
                  (if (instance? clojure.lang.IMeta o)
                    (let [m (if (and line (seq? o))
                              (assoc m :line line :column column)
                              m)]
                      (if (instance? clojure.lang.IObj o)
                        (with-meta o (merge (meta o) m))
                        (reset-meta! o m)))
                    (clojure.tools.reader.impl.errors/throw-bad-metadata-target rdr o))))))

(with-redefs [clojure.tools.reader/read-meta read-meta]
  (clojure.tools.reader/read-string "^{:meta true} :a"))
