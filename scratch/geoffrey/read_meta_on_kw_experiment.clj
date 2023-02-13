(ns hyperfiddle.q8.reader
  (:refer-clojure :exclude [read-string])
  (:require
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.impl.errors :as err]
   [clojure.tools.reader.impl.utils :as utils]
   [hyperfiddle.q8.impl :as impl]
   [clojure.tools.reader.reader-types :as reader-types])
  (:import (clojure.lang IObj IMeta)))

(def ^:private starting-line-col-info @#'reader/starting-line-col-info)
(def ^:private read* @#'reader/read*)

(defn- read-meta
  "Read metadata and return the following object with the metadata applied"
  [rdr _ opts pending-forms]
  (reader-types/log-source rdr
                           (let [[line column] (starting-line-col-info rdr)
                                 m             (utils/desugar-meta (read* rdr true nil opts pending-forms))]
                             (when-not (map? m)
                               (err/throw-bad-metadata rdr m))
                             (let [o (read* rdr true nil opts pending-forms)]
                               (cond (instance? IMeta o) (let [m (if (and line (seq? o))
                                                                   (assoc m :line line :column column)
                                                                   m)]
                                                           (if (instance? IObj o)
                                                             (with-meta o (merge (meta o) m))
                                                             (reset-meta! o m)))
                                     (keyword? o)        (impl/meta-keyword o m)
                                     :else               (err/throw-bad-metadata-target rdr o))))))

(defn read-string [^String s]
  (assert (string? s) "HFQL expression must be a string")
  (binding [reader/*data-readers*           *data-readers*
            reader/*default-data-reader-fn* *default-data-reader-fn*
            reader/*alias-map*              (merge {'hf (the-ns 'hyperfiddle.api)}
                                                   (ns-aliases *ns*))]
    (with-redefs [reader/read-meta read-meta]
      (list 'quote `(hyperfiddle.q8/hfql ~(reader/read-string s))))))
