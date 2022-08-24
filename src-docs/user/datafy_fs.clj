(ns user.datafy-fs
  "nav implementation for java file system traversals"
  (:require [hyperfiddle.rcf :refer [tests]]
            [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :as ccp :refer [nav]]
            clojure.repl)
  (:import java.nio.file.Paths
           java.nio.file.Files
           java.io.File
           java.nio.file.LinkOption
           java.nio.file.attribute.BasicFileAttributes
           java.nio.file.attribute.FileTime))

(def ... `...) ; resolve to symbol for test assertions

(defn file-attrs [h]
  (-> h
      .getAbsolutePath
      (java.nio.file.Paths/get (make-array String 0))
      (java.nio.file.Files/readAttributes java.nio.file.attribute.BasicFileAttributes
                                          (make-array java.nio.file.LinkOption 0))))

(comment
  (def h (clojure.java.io/file "src/"))
  (.getName h)
  (.isDirectory h)
  (.isFile h)
  (.listFiles h)
  (for [x (take 5 (.listFiles h))]
    (.getName x))
  (.getPath h)
  (seq (.listFiles h)))

(extend-protocol ccp/Datafiable
  java.nio.file.attribute.FileTime
  (datafy [o] (-> o .toInstant java.util.Date/from)))

(extend-protocol ccp/Datafiable
  java.io.File
  (datafy [o]
    ; represent object as EDN-ready data, partially (e.g. one level deep).
    ; datafy is partial print with ability to resolve further via lazy links
    (let [m (file-attrs o)]
      (-> (merge {::name (.getName o)
                  #_#_::creationTime (-> m .creationTime .toInstant java.util.Date/from)
                  #_#_::lastAccessTime (-> m .lastAccessTime .toInstant java.util.Date/from)
                  ::lastModifiedTime (-> m .lastModifiedTime .toInstant java.util.Date/from)
                  ;::isRegularFile (if-not (.isRegularFile m) :not-a-regular-file)
                  ;::isSymbolicLink (if (.isSymbolicLink m) :link)
                  ;::isOther (if (.isOther m) :other nil)
                  ::size (.size m)}
                 (if (.isDirectory m)
                   {::isDirectory (if (.isDirectory m) ::folder ::file)
                    ::children (lazy-seq (.listFiles o))}))
          (with-meta {`ccp/nav
                      (fn [xs k v]
                        (case k
                          ::lastModifiedTime (.lastModifiedTime m)
                          ::children (vec v)
                          v))})))))

(tests
  ; careful, calling seq loses metas on the underlying
  (def h (clojure.java.io/file "src/"))
  (type h) := java.io.File
  (datafy h)
  := #:user.datafy-fs{:name "src",
                      :lastModifiedTime _,
                      :size _,
                      :isDirectory ::folder,
                      :children _}
  (as-> (datafy h) %
        (nav % ::children (::children %))
        (datafy %)
        (map type %))
  := [java.io.File java.io.File]

  "nav to a leaf returns the native object"
  (as-> (datafy h) %
        (nav % ::lastModifiedTime (::lastModifiedTime %)))
  (type *1) := java.nio.file.attribute.FileTime

  "datafy again to get the plain value"
  (type (datafy *2)) := java.util.Date

  (as-> (datafy h) <>
        (nav <> ::children (::children <>))
        (datafy <>) ; can skip - simple data
        (nav <> 0 (<> 0))
        (datafy <>))
  := #:user.datafy-fs{:name "hyperfiddle",
                      :lastModifiedTime _,
                      :size _,
                      :isDirectory ::folder,
                      :children _})
