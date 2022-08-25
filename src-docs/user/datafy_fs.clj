(ns user.datafy-fs
  "nav implementation for java file system traversals"
  (:require [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :refer [tests]])
  (:import java.nio.file.Paths
           java.nio.file.Files
           java.io.File
           java.nio.file.LinkOption
           java.nio.file.attribute.BasicFileAttributes
           java.nio.file.attribute.FileTime))

; spec the data, not the object
(s/def ::name string?)
(s/def ::absolute-path string?)
(s/def ::modified inst?)
(s/def ::created inst?)
(s/def ::accessed inst?)
(s/def ::size string?)
(s/def ::kind qualified-keyword?) ; extension as well?
(s/def ::file (s/keys :opt [::name ::absolute-path ::modified ::created ::accessed ::size ::kind]))
(s/def ::children (s/coll-of ::file))

(defn get-extension [path]
  (let [found (last (re-find #"(\.[a-zA-Z0-9]+)$" path))
        ext (and found (subs found 1))]
    (or ext nil)))

(tests
  "get-extension"

  (tests
    "empty"
    (get-extension "") := nil
    (get-extension ".") := nil
    (get-extension "..") := nil
    (get-extension "image") := nil
    (get-extension "image.") := nil
    (get-extension "image..") := nil)

  (tests
    "found"
    (get-extension "image.png") := "png"
    (get-extension "image.blah.png") := "png"
    (get-extension "image.blah..png") := "png"))

(defn file-attrs [h]
  (-> h .getAbsolutePath
      (java.nio.file.Paths/get (make-array String 0))
      (java.nio.file.Files/readAttributes
        java.nio.file.attribute.BasicFileAttributes
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
  (datafy [file]
    ; represent object's top layer as EDN-ready value records, for display
    ; datafy is partial display view of an object as value records
    ; nav is ability to resolve back to the underlying object pointers
    ; they compose to navigate display views of objects like a link
    (let [attrs (file-attrs file)
          n (.getName file)]
      (as-> {::name n
             ::kind (cond (.isDirectory attrs) ::dir
                          (.isSymbolicLink attrs) ::symlink
                          (.isRegularFile attrs) (if-let [s (get-extension n)]
                                                   (keyword (namespace ::foo) s)
                                                   ::unknown-file-type)
                          (.isOther attrs) ::other)
             ::absolute-path (-> file .getAbsolutePath)
             ::created (-> attrs .creationTime .toInstant java.util.Date/from)
             ::accessed (-> attrs .lastAccessTime .toInstant java.util.Date/from)
             ::modified (-> attrs .lastModifiedTime .toInstant java.util.Date/from)
             ::size (.size attrs)} %
            (merge % (if (= ::dir (::kind %))
                       {::children (lazy-seq (.listFiles file))}))
            (with-meta % {`ccp/nav
                          (fn [xs k v]
                            (case k
                              ; reverse data back to object, to be datafied again by caller
                              ::modified (.lastModifiedTime attrs)
                              ::created (.creationTime attrs)
                              ::accessed (.lastAccessTime attrs)
                              ::children (some-> v vec)
                              v))})))))

(tests
  ; careful, calling seq loses metas on the underlying
  (def h (clojure.java.io/file "src/"))
  (type h) := java.io.File
  "(datafy file) returns an EDN-ready data view that is one layer deep"
  (datafy h)
  := #:user.datafy-fs{:name "src",
                      :absolute-path _,
                      :size _,
                      :modified _,
                      :created _,
                      :accessed _,
                      :kind ::dir,
                      :children _}

  "datafy of a directory includes a Clojure coll of children, but child elements are native file
  objects"
  (as-> (datafy h) %
        (nav % ::children (::children %))
        (datafy %)
        (take 2 (map type %)))
  := [java.io.File java.io.File]

  "nav to a leaf returns the native object"
  (as-> (datafy h) %
        (nav % ::modified (::modified %)))
  (type *1) := java.nio.file.attribute.FileTime

  "datafy again to get the plain value"
  (type (datafy *2)) := java.util.Date

  (as-> (datafy h) %
        (nav % ::children (::children %))
        (datafy %) ; can skip - simple data
        (nav % 0 (% 0))
        (datafy %)
        #_(s/conform ::file %))
  := #:user.datafy-fs{:name "hyperfiddle",
                      :absolute-path _,
                      :size _,
                      :modified _,
                      :created _,
                      :accessed _,
                      :kind ::dir,
                      :children _}
  nil)
