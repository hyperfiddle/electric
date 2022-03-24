(ns hyperfiddle.common.transit
  (:require [cognitect.transit :as t]
            #?(:cljs [com.cognitect.transit.types]))
  #?(:clj (:import (java.io ByteArrayInputStream ByteArrayOutputStream))))

(def ^:dynamic string-encoding "UTF-8")

;; Custom handlers are allowed as decode and encode are not just for network
(defn decode
  "Transit decode an object from `s`."
  ([s] (decode s :json))
  ([s type] (decode s type {}))
  ([s type opts]
   #?(:clj
      (let [in (ByteArrayInputStream. (.getBytes ^String s ^String string-encoding))]
        (t/read (t/reader in type opts)))
      :cljs
      (t/read (t/reader type opts) s))))

(defn encode
  "Transit encode `x` into a String."
  ([x] (encode x :json))
  ([x type] (encode x type {}))
  ([x type opts]
   #?(:clj
      (let [out (ByteArrayOutputStream.)]
        (t/write (t/writer out type opts) x)
        (.toString out))
      :cljs
      (t/write (t/writer type opts) x))))

;;;;;;;;;;;;;;;;
;; EXTENTIONS ;;
;;;;;;;;;;;;;;;;

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728
