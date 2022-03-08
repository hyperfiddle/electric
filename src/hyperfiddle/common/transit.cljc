(ns hyperfiddle.common.transit
  (:require [cognitect.transit :as t]
            [hfdl.impl.runtime :as pr :refer #?(:clj [] :cljs [Failure Pending])]
            [hfdl.impl.util :as u]
            #?(:cljs [com.cognitect.transit.types]))
  #?(:clj (:import (hfdl.impl.runtime Failure Pending)
                   (java.io ByteArrayInputStream ByteArrayOutputStream))))

(def remote-exception
  #?(:clj (Error. "Remote exception.")
     :cljs (js/Error. "Remote exception.")))

(def read-handlers
  {"hyperfiddle.Failure" (t/read-handler (fn [code]
                                           (pr/->Failure
                                             (case code
                                               :remote remote-exception
                                               :pending (pr/->Pending)
                                               :cancelled (missionary.Cancelled.)))))})

(def write-handlers
  {Failure (t/write-handler (constantly "hyperfiddle.Failure")
             (fn [{:keys [error]}]
               (if (instance? Pending error)
                 :pending (if (instance? missionary.Cancelled error)
                            :cancelled (do (u/pst error) :remote)))))})

(def ^:dynamic string-encoding "UTF-8")

;; Custom handlers are allowed as decode and encode are not just for network
(defn decode
  "Transit decode an object from `s`."
  ([s] (decode s :json))
  ([s type] (decode s type {}))
  ([s type opts]
   (let [opts (update opts :handlers merge read-handlers)]
     #?(:clj
        (let [in (ByteArrayInputStream. (.getBytes ^String s ^String string-encoding))]
          (t/read (t/reader in type opts)))
        :cljs
        (t/read (t/reader type opts) s)))))

(defn encode
  "Transit encode `x` into a String."
  ([x] (encode x :json))
  ([x type] (encode x type {}))
  ([x type opts]
   (let [opts (update opts :handlers merge write-handlers)]
     #?(:clj
        (let [out (ByteArrayOutputStream.)]
          (t/write (t/writer out type opts) x)
          (.toString out))
        :cljs
        (t/write (t/writer type opts) x)))))

;;;;;;;;;;;;;;;;
;; EXTENTIONS ;;
;;;;;;;;;;;;;;;;

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728
