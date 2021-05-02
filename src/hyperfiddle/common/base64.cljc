(ns hyperfiddle.common.base64
  (:require [clojure.string :as str])
  #?(:cljs (:require [goog.crypt.base64 :as base64]))
  #?(:clj (:import (java.util Base64))))

(defn encode-string [s]
  #?(:clj  (.encodeToString (Base64/getEncoder) (.getBytes s))
     :cljs (base64/encodeString s)))

(defn decode-string [s]
  #?(:clj  (String. (.decode (Base64/getDecoder) (.getBytes s)))
     :cljs (base64/decodeString s)))

(defn encode-url-safe [s]
  (-> (encode-string s)
      (str/replace \+ \-)
      (str/replace \/ \_)
      (str/replace \= \,)))

(defn decode-url-safe [s]
  (-> s
      (str/replace \- \+)
      (str/replace \_ \/)
      (str/replace \, \=)
      (decode-string)))
