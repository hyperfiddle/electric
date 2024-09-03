(ns contrib.base64
  (:require [clojure.set :refer [map-invert]]
            clojure.string
            [hyperfiddle.rcf :refer [tests]]
            #?(:cljs [goog.crypt.base64 :as base64]))
  #?(:clj (:import (java.util Base64))))

(defn base64-encode [s]
  #?(:clj  (.encodeToString (Base64/getEncoder) (.getBytes s))
     :cljs (base64/encodeString s)
     :node (.toString (js/Buffer.from s) "base64")))

(defn base64-decode [s]
  #?(:clj  (String. (.decode (Base64/getDecoder) (.getBytes s)))
     :cljs (base64/decodeString s)
     :node (.toString (js/Buffer.from s "base64"))))

(tests
  (base64-encode "hello world") := "aGVsbG8gd29ybGQ="
  (base64-decode "aGVsbG8gd29ybGQ=") := "hello world"

  (base64-encode (str (char 0x00))) := "AA=="
  (base64-decode "AA==") := (str (char 0x00))

  (base64-encode "C:\\") := "Qzpc"
  (base64-decode "Qzpc") := "C:\\"

  (base64-encode "{") := "ew=="
  (base64-decode "ew==") := "{"
  (base64-decode "ey") := "{") ; why is this the same

(defn str-replace-chars [s char-mapping]
  (reduce (fn [a [k v]] (clojure.string/replace a k v))
          s
          char-mapping))

(tests
  (str-replace-chars "hello world" {\l \X, \w \M}) := "heXXo MorXd")

(def url-unsafe {\+ \-
                 \/ \_
                 \= \,})

(defn base64-encode-url-safe [s] (str-replace-chars (base64-encode s) url-unsafe))
(defn base64-decode-url-safe [s] (base64-decode (str-replace-chars s (map-invert url-unsafe)) ))

(tests
  (base64-encode-url-safe "{") := "ew,,"
  (base64-decode-url-safe "ew,,") := "{"

  (base64-encode-url-safe "{--!.,@#$%^&*()") := "ey0tIS4sQCMkJV4mKigp"
  (base64-decode-url-safe "ey0tIS4sQCMkJV4mKigp") := "{--!.,@#$%^&*()"

  (base64-encode "+-") := "Ky0="
  (base64-encode-url-safe "+-") := "Ky0,"
  (base64-decode-url-safe "Ky0,") := "+-"
  (-> "+-" base64-encode-url-safe base64-decode-url-safe) := "+-")
