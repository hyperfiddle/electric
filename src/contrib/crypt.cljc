(ns contrib.crypt
  (:require #?(:cljs [goog.crypt :as crypt])
            #?(:cljs [goog.crypt.Sha256])
            #?(:cljs [goog.crypt.base64 :as base64])
            [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
            [contrib.base64])
  (:import #?(:clj [java.util Base64]))
  #?(:cljs (:require-macros contrib.crypt)))

(defn sha256-base64 [o]
  #?(:clj
     (let [sha256er (java.security.MessageDigest/getInstance "SHA-256")]
       (String. (->> (.getBytes (pr-str o) "UTF-8") (.digest sha256er) (.encode (Base64/getEncoder))) "UTF-8"))
     :cljs
     (let [sha256er (goog.crypt.Sha256.)]
       (.update sha256er (goog.crypt.stringToByteArray (pr-str o)))
       (goog.crypt.base64/encodeByteArray (.digest sha256er)))))

(tests
  (string? (sha256-base64 [:x "y"])) := true
  (string? (sha256-base64 nil))      := true)
