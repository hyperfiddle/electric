(ns contrib.rfc3986
  (:require clojure.set
            [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]]))

; https://tools.ietf.org/html/rfc3986#appendix-A
;
; pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
; unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
; sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="

(def -sub-delims #{\! \$ \& \' \( \) \* \+ \, \; \=})
(def -unreserved (clojure.set/union (set (map char "abcdefghijklmnopqrstuvwxyz"))
                                    (set (map char "0123456789"))
                                    #{\- \. \_ \~}))
(def -pchar (clojure.set/union -unreserved -sub-delims #{\: \@}))

(defn char->hex-str [c]
  (str/upper-case #?(:clj (java.net.URLEncoder/encode (str c))
                     :cljs (js/encodeURIComponent c))))

(defn encode-pchar
  "percent-encode a url path segment without over-encoding (which many platform url decoders do).
  Notably, this impl is compatible with java.net.URI which fails on some chars in the 'unwise set',
  which are probably safe today."
  [s]
  (->> s
       (map (fn [c]
              (if (-pchar c) ; whitelist
                c
                (char->hex-str c))))
       (str/join)))

(defn hex-str->char [str]
  #?(:clj (java.net.URLDecoder/decode str)
     :cljs (js/decodeURIComponent str)))

(defn tokens [str]
  (map first (re-seq #"((%[0123456789ABCDEF]{2})+|[^%]+)" str)))

(defn parse-token [str]
  (if (str/starts-with? str "%")
    (hex-str->char str)
    str))

(defn decode-pchar [str] (str/join (map parse-token (tokens str))))

(tests
  "pchar encoding and decoding"
  (encode-pchar "google-oauth2|116635422485042503270")
  := "google-oauth2%7C116635422485042503270"

  (decode-pchar "google-oauth2%7C116635422485042503270")
  := "google-oauth2|116635422485042503270"

  "non-ascii characters"
  (encode-pchar "!$&'[]()*+,;=|") := "!$&'%5B%5D()*+,;=%7C"
  (decode-pchar "!$&'%5B%5D()*+,;=%7C") := "!$&'[]()*+,;=|"

  "bijection"
  ((comp decode-pchar encode-pchar) "google-oauth2|116635422485042503270")
  := "google-oauth2|116635422485042503270"

  ;"unicode" ; broken, not sure why
  (encode-pchar "위키백과:대문")
   := "%EC%9C%84%ED%82%A4%EB%B0%B1%EA%B3%BC:%EB%8C%80%EB%AC%B8"
   (decode-pchar "%EC%9C%84%ED%82%A4%EB%B0%B1%EA%B3%BC:%EB%8C%80%EB%AC%B8") := "위키백과:대문"
  ((comp decode-pchar encode-pchar) "위키백과:대문")
  )
