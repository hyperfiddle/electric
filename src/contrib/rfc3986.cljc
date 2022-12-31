(ns contrib.rfc3986
  (:require clojure.set
            [clojure.string :as string]
            [contrib.char$ :refer [char-code char->hex-str hex-str->char]]
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

(defn encode-pchar
  "percent-encode a url path segment without over-encoding (which many platform url decoders do).
  Notably, this impl is compatible with java.net.URI which fails on some chars in the 'unwise set',
  which are probably safe today."
  [s]
  (->> s
       (map (fn [c]
              (if (-pchar c) ; whitelist
                c
                (str "%" (char->hex-str c)))))
       (string/join)))

(defn decode-pchar [s]
  (-> (loop [decoded []
             [c & ss] s]
        (if-not c
          decoded ; done
          (if (= 37 (char-code c)) ; 37 is \% written portably
            (recur (conj decoded (hex-str->char (string/join (take 2 ss)))) (drop 2 ss))
            (recur (conj decoded c) ss))))
      string/join))

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
  ;(encode-rfc3986-pchar "위키백과:대문") := "%4%4%1%C:%0%8"
  ;(decode-rfc3986-pchar "%4%4%1%C:%0%8") := "위키백과:대문"
  ;((comp decode-rfc3986-pchar encode-rfc3986-pchar) "위키백과:대문")
  )
