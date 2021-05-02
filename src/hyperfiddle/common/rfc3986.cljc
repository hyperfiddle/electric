(ns hyperfiddle.common.rfc3986
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [hyperfiddle.common.char :refer [char-code char->hex-str hex-str->char]]
    [clojure.test :refer [deftest is]]))


; https://tools.ietf.org/html/rfc3986#appendix-A
;
; pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
; unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
; sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="

(def -sub-delims #{\! \$ \& \' \( \) \* \+ \, \; \=})

(def -unreserved (set/union (set (map char "abcdefghijklmnopqrstuvwxyz"))
                            (set (map char "0123456789"))
                            #{\- \. \_ \~}))

(def -pchar (set/union -unreserved -sub-delims #{\: \@}))

(defn encode-rfc3986-pchar "precent-encode a url path segment without over-encoding. The usual platform url decoders
over-encode by a lot. Notably, this is compatible with java.net.URI which fails on some chars in the 'unwise set', which
are probably safe today."
  [s]
  (->> s
       (map (fn [c]
              (if (-pchar c)                                ; whitelist
                c
                (str "%" (char->hex-str c)))))
       (string/join)))

(defn decode-rfc3986-pchar [s]
  (-> (loop [decoded []
             [c & ss] s]
        (if-not c
          decoded                                           ; done
          (if (= 37 (char-code c))                          ; 37 is \% written portably
            (recur (conj decoded (hex-str->char (string/join (take 2 ss)))) (drop 2 ss))
            (recur (conj decoded c) ss))))
      string/join))

(deftest url-encode-1 []
  (is (= (encode-rfc3986-pchar "google-oauth2|116635422485042503270")
         "google-oauth2%7C116635422485042503270")))

(deftest url-decode-1 []
  (is (= (decode-rfc3986-pchar "google-oauth2%7C116635422485042503270")
         "google-oauth2|116635422485042503270")))

(deftest url-biject-1 []
  (is (= ((comp decode-rfc3986-pchar encode-rfc3986-pchar) "google-oauth2|116635422485042503270") "google-oauth2|116635422485042503270"))
  (is (= ((comp decode-rfc3986-pchar encode-rfc3986-pchar) "google-oauth2|116635422485042503270") "google-oauth2|116635422485042503270"))
  )

(deftest unicode-1 []
  ;; (is (= ((comp decode-rfc3986-pchar encode-rfc3986-pchar) "위키백과:대문")))
  )
