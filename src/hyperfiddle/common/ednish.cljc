(ns hyperfiddle.common.ednish
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]
    [edamame.core :refer [parse-string]]
    [hyperfiddle.common.base64 :as base64]
    [hyperfiddle.common.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar]]))


; https://tools.ietf.org/html/rfc2396#section-2.4.3
;
; unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"

(def -edn-dialect-mappings
  {\space \,
   \" \'
   \# \~
   \/ \!
   ;\{ \( \} \)
   \[ \( \] \)
   })

; Paste this into chrome and it will display properly
; https://ko.wikipedia.org/wiki/%EC%9C%84%ED%82%A4%EB%B0%B1%EA%B3%BC:%EB%8C%80%EB%AC%B8

(defn encode-ednish "Re-encode an edn-string to url-safe dialect of edn-ish. Vectors, sets and maps
coalesce into lists and are not disambiguated."
  [edn-str]
  (reduce (fn [a [k v]]
            (str/replace a k v))
          edn-str
          -edn-dialect-mappings))

(defn decode-ednish [ednish-str]
  (reduce (fn [a [k v]] (str/replace a k v))
          ednish-str
          (set/map-invert -edn-dialect-mappings)))

(def encode-uri (comp encode-rfc3986-pchar encode-ednish pr-str))
(def decode-uri (comp parse-string decode-ednish decode-rfc3986-pchar))


(defn default-query-encoder [key]
  [(encode-uri key)
   encode-uri
   ;; (comp base-64-url-safe/encode pr-str)
   (comp parse-string base64/decode-url-safe)])

(defn default-query-decoder [key]
  [(decode-uri key)
   (comp base64/encode-url-safe pr-str)
   decode-uri])

(defn url-encode [route]
  (let [[f & args] route]
    (str "/" (encode-uri f) "/"
         (some->> args
                  (map-indexed (fn [i arg]
                                 (let [[_sk encoder _decoder] (default-query-encoder i)]
                                   (encoder arg))))
                  (str/join "/")))))

(def url-regex #"^/([^/?#]+/?)*(\?[^#]*)?(#.*)?$")

(defn positional
  "Transform a map into a list. Expect keys to be 0,1,2,3… and contiguous.
  eg. {0 :foo, 1 :bar} => [:foo :bar]"
  [amap]
  (->> (range (inc (count amap)))
       (reduce (fn [acc idx]
                 (if (contains? amap idx)
                   (conj acc (get amap idx))
                   (reduced acc)))
               [])
       (seq)))

(defn url-decode [s]
  {:pre  [(string? s)]}
  (if-let [[match _ query hash] (re-find url-regex s)]
    (->> (cond-> match
           (seq hash)  (-> (str/split #"#") first)
           (seq query) (-> (str/split #"\?") first)
           true        (str/split #"/"))
         (rest)
         (map-indexed vector)
         (reduce (fn [acc [k v]]
                   (let [[_ _encoder decoder] (default-query-decoder v)]
                     (assoc acc k (decoder (or v "")))))
                 {})
         (positional))
    (throw (ex-info (str "Invalid URL " s) {:url s}))))

(deftest ednish-1 []
  (is (= (encode-ednish (pr-str :hyperfiddle.blog/post))
         ":hyperfiddle.blog!post"))
  (is (= (encode-ednish (pr-str "kobe"))
         "'kobe'"))
  (is (= (encode-ednish (pr-str #{"events" "news"}))
         "~{'news','events'}"))
  (is (= (encode-ednish (pr-str #uuid "07655f77-608d-472b-bc5e-86fcecc40b00"))
         "~uuid,'07655f77-608d-472b-bc5e-86fcecc40b00'"))
  )

#?(:clj
   (deftest ednish-tunneling []
     (def v #uri "datomic:free://datomic:4334/~dustin.getz")
     (def encoded (encode-ednish (pr-str v)))
     (pr-str v)
     (is (= (encode-ednish (pr-str v)) "~uri,'datomic:free:!!datomic:4334!~dustin.getz'"))
     ;; (is (= (decode-ednish encoded) (pr-str v)))
     ;; "#uri \"datomic:free://datomic:4334/#dustin.getz\""
     ))
