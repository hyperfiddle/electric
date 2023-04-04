(ns contrib.ednish
  (:require clojure.set
            clojure.string
            clojure.edn
            contrib.rfc3986
            [hyperfiddle.rcf :refer [tests]]))

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

(defn encode "Re-encode an edn-string to url-safe dialect of edn-ish. Vectors, sets and maps
coalesce into lists and are not disambiguated."
  [edn-str]
  (reduce (fn [a [k v]]
            (clojure.string/replace a k v))
          edn-str
          -edn-dialect-mappings))

(defn decode [ednish-str]
  (reduce (fn [a [k v]] (clojure.string/replace a k v))
          ednish-str
          (clojure.set/map-invert -edn-dialect-mappings)))

(tests
  (encode (pr-str :hyperfiddle.blog/post)) := ":hyperfiddle.blog!post"
  (encode (pr-str :a!b)) := ":a!b"
  (encode (pr-str "kobe")) := "'kobe'"
  (encode (pr-str #{"events" "news"})) := "~{'news','events'}"
  (encode (pr-str #uuid "07655f77-608d-472b-bc5e-86fcecc40b00"))
  := "~uuid,'07655f77-608d-472b-bc5e-86fcecc40b00'")

(def encode-uri (comp contrib.rfc3986/encode-pchar encode pr-str))
(def decode-uri (comp clojure.edn/read-string decode contrib.rfc3986/decode-pchar))

(tests
  "url encoding"
  (encode-uri "|") := "'%7C'"
  (decode-uri "'%7C'") := "|"

  (encode-uri "!$&'[]()*+,;=|") := "'!$&'()()*+,;=%7C'"
  ;(decode-uri "'!$&'()()*+,;=%7C'") := "!$&'()()*+,;=|" -- todo why broken?
  )

;(tests -- No reader function for tag uri -- this test passes in hf-2020
;  "ednish-tunneling"
;  (def v #uri "datomic:free://datomic:4334/~dustin.getz")
;  (def encoded (encode-ednish (pr-str v)))
;  (pr-str v) := _
;  (encode-ednish (pr-str v)) := "~uri,'datomic:free:!!datomic:4334!~dustin.getz'"
;  ;(is (= (decode-ednish encoded) (pr-str v)))
;  ; "#uri \"datomic:free://datomic:4334/#dustin.getz\""
;  )

(defn discard-leading-slash [path]
  (if (clojure.string/starts-with? path "/") (subs path 1) path))

(defn decode-path [path read-edn-str]
   {:pre [(string? path) (some? read-edn-str)]}
   (when-not (= path "/")
     (let [path (discard-leading-slash path)]
       (decode-uri path))))
