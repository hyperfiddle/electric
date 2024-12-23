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
  [[\space \,]
   [\# \~]
   [\' \’] ; \u2019 – typographic apostrophe (vs typewriter). URL safe and eye friendly. Prevents clashes with `"`.
   [\" \']
   [\/ \!]
   ;; \{ \( \} \)
   [\[ \(]
   [\] \)]
   ])

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
          (map (fn [[k v]] [v k]) (reverse -edn-dialect-mappings))))

(tests
  (encode (pr-str :hyperfiddle.blog/post)) := ":hyperfiddle.blog!post"
  (encode "pour l'amour") := "pour,l’amour"
  (encode "pour l’amour") := "pour,l’amour" ; note the apostrophe
  (decode (encode "pour l'amour")) := "pour l'amour"
  (decode (encode "pour l’amour")) := "pour l'amour" ; typographic apostrophe replaced by typewriter apostrophe, DBs collates them anyway, should be harmless.
  (encode (pr-str :a!b)) := ":a!b"
  (encode (pr-str "kobe")) := "'kobe'"
  (encode (pr-str #{"events" "news"})) := "~{'news','events'}"
  (encode (pr-str #uuid "07655f77-608d-472b-bc5e-86fcecc40b00"))
  := "~uuid,'07655f77-608d-472b-bc5e-86fcecc40b00'")

(defn encode-uri [x]
  (binding [*print-namespace-maps* true] ; unify platform default settings for more compact URLs. Defaults to: true in clj, false in cljs.
    (-> x pr-str encode contrib.rfc3986/encode-pchar)))

(def decode-uri (comp clojure.edn/read-string decode contrib.rfc3986/decode-pchar))

(tests
  "url encoding"
  (encode-uri "|") := "'%7C'"
  (decode-uri "'%7C'") := "|"

  (encode-uri "!") := "'!'"
  (decode-uri "'!'") := "/" ; Exclamation mark clashes with /

  (encode-uri "$&'[]()*+,;=|") := "'$&’()()*+,;=%7C'"
  (decode-uri "'$&’()()*+,;=%7C'") := "$&'[][]*+ ;=|"

  (encode-uri `(Toggle)) := "(contrib.ednish!Toggle)"
  (decode-uri "(contrib.ednish!Toggle)") := '[contrib.ednish/Toggle]

  (encode-uri {:foo :bar}) := "%7B:foo,:bar%7D"
  (encode-uri {::foo :bar, :baz :asdf})  := "%7B:contrib.ednish!foo,:bar,,:baz,:asdf%7D"
  (encode-uri {::foo :bar, ::baz :asdf}) := "~:contrib.ednish%7B:foo,:bar,,:baz,:asdf%7D"
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
