(ns contrib.uri
  (:require cognitect.transit
            clojure.edn
            [hyperfiddle.rcf :refer [tests]]))

; clojure code literals
; clojure runtime literals
; cljs code literals
; cljs runtime literals use EDN reader, because the js runtime is unsafe
; clojure.edn (clojure/script)
; tools.edn (clojure/script)

(defn print-uri [o w]
  (#?(:clj .write :cljs -write)
    w (str "#uri \""
           #?(:clj (.toString o) ; ?
              :cljs (.-uri-str o)) "\"")))

#?(:cljs (deftype URI [uri-str]
           Object (toString [_] uri-str)
           IPrintWithWriter (-pr-writer [o writer _] (print-uri o writer) #_(-write writer (uri-pr-str o)))
           IHash (-hash [this] (hash uri-str))
           IEquiv (-equiv [this other] (and (instance? URI other) (= (.-uri-str this) (.-uri-str other))))
           IComparable (-compare [this other] (and (instance? URI other) (compare (.-uri-str this) (.-uri-str other))))))

(defn is-uri? [o] #?(:clj (or (instance? java.net.URI o)
                              (instance? com.cognitect.transit.URI o))
                     :cljs (instance? URI o)))

; for data_readers.cljc
(defn uri-clj-reader [s] (java.net.URI. s)) ; for data_readers.cljc
(defn uri-cljs-reader [s] #?(:clj `(contrib.uri/->URI ~s))) ; called from cljs compiler jvm
; https://github.com/clojure/clojurescript/commit/5379f722588370f9f1934e9a78e777e24e953c81

(tests
  "#uri code literals are auto-wired from data_readers.cljc"
  (def x #uri "http://localhost:8080/a?b#c")
  x := #?(:clj (java.net.URI. "http://localhost:8080/a?b#c")
          :cljs (->URI "http://localhost:8080/a?b#c"))

  (is-uri? x) := true)

#?(:clj (defmethod print-method java.net.URI [o ^java.io.Writer w] (print-uri o w)))
#?(:clj (defmethod print-dup java.net.URI [o ^java.io.Writer w] (print-uri o w)))
#?(:clj (defmethod print-method com.cognitect.transit.URI [o ^java.io.Writer w] (print-uri o w)))
#?(:clj (defmethod print-dup com.cognitect.transit.URI [o ^java.io.Writer w] (print-uri o w)))

(tests
  "#uri edn serialization (calls the Writer interfaces)"
  (pr-str x) := "#uri \"http://localhost:8080/a?b#c\"")

(tests
  "#uri runtime literal readers are auto-wired in clojure from data_readers.cljc"
  #?@(:clj ((clojure.core/read-string "#uri \"http://localhost:8080/a?b#c\"") := x))

  "#uri runtime literals are NOT auto-wired in cljs reader (the cljs JS runtime reader is always an
  EDN reader for safety, unlike clojure)"
  ; https://www.clojurescript.org/guides/reader
  #?@(:cljs ((cljs.reader/read-string "#uri \"http://localhost:8080/a?b#c\"")
             :throws js/Error ; No reader function for tag uri.
             (contains? (set (keys @cljs.reader/*tag-table*)) 'inst) := true
             (contains? (set (keys @cljs.reader/*tag-table*)) 'uri) := false)))

(tests
  "control - clojure.edn"
  #?@(:clj ((clojure.edn/read-string "1") := 1)
      :cljs ((clojure.edn/read-string "1") := 1))

  "edn readers don't auto-wire the unsafe code literals"
  #?@(:clj ((clojure.edn/read-string "#uri \"http://localhost:8080/a?b#c\"") :throws RuntimeException)
      :cljs ((clojure.edn/read-string "#uri \"http://localhost:8080/a?b#c\"") :throws js/Error)))

(tests
  "#uri direct edn reader configuration - different for clj and cljs"
  #?@(:clj ((clojure.edn/read-string {:readers {'uri #(java.net.URI. %)}} "#uri \"http://localhost:8080/a?b#c\"") := x)
      :cljs ((clojure.edn/read-string {:readers {'uri ->URI}} "#uri \"http://localhost:8080/a?b#c\"") := x)))

(comment
  (tests
    "cljs userland can globally register an EDN tag reader, but it's probably a bad idea"
    #?@(:cljs ((cljs.reader/register-tag-parser! 'uri ->URI)
               (clojure.edn/read-string "#uri \"http://localhost:8080/a?b#c\"") := x))))

(tests
  "clj userland can not globally register an EDN tag reader"
  #?@(:clj ((clojure.edn/read-string "#uri \"http://localhost:8080/a?b#c\"") :throws RuntimeException)))

; for easier config – optional
(defn uri-edn-reader [s] #?(:clj (java.net.URI. s) ; careful, don't call this from the cljs compiler
                            :cljs (->URI s)))

(tests
  "#uri easy config – same for clj and cljs"
  #?@(:clj ((clojure.edn/read-string {:readers {'uri uri-edn-reader}} "#uri \"http://localhost:8080/a?b#c\"") := x)
      :cljs ((clojure.edn/read-string {:readers {'uri uri-edn-reader}} "#uri \"http://localhost:8080/a?b#c\"") := x)))
