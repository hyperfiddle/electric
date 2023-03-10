(ns contrib.uri
  (:require clojure.edn
            #?(:cljs goog.Uri)
            [hyperfiddle.rcf :refer [tests]]))

;(defmacro tests [& body])

; clojure.core/uri? builtin hardcodes java.net.URI and goog.Uri
; transit-java, transit-js – OOTB read as dummy URI type (concrete type is deferred to language impl per transit design)
; transit-clj, transit-cljs – OOTB read as dummy URI type!! they do not align to clojure.core/uri? ootb!

(comment
  ; tests disabled because once this extension ns is loaded, they no longer pass
  ; (and cljs will auto-load this ns due to data_loaders.cljc)
  #?(:clj (tests
            "document java.net.URI OOTB behavior"
            (def jx (java.net.URI. "http://localhost:8080/a?b#c"))
            (str jx) := "http://localhost:8080/a?b#c"
            (pr-str jx) := "#object[java.net.URI 0x5cbf50ff \"http://localhost:8080/a?b#c\"]"
            (uri? jx) := true
            (= (java.net.URI. "http://localhost:8080/a?b#c")
               (java.net.URI. "http://localhost:8080/a?b#c")) := true
            ;(= #user/uri "http://localhost:8080/a?b#c" jx) := true -- compiler error
            (= (read-string "#user/uri \"http://localhost:8080/a?b#c\"") jx) :throws Exception)
     :cljs (tests
             "document goog.Uri OOTB behavior"
             (def gx (goog.Uri. "http://localhost:8080/a?b#c"))
             (str gx) := "http://localhost:8080/a?b#c" ; str repr
             (pr-str gx) := "#object[Object http://localhost:8080/a?b#c]" ; needs edn extension
             (uri? gx) := true
             (= (goog.Uri. "http://localhost:8080/a?b#c")
                (goog.Uri. "http://localhost:8080/a?b#c")) := false
             ;(= #user/uri "http://localhost:8080/a?b#c" jx) := true -- compiler error
             (= (cljs.reader/read-string "#user/uri \"http://localhost:8080/a?b#c\"") jx) :throws js/Error)))

; clojure code literals
; clojure runtime literals
; cljs code literals
; cljs runtime literals use EDN reader, because the js runtime is unsafe
; clojure.edn (clojure/script)
; tools.edn (clojure/script)

; We decline to implement the transit dummy URI types, see https://github.com/cognitect/transit-cljs/issues/16
; https://github.com/cognitect/transit-clj/blob/master/src/cognitect/transit.clj
; https://github.com/cognitect/transit-cljs/blob/master/src/cognitect/transit.cljs
; https://github.com/cognitect/transit-java/blob/master/src/main/java/com/cognitect/transit/URI.java
; https://github.com/cognitect/transit-js/blob/9c28b4d9afaddae3cf15073296fcd736f68600a9/src/com/cognitect/transit/types.js#L36

(defn print-uri [o w]
  {:pre [(uri? o)]}
  (let [str-rep (cond
                  #?@(:clj ((instance? java.net.URI o) (str o))
                      :cljs ((instance? goog.Uri o) (str o))))]
    (#?(:clj .write :cljs -write) w (str "#user/uri \"" str-rep "\""))))

; FAQ: The reader docs say that custom literals should be namespaced to avoid name collisions with
; core, why is `uri` unqualified here? https://clojure.org/reference/reader
; A: we think this should be in core, in alignment with clojure.core/uri? being hardcoded to
; java.net.URI and goog.Uri, they are not userland types.

#?(:cljs (extend-type goog.Uri IPrintWithWriter (-pr-writer [o writer _] (print-uri o writer))))
#?(:clj (defmethod print-method java.net.URI [o ^java.io.Writer w] (print-uri o w)))
#?(:clj (defmethod print-dup java.net.URI [o ^java.io.Writer w] (print-uri o w)))
;#?(:clj (defmethod print-method com.cognitect.transit.URI [o ^java.io.Writer w] (print-uri o w)))

(tests
  (def x #?(:clj (java.net.URI. "http://localhost:8080/a?b#c")
            :cljs (goog.Uri. "http://localhost:8080/a?b#c")))
  (str x) := "http://localhost:8080/a?b#c" ; unchanged
  (pr-str x) := "#user/uri \"http://localhost:8080/a?b#c\"")

; for data_readers.cljc
#?(:clj (defn uri-clj-reader [s] (java.net.URI. s)))
#?(:clj (defn uri-cljs-reader [s] `(goog.Uri. ~s))) ; called from cljs compiler jvm
; https://github.com/clojure/clojurescript/commit/5379f722588370f9f1934e9a78e777e24e953c81

;; (tests
;;   "#user/uri code literals are auto-wired from data_readers.cljc" ; data_readers is not part of contrib
;;   (def x #user/uri "http://localhost:8080/a?b#c")
;;   (uri? x) := true
;;   (str x) := "http://localhost:8080/a?b#c"
;;   (pr-str x) := "#user/uri \"http://localhost:8080/a?b#c\""

;;   "note goog.Uri is mutable, so no equality in cljs"
;;   (= #user/uri "http://localhost:8080/a?b#c"
;;      #user/uri "http://localhost:8080/a?b#c") := #?(:clj true :cljs false))


; Readers

(tests
  "clj #user/uri runtime literal readers are auto-wired in clojure from data_readers.cljc"
  #?@(:clj ((clojure.core/read-string "#user/uri \"http://localhost:8080/a?b#c\"") := x))

  "cljs #user/uri runtime literals are NOT auto-wired in cljs reader (the cljs JS runtime reader is always an
  EDN reader for safety, unlike clj)"
  ; https://www.clojurescript.org/guides/reader
  #?@(:cljs ((cljs.reader/read-string "#user/uri \"http://localhost:8080/a?b#c\"")
             :throws js/Error ; No reader function for tag uri.
             (contains? (set (keys @cljs.reader/*tag-table*)) 'inst) := true
             (contains? (set (keys @cljs.reader/*tag-table*)) 'uri) := false)))

(tests
  "control - clojure.edn"
  #?@(:clj ((clojure.edn/read-string "1") := 1)
      :cljs ((clojure.edn/read-string "1") := 1))

  "edn readers don't auto-wire the unsafe code literals"
  #?@(:clj ((clojure.edn/read-string "#user/uri \"http://localhost:8080/a?b#c\"") :throws RuntimeException)
      :cljs ((clojure.edn/read-string "#user/uri \"http://localhost:8080/a?b#c\"") :throws js/Error)))

(tests
  "#user/uri direct edn reader configuration - different for clj and cljs"
  (def edn-read-str (partial clojure.edn/read-string {:readers {'user/uri #?(:clj #(java.net.URI. %)
                                                                             :cljs #(goog.Uri. %))}}))
  (edn-read-str "#user/uri \"http://localhost:8080/a?b#c\"")
  (uri? *1) := true)

(comment
  (tests
    "cljs userland can globally register an EDN tag reader, but it's probably a bad idea"
    #?@(:cljs ((cljs.reader/register-tag-parser! 'uri #(goog.Uri. %))
               (clojure.edn/read-string "#user/uri \"http://localhost:8080/a?b#c\"") := x))))

(tests
  "clj userland can not globally register an EDN tag reader"
  #?@(:clj ((clojure.edn/read-string "#user/uri \"http://localhost:8080/a?b#c\"") :throws RuntimeException)))

; for easy portable config – optional
;(defn uri-edn-reader [s] #?(:clj (java.net.URI. s) :cljs (goog.Uri. s)))
;
;(tests
;  "#user/uri easy portable config – same for clj and cljs"
;  (clojure.edn/read-string {:readers {'uri uri-edn-reader}} "#user/uri \"http://localhost:8080/a?b#c\"")
;  (uri? *1) := true)
