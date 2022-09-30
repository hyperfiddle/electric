(ns hyperfiddle.data-readers
  ;(:refer-clojure :exclude [read-string])
  (:require [hyperfiddle.rcf :refer [tests]]
            #?(:cljs goog.math))
  (:import #?(:clj [java.lang Long]
              :cljs [goog.math Long])))

(def js-max-int 9007199254740991) ; https://github.com/cognitect/transit-js/blob/9c28b4d9afaddae3cf15073296fcd736f68600a9/src/com/cognitect/transit/types.js#L92-L94
;(def n 15846161579653706)
;15846161579653706 ; datomic cloud ids can be larger

; https://www.clojurescript.org/about/differences
; The Clojure compiler does not automatically require reader functions referred to in data_readers.clj/c,
; but the Clojurescript compiler does.

(defn long-edn-reader [s]
  ; #long "65332980922449989"
  ; Wrapped in string because the tag operates on a processed platform value
  ; (so Javascript has already damaged the long)
  #?(:clj (java.lang.Long/parseLong s)
     :cljs (goog.math.Long/fromString s)))

; #long is not useful for literals, only for reading runtime EDN IDs from Datomic Cloud – todo disable it

(defn long-clj-reader [s] (java.lang.Long/parseLong s))

(defn long-cljs-reader [s]
  ; https://github.com/clojure/clojurescript/commit/5379f722588370f9f1934e9a78e777e24e953c81#diff-0e452eb09452a0bc85b30bcca5e1f7cfbc3f880661dfa22540ac8d781888e4ee
  #?(:cljs (goog.math.Long/fromString s) ; cljs runtime literals via cljs.reader/read-string
     :clj `(goog.math.Long/fromString ~s))) ; cljs compile-time literals

(tests
  "#long clojure/script compile-time reader"
  (def y #long "15846161579653706") ; in the JVM the #long adds nothing; in cljs it forces a goog.math.Long wrapper
  y := #?(:clj (java.lang.Long/parseLong "15846161579653706")
          :cljs (goog.math.Long/fromString "15846161579653706"))

  "in cljs, literal 1 and #long 1 are not the same!"
  y := #?(:clj 15846161579653706 :cljs (goog.math.Long/fromString "15846161579653706")))

#?(:cljs (defn- long-edn-writer ^String [^goog.math.Long o] (str "#long \"" (.toString o) "\"")))

#?(:cljs
   (extend-type goog.math.Long
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (long-edn-writer o)))))

(tests
  "#long runtime pr-str"
  (pr-str 1) := #?(:clj "1" :cljs "1") ; control
  (pr-str y) := #?(:clj "15846161579653706" :cljs "#long \"15846161579653706\""))

(tests
  "#long runtime read-string"
  #?@(:clj ((clojure.core/read-string "#long \"15846161579653706\"") := 15846161579653706)

      ; the following should work but doesn't; why doesn't cljs see data_readers.cljc at runtime?
      ; actual: #error {:message "No reader function for tag long.", :data {:type :reader-exception, :ex-kind :reader-error}}
      :cljs ((cljs.reader/read-string "#long \"15846161579653706\"") :throws js/Error #_(goog.math.Long/fromString "15846161579653706"))))

(comment
  ; https://www.clojurescript.org/guides/reader
  @cljs.reader/*tag-table* ; no #long, wtf
  )
