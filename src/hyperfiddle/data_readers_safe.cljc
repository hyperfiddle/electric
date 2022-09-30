(ns hyperfiddle.data-readers-safe
  ;(:refer-clojure :exclude [read-string])
  (:require [hyperfiddle.rcf :refer [tests]]
            #?(:cljs goog.math))
  (:import #?(:clj [java.lang Long]
              :cljs [goog.math Long])))

; Issue: Datomic Cloud db/ids (java.lang.Longs) can exceed the maximum integer representable in
; javascript's Number type (double precision). Transit-js will read too-large numbers as
; goog.math.Long.
;
; For the EDN case, CLJS reader sees all values in the platform number type, which means the js
; platform has already damaged the number by rounding it to the closest representable double,
; so edn serializers must encode large numbers as strings.
;
; For the Clojure reader case, I think we don't need to support large compile-time literal longs,
; this is a runtime concern only and thus we only implement the EDN readers here.

(defn long-edn-reader [s]
  #?(:clj (java.lang.Long/parseLong s)
     :cljs (goog.math.Long/fromString s)))

#?(:cljs (defn- long-edn-writer ^String [^goog.math.Long o] (str "#long \"" (.toString o) "\"")))

#?(:cljs
   (extend-type goog.math.Long
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (long-edn-writer o)))))

(tests
  (def js-max-int 9007199254740991) ; https://github.com/cognitect/transit-js/blob/9c28b4d9afaddae3cf15073296fcd736f68600a9/src/com/cognitect/transit/types.js#L82-L99
  (pr-str js-max-int) := #?(:clj "9007199254740991"
                            :cljs "9007199254740991")

  (def too-big (long-edn-reader "9007199254740992"))
  (pr-str too-big)
  := #?(:clj "9007199254740992"
        :cljs "#long \"9007199254740992\""))
