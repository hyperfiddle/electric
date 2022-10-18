(ns contrib.datomic-cloud-contrib
  (:require clojure.edn
            [hyperfiddle.rcf :refer [tests]])
  (:import [goog.math Long]))

; Issue: Datomic Cloud db/ids (java.lang.Longs) can exceed the maximum integer representable in
; javascript's Number type which is a double:

(tests
  "javascript platform number type rounds large longs when they exceed what double precision
  can represent exactly"
  (def js-max-int 9007199254740991)
  (= js-max-int (+ 1 js-max-int)) := false ; correct
  (= (+ 1 js-max-int) (+ 2 js-max-int)) := true ; yikes!
  (= 9007199254740992 9007199254740993) := true ; yikes!

  (->> [9007199254740989
        9007199254740990
        9007199254740991
        9007199254740992
        9007199254740993
        9007199254740994
        9007199254740995
        9007199254740996]
       (map (partial = 9007199254740992)))
  := [false false false
      true true ; yikes!
      false false false])

; transit-js works around this by reading too-large numbers as goog.math.Long:
; https://github.com/cognitect/transit-js/blob/9c28b4d9afaddae3cf15073296fcd736f68600a9/src/com/cognitect/transit/types.js#L82-L99

(tests
  "Transit-js workaround is to use object longs instead"
  (def xs (map goog.math.Long/fromString
               ["9007199254740989"
                "9007199254740990"
                "9007199254740991"
                "9007199254740992"
                "9007199254740993"
                "9007199254740994"
                "9007199254740995"
                "9007199254740996"]))
  (map (partial = 9007199254740992) xs)
  := [false false false false false false false false])

; EDN serializers must encode the value as strings, because by the time the cljs reader sees a
; value, it has already been ready by javascript and damaged.

(extend-type goog.math.Long
  IPrintWithWriter
  (-pr-writer [^goog.math.Long o writer _] (-write writer (str "#goog.math/Long \"" (.toString o) "\""))))

(tests
  "edn writer"
  (def too-big (goog.math.Long/fromString "9007199254740992"))
  (pr-str too-big) := "#goog.math/Long \"9007199254740992\""

  (->> ["9007199254740989"
        "9007199254740990"
        "9007199254740991"
        "9007199254740992"
        "9007199254740993"
        "9007199254740994"
        "9007199254740995"
        "9007199254740996"]
       (map (comp pr-str goog.math.Long/fromString)))
  := ["#goog.math/Long \"9007199254740989\""
      "#goog.math/Long \"9007199254740990\""
      "#goog.math/Long \"9007199254740991\""
      "#goog.math/Long \"9007199254740992\""
      "#goog.math/Long \"9007199254740993\""
      "#goog.math/Long \"9007199254740994\""
      "#goog.math/Long \"9007199254740995\""
      "#goog.math/Long \"9007199254740996\""])

(tests
  (cljs.reader/read-string "#goog.math/Long \"9007199254740996\"")
  :throws js/Error ; No reader function for tag long.

  (def x (cljs.reader/read-string {:readers {'goog.math/Long goog.math.Long/fromString}} "#goog.math/Long \"9007199254740996\""))
  x := (goog.math.Long/fromString "9007199254740996")

  ; x := #goog.math/Long"9007199254740996" -- compiler error
  ; this is runtime only! Is there a business case for hardcoding large literals like that?

  "you can inject something like this over a region (to avoid global state)"
  ; yes we know bindings are pretty busted in clojure, but it beats mutable global state
  (def ^:dynamic *read-str* nil)
  (binding [*read-str* (partial cljs.reader/read-string {:readers {'goog.math/Long goog.math.Long/fromString}})]
    (*read-str* "#goog.math/Long \"9007199254740996\"")) := x)

(tests
  "edn reader"
  (cljs.reader/read-string "#goog.math/Long \"9007199254740996\"")
  :throws js/Error ; No reader function for tag long.

  (clojure.edn/read-string {:readers {'goog.math/Long goog.math.Long/fromString}} "#goog.math/Long \"9007199254740996\"")
  := x

  (def ^:dynamic *read-str* nil)
  (binding [*read-str* (partial clojure.edn/read-string {:readers {'goog.math/Long goog.math.Long/fromString}})]
    (*read-str* "#goog.math/Long \"9007199254740996\"")) := x)

; For the Clojure reader case, I think we don't need to support large compile-time literal longs,
; this is a runtime concern only and thus we only implement the EDN readers in clojurescript.
