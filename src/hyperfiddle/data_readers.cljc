(ns hyperfiddle.data-readers
  (:refer-clojure :exclude [read-string])
  #?(:cljs (:require [goog.math.Long :as gl]))
  #?(:clj (:import [java.lang Long])))

(defn long-edn-reader [s]
  ; #long "65332980922449989"
  ; Wrapped in string because the tag operates on a processed platform value
  ; (so Javascript has already damaged the long)
  #?(:clj (Long/parseLong s))
  #?(:cljs (gl/fromString s)))

#?(:cljs
   (defn- long-edn-writer ^String [^goog.math.Long o]
     (str "#long " (pr-str (.toString o)))))

#?(:cljs
   (extend-type goog.math.Long
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (long-edn-writer o)))))
