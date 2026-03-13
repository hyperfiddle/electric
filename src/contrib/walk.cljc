(ns ^:deprecated contrib.walk
  "Deprecated: moved to dustingetz.walk"
  (:require [dustingetz.walk]))

(def has-meta? dustingetz.walk/has-meta?)
(def supports-with-meta? dustingetz.walk/supports-with-meta?)
(def walk dustingetz.walk/walk)
(def forward-metas dustingetz.walk/forward-metas)
(def prewalk dustingetz.walk/prewalk)
(def postwalk dustingetz.walk/postwalk)
