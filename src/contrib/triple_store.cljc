(ns ^:deprecated contrib.triple-store
  "Deprecated: moved to dustingetz.triple-store"
  (:refer-clojure :exclude [find key])
  (:require [dustingetz.triple-store]))

(def TripleStore dustingetz.triple-store/TripleStore)
(def ->TripleStore dustingetz.triple-store/->TripleStore)
(def map->TripleStore dustingetz.triple-store/map->TripleStore)
(def ->ts dustingetz.triple-store/->ts)
(def add dustingetz.triple-store/add)
(def del dustingetz.triple-store/del)
(def upd dustingetz.triple-store/upd)
(def asc dustingetz.triple-store/asc)
(def ->node dustingetz.triple-store/->node)
(def ? dustingetz.triple-store/?)
(def find dustingetz.triple-store/find)
(def find1 dustingetz.triple-store/find1)
(def key dustingetz.triple-store/key)
