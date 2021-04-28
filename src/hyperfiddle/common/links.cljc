(ns hyperfiddle.common.links
  (:require [clojure.edn :as edn]))

(defn sexp->link [sexp] (pr-str sexp))

(defn link->sexp [str]
  (edn/read-string str))
