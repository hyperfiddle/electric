(ns hyperfiddle.electric.impl.cljs-file-to-analyze.require
  #?(:cljs (:require-macros hyperfiddle.electric.impl.cljs-file-to-analyze.require)))

(defmacro macrodef [sym] `(def ~sym))
(defmacro refdef [sym] `(def ~sym))
(defmacro renameme [sym] `(def ~sym))
