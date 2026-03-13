(ns ^:deprecated contrib.electric-codemirror
  "Deprecated: moved to dustingetz.electric-codemirror"
  (:require [dustingetz.electric-codemirror]))

#?(:cljs (def theme dustingetz.electric-codemirror/theme))
#?(:cljs (def inline-extensions dustingetz.electric-codemirror/inline-extensions))
#?(:cljs (def make-state dustingetz.electric-codemirror/make-state))
#?(:cljs (def make-cm! dustingetz.electric-codemirror/make-cm!))
#?(:cljs (def cm-set! dustingetz.electric-codemirror/cm-set!))
#?(:cljs (def codemirror dustingetz.electric-codemirror/codemirror))
(def CodeMirror dustingetz.electric-codemirror/CodeMirror)
(def read-edn dustingetz.electric-codemirror/read-edn)
(def write-edn dustingetz.electric-codemirror/write-edn)
(def Edn dustingetz.electric-codemirror/Edn)
(def String_ dustingetz.electric-codemirror/String_)
