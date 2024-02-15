(ns hyperfiddle.electric.impl.cljs-file-to-analyze
  "docstring" {:attr :map}
  (:require
   [hyperfiddle.electric.impl.cljs-file-to-analyze.require :as req :refer [refdef renameme] :rename {renameme renamed}]
   [hyperfiddle.electric.impl.cljs-file-to-analyze.include :as inc :include-macros true]
   [hyperfiddle.electric.impl.cljs-file-to-analyze.refer-macros :refer-macros [refmac]])
  (:require-macros [hyperfiddle.electric.impl.cljs-file-to-analyze.macro-ns :as reqmac :refer [reqmacrefer reqmacrename] :rename {reqmacrename reqmacrenamed}])
  (:use [hyperfiddle.electric.impl.cljs-file-to-analyze.use :only [useme renameme] :rename {renameme use-renamed}])
  (:use-macros [hyperfiddle.electric.impl.cljs-file-to-analyze.use-macros :only [useme-mac renameme-mac] :rename {renameme-mac use-renamed-mac}])
  (:refer-clojure :exclude [first] :rename {next nxt}))

(def foo 1)

(do (def bar 2) (def baz 3))

(do (defn an-fn []))

(hyperfiddle.electric.impl.cljs-file-to-analyze.require/macrodef behind-require)
(req/macrodef behind-alias)
(hyperfiddle.electric.impl.cljs-file-to-analyze.macro-ns/reqmacrodef behind-require-macros)
(reqmac/reqmacrodef behind-require-macro-alias)
(refdef behind-required-refer)
(renamed behind-required-rename)
(reqmacrefer behind-require-macro-refer)
(reqmacrenamed behind-require-macro-rename)
(inc/include behind-include-macros)
(refmac behind-refer-macros)
(useme behind-use)
(use-renamed behind-use-renamed)
(useme-mac behind-use-macro)
(use-renamed-mac behind-use-macro-renamed)
