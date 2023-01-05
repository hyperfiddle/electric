(ns contrib.cljs-target
  #?(:cljs (:require-macros [contrib.cljs-target]))
  #?(:cljs (:require [goog.object :as object])))

; preferred runtime check for target through public API https://cljs.github.io/api/cljs.core/STARtargetSTAR
#?(:cljs (defn nodejs? [] (= cljs.core/*target* "nodejs")))
#?(:cljs (defn browser? [] (= cljs.core/*target* "default")))

;(defmacro do-cljs [& body] (when (some? (:js-globals &env)) `(do ~@body)))

; undocumented hack, only works in macros. https://stackoverflow.com/a/47499855
#?(:clj (defn- cljs-target [] (get-in @cljs.env/*compiler* [:options :closure-defines 'cljs.core/*target*])))

(defmacro do-nodejs  [& body] (if     (= "nodejs" (cljs-target)) `(do ~@body)))
(defmacro do-browser [& body] (if-not (= "nodejs" (cljs-target)) `(do ~@body)))
