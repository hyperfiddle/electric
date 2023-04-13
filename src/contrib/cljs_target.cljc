(ns contrib.cljs-target
  "Do not guard this require to cljs only, it contains clj macros. It is safe to 
require from clj and cljs due to careful consideration below."
  #?(:cljs (:require-macros [contrib.cljs-target]))
  ;(:require cljs.env) -- clojurescript must be on classpath during server macroexpansion (AOT or runtime depending on config)
  #?(:cljs (:require [goog.object :as object])))

; preferred runtime check for target through public API https://cljs.github.io/api/cljs.core/STARtargetSTAR
#?(:cljs (defn nodejs? [] (= cljs.core/*target* "nodejs")))
#?(:cljs (defn browser? [] (= cljs.core/*target* "default")))

;(defmacro do-cljs [& body] (when (some? (:js-globals &env)) `(do ~@body)))

; undocumented hack, only works in macros. https://stackoverflow.com/a/47499855
#?(:clj (defn- cljs-target []
          ; don't force app to :require clojurescript at runtime on the server
          ; (It's okay if you do, it just means clojurescript must be on server classpath)
          (let [compiler @(requiring-resolve 'cljs.env/*compiler*)]
            (get-in @compiler [:options :closure-defines 'cljs.core/*target*]))))

(defmacro do-nodejs  [& body] (if     (= "nodejs" (cljs-target)) `(do ~@body)))
(defmacro do-browser [& body] (if-not (= "nodejs" (cljs-target)) `(do ~@body)))
