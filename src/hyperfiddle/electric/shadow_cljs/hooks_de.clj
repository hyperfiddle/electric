(ns hyperfiddle.electric.shadow-cljs.hooks-de
  (:require [shadow.build.compiler]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as cljs-ana]))

;; Shadow-cljs doesn't expose a way to act before compiling a cljs file.
;; It filters resources in a series of functions, calling `do-compile-cljs-resource` in the end.
;; So we wrap this final step and alter the var.
(defonce original-do-compile-cljs-resource shadow.build.compiler/do-compile-cljs-resource)
(def !built-this-cycle (atom #{}))      ; build once per cycle
(defonce first-compile? true)           ; on first compile we don't need to recompile
(defn wrapped-do-compile-cljs-resource [state {ns$ :ns :as rc} source]
  (swap! lang/!a cljs-ana/purge-ns ns$)
  (when (and (not (@!built-this-cycle ns$)) (some-> (find-ns ns$) meta ::lang/has-edef?))
    (prn ::recompile-clj ns$)
    (require ns$ :reload))
  (original-do-compile-cljs-resource state rc source))

(defn reload-clj "On `e/defn` change, recompile Clojure namespace (because the expression
  may contain e/client and/or e/server). Prevents double-reloads (e.g. from :require-macros)."
  {:shadow.build/stage :compile-finish} [build-state]
  (when first-compile?
    (alter-var-root #'first-compile? not)
    (alter-var-root #'shadow.build.compiler/do-compile-cljs-resource (constantly #'wrapped-do-compile-cljs-resource)))
  (reset! !built-this-cycle #{})
  build-state)
