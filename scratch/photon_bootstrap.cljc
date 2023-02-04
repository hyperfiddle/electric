(ns hyperfiddle.photon-bootstrap
  (:refer-clojure :exclude [def fn defn for])
  (:require [clojure.core :as cc]
            [hyperfiddle.photon-impl.compiler :as c]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])     ; todo remove
            #?(:cljs [hyperfiddle.photon-client]))
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn for]])))


#?(:clj
   (do
     ; Optionally, tell RCF not to rewrite Photon programs.
     (defmethod ana/macroexpand-hook `hyperfiddle.photon/run [the-var form env args] `(hyperfiddle.photon/run ~@args)) ; optional
     ;(defmethod ana/macroexpand-hook `hyperfiddle.photon/run2 [_the-var _form _env args] `(hyperfiddle.photon/run2 ~@args))

     ; Don't expand cc/binding (prevent infinite loop). Explicit implicit do
     (defmethod ana/macroexpand-hook 'clojure.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))
     (defmethod ana/macroexpand-hook 'cljs.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))))

(defmacro def
  ([sym] `(hyperfiddle.photon/def ~sym ::c/unbound))
  ([sym form]
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta sym assoc ::c/node (if (:js-globals &env) form `(quote ~form))))))

;; TODO self-refer
(defmacro fn [args & body]
  (->> body
    (cons (vec (interleave args (next c/arg-sym))))
    (cons `let)
    (list ::c/closure)))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & fdecl]
  (let [[_defn sym & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: Support IDE documentation on hover
    `(hyperfiddle.photon-bootstrap/def ~sym (fn ~@(if (string? (first fdecl)) ; GG: skip docstring
                                                    (rest fdecl)
                                                    fdecl)))))

(defmacro for [bindings & body]
  `(hyperfiddle.photon/for-by identity ~bindings ~@body))


