(ns hyperfiddle.electric-local-def-de
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def-de))
  (:require [clojure.core :as cc]
            [contrib.assert :as ca]
            [contrib.cljs-target]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana]) ; todo remove
            [missionary.core :as m]))

#?(:clj
   (do
     ;; Optionally, tell RCF not to rewrite Electric programs.
     (defmethod ana/macroexpand-hook `single [the-var form env args]
       (reduced form))))

(defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}}))

(def web-config {::lang/peers {:client :cljs, :server :clj}})

#?(:clj (defmacro test-compile
          ([nm form] `(test-compile ~nm {} ~form))
          ([nm env form] `(lang/compile ~nm '~form (merge web-config (lang/normalize-env ~env))))))

(defn run-single [frame] (m/reduce #(do %2) nil frame))
#?(:clj (defmacro single {:style/indent 1} [conf & body]
          (ca/check map? conf)
          (let [env (merge (->local-config &env) (lang/normalize-env &env) conf)]
            `(run-single (r/root-frame {::Main ~(lang/compile ::Main `(do ~@body) env)} ::Main)))))
