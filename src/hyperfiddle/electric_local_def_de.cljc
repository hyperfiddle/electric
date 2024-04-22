(ns hyperfiddle.electric-local-def-de
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def-de))
  (:require [clojure.core :as cc]
            [contrib.assert :as ca]
            #?(:clj [fipp.edn])
            [contrib.cljs-target]
            [hyperfiddle.electric-de :as e]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [hyperfiddle.rcf.analyzer :as ana]) ; todo remove
            [missionary.core :as m]))

#?(:clj
   (do
     ;; Optionally, tell RCF not to rewrite Electric programs.
     (defmethod ana/macroexpand-hook `single [the-var form env args]
       (reduced form))))

(defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}}))

#?(:clj (defmacro test-compile
          ([nm form] `(test-compile ~nm {} ~form))
          ([nm env form] `(lang/compile ~nm '~form (merge e/web-config (lang/normalize-env ~env))))))

#?(:clj (defn code->ts* [env conf form]
          (ca/check map? conf)
          (let [env (merge (->local-config env) (lang/normalize-env env) conf)
                expanded (lang/expand-all env `(::lang/ctor ~form))
                ts (lang/analyze expanded '_ env (lang/->ts))
                _  (when (::lang/print-analysis env) (run! prn (->> ts :eav vals (sort-by :db/id))))]
            (lang/analyze-electric env ts))))

#?(:clj (defmacro code->ts {:style/indent 1} [conf & body]
          `(code->ts* ~&env ~conf '(do ~@body))))

#?(:clj (defn ->env [env conf] (merge (->local-config env) (lang/normalize-env env) conf)))

(defn run-single [frame] (m/reduce #(do %2) nil frame))
(defmacro single {:style/indent 1} [conf & body]
  (ca/is conf map? "provide config map as first argument")
  `(run-single (r/root-frame (r/->defs {::Main ~(lang/->source (->env &env conf) ::Main `(e/fn [] (do ~@body)))}) ::Main)))

(defn run-local [defs main]
  (m/reduce #(do %2) nil
    (let [s->c (m/dfv), c->s (m/dfv)
          c (m/stream (r/peer (fn [!] (s->c !) #()) :client defs main))
          s (m/stream (r/peer (fn [!] (c->s !) #()) :server defs main))]
      (m/ap (m/amb=
              (let [v (m/?> c)] ((m/? c->s) v))
              (let [v (m/?> s)] ((m/? s->c) v)))))))

(defmacro local {:style/indent 1} [conf & body]
  (ca/is conf map? "provide config map as first argument")
  `(run-local (r/->defs {::Main ~(lang/->source (->env &env conf) ::Main `(e/fn [] (do ~@body)))}) ::Main))
