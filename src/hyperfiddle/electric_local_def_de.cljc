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

(defmacro main {:style/indent 1} [conf & body]
  (ca/is conf map? "provide config map as first argument")
  `(r/->defs {::Main ~(lang/->source (->env &env conf) ::Main `(e/fn [] (do ~@body)))}))

(defn half-conn [in-cb out-cb handler]
  (m/reduce (constantly nil)
    (m/ap (let [v (m/?> (m/stream (handler (fn [!] (in-cb !) #()))))]
            ((m/? out-cb) v)))))

(defn full-conn [server-handler client-handler]
  (let [s->c (m/dfv)
        c->s (m/dfv)]
    (m/join {}
      (half-conn c->s s->c server-handler)
      (half-conn s->c c->s client-handler))))

(defn run-local [defs main]
  (r/client {}
    (fn [handler]
      (full-conn (r/server {} defs main) handler)) defs main))

(def run-single (partial r/client {} (fn [_] m/never)))

(defmacro local {:style/indent 1} [conf & body]
  `(run-local (main ~conf ~@body) ::Main))

(defmacro single {:style/indent 1} [conf & body]
  `(run-single (main ~conf ~@body) ::Main))