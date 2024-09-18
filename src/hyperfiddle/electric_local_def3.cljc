(ns hyperfiddle.electric-local-def3
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def3))
  (:require [clojure.core :as cc]
            [contrib.assert :as ca]
            #?(:clj [fipp.edn])
            [contrib.cljs-target]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.runtime3 :as r]
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

(defn half-conn [out-cb latency events]
  (m/reduce (constantly nil)
    (m/ap (let [v (m/?> (m/zip {} latency events))]
            ((m/? out-cb) v)))))

(defn run-local [[inbound-latency outbound-latency] defs main]
  (let [s->c (m/dfv)
        c->s (m/dfv)
        client (r/make-peer :client {} (fn [!] (s->c !) #()) defs main nil)
        server (r/make-peer :server {} (fn [!] (c->s !) #()) defs main nil)]
    (m/join {}
      (half-conn s->c inbound-latency (r/peer-events server))
      (r/peer-boot client (partial half-conn c->s outbound-latency)))))

(defn run-single [defs main]
  (r/peer-sink (r/make-peer :client {} nil defs main nil)))

(def no-latency [(m/seed (repeat nil)) (m/seed (repeat nil))])

(defmacro local {:style/indent 1} [conf & body]
  `(run-local ~(::lang/remote-latency conf `no-latency) (main ~conf ~@body) ::Main))

(defmacro single {:style/indent 1} [conf & body]
  `(run-single (main ~conf ~@body) ::Main))