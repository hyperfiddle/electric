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

(def run-local
  (letfn [(subject [^objects state slot]
            (fn [cb] (aset state slot cb) #(aset state slot nil)))
          (reader [^objects state slot]
            (m/ap (m/? (m/?> (m/seed (repeat (aget state slot)))))))
          (writer [^objects state slot latency events]
            (m/reduce (fn ([] (aget state slot))
                        ([cb x] (cb x) cb))
              (m/zip {} latency events)))]
    (fn [[client-read-clock client-write-clock] [server-read-clock server-write-clock] defs main]
      (let [state (doto (object-array 4)
                    (aset 0 (m/mbx))                        ;; client->server
                    (aset 1 (m/mbx)))]                      ;; server->client
        (m/join (constantly nil)
          (r/peer-boot (r/make-peer :client {} (subject state 2) defs main nil)
            (partial writer state 0 client-write-clock))
          (writer state 1 server-write-clock
            (r/peer-events (r/make-peer :server {} (subject state 3) defs main nil)))
          (writer state 3 server-read-clock (reader state 0))
          (writer state 2 client-read-clock (reader state 1)))))))

(def immediate [(m/seed (repeat nil)) (m/seed (repeat nil))])

(defmacro local {:style/indent 1} [conf & body]
  `(run-local
     ~(::lang/client-clock conf `immediate)
     ~(::lang/server-clock conf `immediate)
     (main ~conf ~@body) ::Main))

(defn run-single [defs main]
  (r/peer-sink (r/make-peer :client {} nil defs main nil)))

(defmacro single {:style/indent 1} [conf & body]
  `(run-single (main ~conf ~@body) ::Main))