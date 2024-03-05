(ns hyperfiddle.electric-local-def-de
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def-de))
  (:require [clojure.core :as cc]
            [contrib.assert :as ca]
            #?(:clj [fipp.edn])
            [contrib.cljs-target]
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

(def web-config {::lang/peers {:client :cljs, :server :clj}})

#?(:clj (defmacro test-compile
          ([nm form] `(test-compile ~nm {} ~form))
          ([nm env form] `(lang/compile ~nm '~form (merge web-config (lang/normalize-env ~env))))))

#?(:clj
   (defn collect-deps [deps]
     (loop [ret (sorted-set) deps deps]
       (if-some [d (first deps)]
         (if (ret d)
           (recur ret (disj deps d))
           (let [dds (lang/get-deps d)]
             (recur (conj ret d) (into deps dds))))
         ret))))

(defn run-single [frame] (m/reduce #(do %2) nil frame))
#?(:clj (defmacro single {:style/indent 1} [conf & body]
          (ca/check map? conf)
          (let [env (merge (->local-config &env) (lang/normalize-env &env) conf)
                expanded (lang/expand-all env `(::lang/ctor (do ~@body)))
                _ (when (::lang/print-expansion env) (fipp.edn/pprint expanded))
                ts (lang/analyze expanded '_ env (lang/->ts))
                _  (when (::lang/print-analysis env) (run! prn (->> ts :eav vals (sort-by :db/id))))
                ts (lang/analyze-electric env ts)
                ctors (mapv #(lang/emit-ctor ts % env ::Main) (lang/get-ordered-ctors-e ts))
                ret-e (lang/get-ret-e ts (lang/get-child-e ts 0))
                deps (lang/emit-deps ts ret-e)
                deps (collect-deps deps)
                defs (into {} (map (fn [dep] [(keyword dep) dep])) deps)
                defs (assoc defs ::Main ctors)]
            (when (::lang/print-source env) (fipp.edn/pprint ctors))
            (when (::lang/print-defs env) (fipp.edn/pprint defs))
            `(run-single (r/root-frame ~defs ::Main)))))
