(ns hyperfiddle.q9.env 
  (:require
   [cljs.analyzer :as cljs]
   [clojure.tools.analyzer.jvm :as clj]
   [hyperfiddle.walk :as walk])
  (:import
   (clojure.lang Var Box)))

(defn- resolve-var [env sym]
  (if (:js-globals env)
    (cljs/get-expander sym env)
    (let [ns-map (clj/build-ns-map)
          sym-ns (when-let [ns (namespace sym)]
                   (symbol ns))
          full-ns (when sym-ns
                    (or (-> ns-map (get (:ns env)) (get :aliases) (get sym-ns))
                        (:ns (ns-map sym-ns))))]
      (when (or (not sym-ns) full-ns)
        (let [name        (if sym-ns (-> sym name symbol) sym)
              mapped-name (-> ns-map
                              (get (or full-ns (:ns env)))
                              :mappings (get name))]
          (if (some? mapped-name)
            mapped-name
            ;; java.lang is implicit so not listed in ns form or env
            (Compiler/maybeResolveIn (the-ns (:ns env)) sym)))))))

(defn- resolve-runtime "
  Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if the var doesn't exist or
  is a macro or special form."
  [env sym]
  (assert (symbol? sym) (str "Can’t resolve " (pr-str sym)))
  (if (:js-globals env)
    (let [b (Box. true)
          v (cljs/resolve-var env sym (fn [env prefix suffix] (cljs/confirm-var-exists env prefix suffix (fn [_ _ _] (set! (.-val b) false)))))]
      (if (.-val b)
        (:name v)
        (when-let [^Var v (resolve-var env sym)]
          (.toSymbol v))))
    (let [v (resolve-var env sym)]
      (or (when (instance? Var v) (.toSymbol ^Var v))
          (when (instance? Class v) (symbol (.getName ^Class v)))))))

(defn- local? [env sym] (contains? (:locals env) sym))

;; NOTE: clj-only specials   #{monitor-exit reify* finally clojure.core/import* catch monitor-enter}
;; NOTE: cljs-only specials  #{& defrecord* ns* ns js*}

(defn- quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn resolve-syms [env form]
  (walk/prewalk (fn [form]
                  (cond (quoted? form) (reduced form)
                        (symbol? form) (cond
                                         (local? env form)   (vary-meta form assoc :external true)
                                         (clj/specials form) (vary-meta form assoc :external true)
                                         :else               (if-let [var (resolve-runtime env form)]
                                                               (vary-meta (symbol var) assoc :external true)
                                                               form))
                        :else          form))
                form))

(defn make-env [&env]
  (if (:js-globals &env)
    &env
    {:ns     (ns-name *ns*)
     :locals (into {} (reduce-kv (fn [r k v] (if (instance? clojure.lang.Compiler$LocalBinding v)
                                               (assoc r (.-sym v) (.-sym v))
                                               (assoc r k v))) {} &env))}))
