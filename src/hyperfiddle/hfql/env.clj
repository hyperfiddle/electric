(ns hyperfiddle.hfql.env
  (:require
   [clojure.tools.analyzer.jvm :as clj]
   [hyperfiddle.photon-impl.compiler :as compiler]
   [hyperfiddle.walk :as walk])
  (:import
   (clojure.lang IObj)))

(defn- local? [env sym] (contains? (:locals env) sym))

;; NOTE: clj-only specials   #{monitor-exit reify* finally clojure.core/import* catch monitor-enter}
;; NOTE: cljs-only specials  #{& defrecord* ns* ns js*}

(defn- quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn meta? [x] (instance? IObj x))

(defn resolve-syms [env form]
  (walk/prewalk (fn [form]
                  (let [form (cond (quoted? form) (reduced form)
                                   (symbol? form) (cond
                                                    (#{'. '%} form)     form ;; special syntax
                                                    (local? env form)   (vary-meta form assoc :external true)
                                                    (clj/specials form) (vary-meta form assoc :external true)
                                                    :else               (if-let [var (compiler/resolve-runtime env form)]
                                                                          (vary-meta (symbol var) assoc :external true)
                                                                          (if-let [var (compiler/resolve-var env form)]
                                                                            (vary-meta (compiler/var-name var) assoc :external true)
                                                                            form)))
                                   :else          form)]
                    (if (meta? form)
                      (with-meta form (resolve-syms env (meta form)))
                      form)))
                form))

(defn make-env [env]
  (if (:js-globals env)
    env
    {:ns     (ns-name *ns*)
     :locals (into {} (reduce-kv (fn [r k v] (if (instance? clojure.lang.Compiler$LocalBinding v)
                                               (assoc r (.-sym v) (.-sym v))
                                               (assoc r k v))) {} env))}))
