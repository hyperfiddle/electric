(ns hyperfiddle.hfql.env
  (:require
   [cljs.analyzer :as cljs]
   [clojure.tools.analyzer.jvm :as clj]
   [hyperfiddle.photon-impl.compiler :as compiler]
   [hyperfiddle.walk :as walk])
  (:import
   (clojure.lang Var Box IObj)))

(defn- resolve-runtime "
  Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if the var doesn't exist or
  is a macro or special form."
  [env sym]
  (assert (symbol? sym) (str "Can’t resolve " (pr-str sym)))
  (if (:js-globals env)
    (let [b (Box. true)
          v (try (with-redefs [cljs/confirm-ns (constantly nil)] ; :db/id referenced as db/id would trigger a "no such namespace: db" warning.
                   (cljs/resolve-var env sym (fn [env prefix suffix] (cljs/confirm-var-exists env prefix suffix (fn [_ _ _] (set! (.-val b) false))))))
                 (catch Throwable t
                   (throw (ex-info "Failed to resolve" {:sym sym} t))))]
      (if (.-val b)
        (:name v)
        (when-let [^Var v (compiler/resolve-var env sym)]
          (.toSymbol v))))
    (let [v (compiler/resolve-var env sym)]
      (or (when (instance? Var v) (.toSymbol ^Var v))
          (when (instance? Class v) (symbol (.getName ^Class v)))))))

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
                                                    :else               (if-let [var (resolve-runtime env form)]
                                                                          (vary-meta (symbol var) assoc :external true)
                                                                          form))
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
