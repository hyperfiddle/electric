(ns hyperfiddle.electric.impl.compiler
  (:require [cljs.analyzer :as cljs-ana]
            [cljs.core]
            [cljs.env]
            [contrib.assert :as ca]
            [contrib.debug]
            [hyperfiddle.electric :as-alias e]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

(defn- fn-> [f a] (fn [o] (f o a)))

(declare -expand-all-in-try)

(defn resolve-cljs [env sym]
  (when (not= '. sym)
    (let [!found? (volatile! true)
          resolved (binding [cljs-ana/*cljs-warnings* (assoc cljs-ana/*cljs-warnings* :undeclared-ns false)]
                     (cljs-ana/resolve-var env sym
                       (fn [env prefix suffix]
                         (cljs-ana/confirm-var-exists env prefix suffix
                           (fn [_ _ _] (vreset! !found? false)))) nil))]
      (when (and resolved @!found? (not (:macro resolved)))
        ;; If the symbol is unqualified and is from a different ns (through e.g. :refer)
        ;; cljs returns only :name and :ns. We cannot tell if it resolved to a macro.
        ;; We recurse with the fully qualified symbol to get all the information.
        ;; The symbol can also resolve to a local in which case we're done.
        ;; TODO how to trigger these in tests?
        (if (and (simple-symbol? sym) (not= (:ns env) (:ns resolved)) (not= :local (:op resolved)))
          (recur env (ca/check qualified-symbol? (:name resolved) {:sym sym, :resolved resolved}))
          resolved)))))

(defn serialized-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _))))

(defn macroexpand-clj [o] (serialized-require (ns-name *ns*)) (macroexpand-1 o))

(defn expand-referred-or-local-macros [o cljs-macro-env]
  ;; (:require [some.ns :refer [some-macro]])
  ;; `some-macro` might be a macro and cljs expander lookup fails to find it
  ;; another case is when a cljc file :require-macros itself without refering the macros
  (if-some [vr (when (simple-symbol? (first o)) (resolve (first o)))]
    (if (and (not (class? vr)) (.isMacro ^clojure.lang.Var vr))
      (apply vr o cljs-macro-env (rest o))
      o)
    o))

(defn expand-macro [env o]
  (let [[f & args] o, n (name f), e (dec (count n))]
    (if (= "." n)
      o
      (if (and (not= ".." n) (= \. (nth n e)))
        `(new ~(symbol (namespace f) (subs n 0 e)) ~@args)
        (if (some? (re-find #"^\.[^.]" n))
          (list* '. (first args) (symbol (subs n 1)) (rest args))
          (if (= :cljs (get (::peers env) (::current env)))
            (let [cljs-env (::cljs-env env)]
              (if (resolve-cljs cljs-env f)
                o
                (let [cljs-macro-env (cond-> cljs-env (::ns cljs-env) (assoc :ns (::ns cljs-env)))]
                  (if-some [expander (cljs-ana/get-expander f cljs-macro-env)]
                    (apply expander o cljs-macro-env args)
                    (expand-referred-or-local-macros o cljs-macro-env)))))
            (macroexpand-clj o)))))))

(defn find-local [env sym] (find (:locals env) sym))
(defn add-local [env sym] (update env :locals assoc sym ::unknown))

(def ^:dynamic *electric* true)

(defn ?meta [metao o]
  (if (instance? clojure.lang.IObj o)
    (cond-> o (meta metao) (vary-meta #(merge (meta metao) %)))
    o))

(defn -expand-all [o env]
  (cond
    (and (seq? o) (seq o))
    (if (find-local env (first o))
      (list* (first o) (mapv (fn-> -expand-all env) (rest o)))
      (case (first o)
        ;; (ns ns* deftype* defrecord* var)

        (do) (if (nnext o)
               (let [body (mapv #(list `e/drain %) (next o))
                     body (conj (pop body) (second (peek body)))] ; last arg isn't drained
                 (recur (?meta o (cons `e/amb body)) env))
               (recur (?meta o (second o)) env))

        (let*) (let [[_ bs & body] o
                     [bs2 env2] (reduce
                                  (fn [[bs env] [sym v]]
                                    [(conj bs sym (-expand-all v env)) (add-local env sym)])
                                  [[] env]
                                  (partition-all 2 bs))]
                 (?meta o (list* 'let* bs2 (mapv (fn-> -expand-all env2) body))))

        (loop*) (let [[_ bs & body] o
                      [bs2 env2] (reduce
                                   (fn [[bs env] [sym v]]
                                     [(conj bs sym (-expand-all v env)) (add-local env sym)])
                                   [[] env]
                                   (partition-all 2 bs))]
                  (recur (?meta o `(binding [r/rec (::closure (let [~@(interleave (take-nth 2 bs2) r/arg-sym)]
                                                                        ~@body))]
                                     (new r/rec ~@(take-nth 2 (next bs2)))))  env2))

        (case clojure.core/case)
        (let [[_ v & clauses] o
              has-default-clause? (odd? (count clauses))
              clauses2 (cond-> clauses has-default-clause? butlast)
              xpand (fn-> -expand-all env)]
          (?meta o (list* 'case (xpand v)
                     (cond-> (into [] (comp (partition-all 2) (mapcat (fn [[match expr]] [match (xpand expr)])))
                               clauses2)
                       has-default-clause? (conj (xpand (last clauses)))))))

        (quote) o

        (fn*) (let [[?name more] (if (symbol? (second o)) [(second o) (nnext o)] [nil (next o)])
                    arities (cond-> more (vector? (first more)) list)]
                (?meta o (apply list
                           (into (if ?name ['fn* ?name] ['fn*])
                             (map (fn [[syms & body]]
                                    (binding [*electric* false]
                                      (list* syms (mapv (fn-> -expand-all (reduce add-local env syms)) body)))))
                             arities))))

        (letfn*) (let [[_ bs & body] o
                       env2 (reduce add-local env (take-nth 2 bs))
                       xpand (fn-> -expand-all env2)
                       bs2 (into [] (comp (partition-all 2)
                                      (mapcat (fn [[sym v]] [sym (binding [*electric* false] (xpand v))])))
                             bs)]
                   (?meta o `(let* [~(vec (take-nth 2 bs2)) (::letfn ~bs2)] ~@(mapv xpand body))))

        (try) (throw (ex-info "try is TODO" {:o o})) #_(list* 'try (mapv (fn-> -all-in-try env) (rest o)))

        (binding clojure.core/binding)
        (let [[_ bs & body] o]
          (?meta o (list* 'binding (into [] (comp (partition-all 2) (mapcat (fn [[sym v]] [sym (-expand-all v env)]))) bs)
                     (mapv #(-expand-all % env) body))))

        (set!) (if *electric*
                 (recur (?meta o `((fn* [v#] (set! ~(nth o 1) v#)) ~(nth o 2))) env)
                 (?meta o (list 'set! (-expand-all (nth o 1) env) (-expand-all (nth o 2) env))))

        (::toggle) (concat (take 3 o)
                          (let [env (assoc env ::current (second o))]
                            (mapv (fn-> -expand-all env) (drop 3 o))))

        #_else
        (if (symbol? (first o))
          (let [o2 (expand-macro env o)]
            (if (identical? o o2)
              (?meta o (list* (first o) (mapv (fn-> -expand-all env) (rest o))))
              (recur (?meta o o2) env)))
          (?meta o (list* (-expand-all (first o) env) (mapv (fn-> -expand-all env) (next o)))))))

    (map-entry? o) (clojure.lang.MapEntry. (-expand-all (key o) env) (-expand-all (val o) env))
    (coll? o) (?meta (meta o) (into (empty o) (map (fn-> -expand-all env)) o))
    :else o))

#_(defn -expand-all-in-try [o env]
  (if (seq? o)
    (if (find-local env (first o))
      (list* (first o) (mapv (fn-> -expand-all env) (rest o)))
      (case (first o)
        (catch) (let [[_ typ sym & body] o, env2 (add-local env sym)]
                  (list* 'catch typ sym (mapv (fn-> -expand-all env2) body)))
        #_else (-expand-all o env)))
    (-expand-all o env)))

;; :js-globals -> cljs env
;; :locals -> cljs or electric env
;; ::lang/peers -> electric env
;; if ::current = :clj expand with clj environment
;; if ::current = :cljs expand with cljs environment

(defn enrich-for-require-macros-lookup [cljs-env nssym]
  (if-some [src (cljs-ana/locate-src nssym)]
    (let [ast (:ast (with-redefs [cljs-ana/missing-use-macro? (constantly nil)]
                      (binding [cljs-ana/*passes* []]
                        (cljs-ana/parse-ns src {:load-macros true, :restore false}))))]
      ;; we parsed the ns form without `ns-side-effects` because it triggers weird bugs
      ;; this means the macro nss from `:require-macros` might not be loaded
      (run! serialized-require (-> ast :require-macros vals set))
      (assoc cljs-env ::ns ast))
    cljs-env))

(tests "enrich of clj source file is noop"
  (cljs.env/ensure (enrich-for-require-macros-lookup {:a 1} 'clojure.core)) := {:a 1})

;; takes an electric environment, which can be clj or cljs
;; if it's clj we need to prep the cljs environment (cljs.env/ensure + cljs.analyzer/empty-env with patched ns)
;; we need to be able to swap the environments infinite number of times

(defn ->common-env [env]
  (if (::cljs-env env)
    env
    (assoc env ::cljs-env
      (if (contains? env :js-globals)
        env
        (cond-> (cljs.analyzer/empty-env) (:ns env) (enrich-for-require-macros-lookup (:ns env)))))))

(defn expand-all [env o] (cljs.env/ensure (-expand-all o (->common-env env))))
