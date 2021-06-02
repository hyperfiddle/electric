(ns hfdl.impl.compiler
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [cljs.analyzer :as cljs]
            [hfdl.impl.runtime :as r])
  (:import clojure.lang.Compiler$LocalBinding
           cljs.tagged_literals.JSValue
           (clojure.lang Box)))

(defn out [& args]
  (doseq [arg args]
    (.print System/out (pr-str arg))
    (.print System/out " "))
  (.println System/out))

;; GG: This is a copy/paste from `clojure.tools.analyser.jvm/macroexpand-1`, without inlining.
(defn macroexpand-1'
  "If form represents a macro form,returns its expansion, else returns form."
  ([form] (macroexpand-1' form (clj/empty-env)))
  ([form env]
   (env/ensure (clj/global-env)
     (cond
       (seq? form) (let [[op & _args] form]
                     (if (clj/specials op)
                       form
                       (let [v (utils/resolve-sym op env)
                             m (meta v)
                             local? (-> env :locals (get op))
                             macro? (and (not local?) (:macro m)) ;; locals shadow macros
                             ]
                         (if macro?
                           (let [res (apply v form (:locals env) (rest form))] ; (m &form &env & args)
                             (when-not (clj/ns-safe-macro v)
                               (clj/update-ns-map!))
                             (if (utils/obj? res)
                               (vary-meta res merge (meta form))
                               res))
                           (clj/desugar-host-expr form env)))))
       (symbol? form) (clj/desugar-symbol form env)
       :else form))))


(defn runtime-resolve
  "Returns the fully qualified symbol of the var resolved by given symbol at runtime."
  [env form]
  (when (symbol? form)
    (let [b (Box. true)
          v (cljs/resolve-var env form
              (fn [env prefix suffix]
                (cljs/confirm-var-exists env prefix suffix
                  (fn [_ _ _] (set! (.-val b) false)))))]
      (when (.-val b) (:name v)))))


(def analyze-clj
  (let [scope-bindings
        (partial reduce-kv
          (fn [scope symbol binding]
            (assoc scope
              symbol (or (when (instance? Compiler$LocalBinding binding)
                           (let [binding ^Compiler$LocalBinding binding]
                             {:op   :local
                              :tag  (when (.hasJavaClass binding)
                                      (some-> binding (.getJavaClass)))
                              :form symbol
                              :name symbol}))
                       binding))) {})]
    (fn [env form]
      (binding [clj/run-passes clj/scheduled-default-passes]
        (clj/analyze form
          (->> env
            (scope-bindings)
            (update (clj/empty-env) :locals merge))
          {:bindings {#'ana/macroexpand-1 macroexpand-1'}})))))

(defn var-symbol [ast]
  (case (:op ast) :var (or (:name ast) (symbol (:var ast))) nil))

(defn global [s]
  (keyword
    (let [n (namespace s)]
      (case n "cljs.core" "clojure.core" n))
    (name s)))

(defn free [env ast]
  (case (:op ast)
    (:local)
    (if (contains? env (:name ast))
      {} {(:name ast) (:form ast)})

    (:let :loop)
    (let [{:keys [bindings body]} ast]
      ((fn rec [env bindings]
         (if-some [[binding & bindings] bindings]
           (merge (free env (:init binding))
                  (rec (assoc env (:name binding) (:form binding)) bindings))
           (free env body))) env (seq bindings)))

    (:invoke)
    (->> (cons (:fn ast) (:args ast))
         (map (partial free env))
         (apply merge))

    (->> (map ast (:children ast))
         (map (partial free env))
         (apply merge))
    ))

(defn free-variables [local methods]
  (->> methods
    (map (fn [{:keys [params body]}]
           (free (into {} (map (juxt :name :form))
                   (if local (cons local params) params)) body)))
    (apply merge)))

(defn emit-fn [prefix body arity]
  (let [arg-syms (into [] (map (comp symbol (partial str (name prefix)))) (range arity))]
    (list `fn arg-syms (apply body arg-syms))))

(def remote {:client :server, :server :client})

(defn map-results [f & results]
  (cons
    (apply f (map first results))
    (apply concat (map next results))))

(def analyze
  (letfn [(analyze-symbol [env sym]
            (let [res (cljs/resolve-var env sym)]
              (case (:op res)
                :js-var (list [:interop #(:name res)])
                :var (list [:global (global (:name res))])
                :local (if-some [p (::peer res)]
                         (if (= p (::peer env))
                           (list [:local (::tier res)])
                           (list [:input] [:local (::tier res)]))
                         (list [:literal (:name res)])))))
          (analyze-literal [_ value]
            (list [:literal `(quote ~value)]))
          (analyze-binding [[env & res] [name init]]
            (let [peer (::peer env)
                  tier (peer (::tier env))]
              (cons
                (-> env
                  (update ::tier assoc peer (inc tier))
                  (update :locals update name assoc
                    :name name ::peer peer ::tier tier))
                (cons (analyze-form env init) res))))
          (analyze-sexpr [env form]
            (out :sexpr form)
            ;; Inlining inhibition : don't macroexpand if the operator refers to a runtime var.
            (let [exp (if (runtime-resolve env (first form))
                        form (cljs/macroexpand-1 env form))] ;; TODO save line+column
              (if (identical? form exp)
                (let [[op & args] form]
                  (case op
                    (var case* throw try def fn* letfn* loop* recur set! ns ns* deftype* defrecord*)
                    (throw (ex-info "Unsupported operation." {:op op :args args}))

                    (let*)
                    (let [[env & res] (reduce analyze-binding (list env) (partition 2 (first args)))]
                      (reduce (partial map-results (fn [r x] [:share x r]))
                        (analyze-form env (cons `do (next args))) res))

                    (do)
                    (->> (reverse args)
                      (map (partial analyze-form env))
                      (apply map-results
                        (fn [expression & statements]
                          (reduce (fn [r x] [:effect x r]) expression statements))))

                    (if)
                    (let [[test then else] args]
                      (analyze-form env `(deref ((u/map-falsey (unquote ~else)) test (unquote ~then)))))

                    (quote)
                    (analyze-literal env (first args))

                    (js* new)
                    (apply map-results
                      (partial vector :interop (partial list op (first args)))
                      (map (partial analyze-form env) (next args)))

                    (.)
                    (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
                      (->> (cons (:target dot) (:args dot))
                        (map (partial analyze-form env))
                        (apply map-results
                          (partial vector :interop
                            (case (:dot-action dot)
                              ::cljs/call (fn [target & args] `(. ~target ~(:method dot) ~@args))
                              ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot)))))))))))

                    (clojure.core/deref cljs.core/deref)
                    (map-results (partial vector :variable) (analyze-form env (first args)))

                    (clojure.core/unquote cljs.core/unquote)
                    (map-results (partial vector :constant) (analyze-form env (first args)))

                    (clojure.core/unquote-splicing cljs.core/unquote-splicing)
                    (let [x (analyze-form (update env ::peer remote) (first args))]
                      (list (reduce (fn [r o] [:output o r]) [:input] (next x)) (first x)))

                    (->> (cons op args)
                      (map (partial analyze-form env))
                      (apply map-results (partial vector :apply)))))
                (analyze-form env exp))))
          (analyze-map [env form]
            (analyze-form env `(hash-map ~@(interleave (keys form) (vals form)))))
          (analyze-vector [env form]
            (analyze-form env `(vector ~@form)))
          (analyze-set [env form]
            (analyze-form env `(hash-set ~@form)))
          (analyze-js [env form]
            (assert nil "Not implemented : #js"))
          (analyze-form [env form]
            ((or
               (and (symbol? form) analyze-symbol)
               (and (seq? form) (seq form) analyze-sexpr)
               (and (map? form) analyze-map)
               (and (vector? form) analyze-vector)
               (and (set? form) analyze-set)
               (and (instance? JSValue form) analyze-js)
               analyze-literal) env form))]
    (fn [env form]
      (-> env
        (assoc
          ::peer :client
          ::tier {:client 0
                  :server 0})
        (analyze-form form)))))

(defn emit-log-failure [form]
  `(u/log-failure ~form))

(defn emit! [prefix node]
  ((fn walk! [tier [op & args]]
     (-> (case op
           :input `(m/signal! (r/input ~prefix))
           :apply `(m/latest u/call ~@(map (partial walk! tier) args))
           :share `(let [~(symbol (str (name prefix) tier))
                         (m/signal! ~(walk! tier (first args)))]
                     ~(walk! (inc tier) (second args)))
           :local (symbol (str (name prefix) (first args)))
           :global `(u/pure ~(symbol (first args)))
           :effect `(do (m/stream! ~(walk! tier (first args)))
                        ~(walk! tier (second args)))
           :output `(do ((r/output-cb ~prefix) ~(walk! tier (first args)))
                        ~(walk! tier (second args)))
           :literal `(u/pure ~(first args))
           :interop `(m/latest
                       ~(emit-fn prefix (first args) (count (next args)))
                       ~@(map (partial walk! tier) (next args)))
           :constant `(u/pure ~(walk! tier (first args)))
           :variable `(switch ~(walk! tier (first args))))
       #_emit-log-failure)) 0 node))

(defn df [prefix env form]
  (let [[client & server] (analyze env form)]
    (out client server)
    `(fn [n# t#]
       (if-some [~prefix (r/get-ctx)]
         (do ((r/remote-cb ~prefix) (list ~@server))
             (~(emit! prefix client) n# t#))
         (u/failer (ex-info "Unable to find dataflow context." {}) n# t#)))))

(defn vars [env forms]
  (into {} (map
             (comp (juxt global identity)
               (if (:js-globals env)
                 (partial runtime-resolve env)
                 (comp var-symbol (partial analyze-clj env)))))
    forms))