(ns hyperfiddle.electric.impl.lang3
  (:refer-clojure :exclude [compile])
  (:require [cljs.analyzer]
            [cljs.env]
            [clojure.string :as str]
            [contrib.assert :as ca]
            [contrib.data :refer [keep-if ->box]]
            [contrib.debug :as dbg]
            [contrib.walk :as cw]
            [clojure.set :as set]
            [contrib.triple-store :as ts]
            [fipp.edn]
            [hyperfiddle.electric3 :as-alias e]
            [hyperfiddle.electric.impl.pures-fns :as pure-fns]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as cljs-ana]
            [hyperfiddle.electric.impl.destructure :as dst]
            [hyperfiddle.electric.impl.runtime3 :as r]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

;;;;;;;;;;;
;;; ENV ;;;
;;;;;;;;;;;

(defn clj-env? [env] (not (contains? env :locals)))
(defn electric-env? [env] (contains? env ::peers))
(defn cljs-env? [env] (and (contains? env :locals) (not (electric-env? env))))
(defn ->env-type [env] (if (:js-globals env) :cljs :clj))
(defn normalize-env [env] (if (clj-env? env) {:locals env, :ns {:name (ns-name *ns*)}} env))
(defn get-ns [env] (ca/is (-> env :ns :name) some? "No ns found in environment map" {:env env}))

(defn serialized-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _))))

(let [-base-cljs-env {:context :statement
                      :locals {}
                      :fn-scope []
                      :js-globals (into {}
                                    (map #(vector % {:op :js-var :name % :ns 'js})
                                      '(alert window document console escape unescape
                                         screen location navigator history location
                                         global process require module exports)))}]
  (defn ->cljs-env
    ([] (->cljs-env (ns-name *ns*)))
    ([nssym] (cond-> -base-cljs-env nssym (assoc :ns {:name nssym})))))

;;;;;;;;;;;;;;;;
;;; EXPANDER ;;;
;;;;;;;;;;;;;;;;

(defn- fn-> [f a] (fn [o] (f o a)))

(declare -expand-all-in-try)

(defn ?expand-clj-method-call [o]
  (let [[s & args] o]
    (if (clojure.lang.Compiler/namesStaticMember s)
      (let [?class (-> s namespace symbol)]
        (if (clojure.lang.Compiler$HostExpr/maybeClass ?class false)
          (list* '. ?class (-> s name symbol) args)
          o))
      o)))

(defn macroexpand-clj [o env]
  (serialized-require (ns-name *ns*))
  (if-some [mac (when-some [mac (resolve env (first o))] (when (.isMacro ^clojure.lang.Var mac) mac))]
    (apply mac o env (next o))
    (try (?expand-clj-method-call o)
         (catch ClassNotFoundException _ o)))) ; e.g. (goog.color/hslToHex ..) won't expand on clj

(def !a (cljs-ana/->!a))

(comment
  (cljs-ana/purge-ns !a 'hyperfiddle.electric3-test)
  )

(defn ->peer-type [env] (get (::peers env) (::current env)))

(defn qualify-sym [sym env]
  (if (= :cljs (->peer-type env))
    (some-> (cljs-ana/find-var @!a sym (get-ns env)) ::cljs-ana/name)
    (do (serialized-require (ns-name *ns*))
        (some-> (resolve env sym) symbol))))

(defn expand-macro [env o]
  (let [[f & args] o, n (name f), e (dec (count n))]
    (cond (= "." n) o
          (and (not= ".." n) (= \. (nth n e))) `(new ~(symbol (namespace f) (subs n 0 e)) ~@args)
          (re-find #"^\.[^.]" n) (list* '. (first args) (symbol (subs n 1)) (rest args))
          (= :cljs (->peer-type env)) (if-some [mac (cljs-ana/find-macro-var @!a f (get-ns env))]
                                        (apply mac o (merge (->cljs-env (get-ns env)) env) args)
                                        o)
          :else (macroexpand-clj o env))))

(defn find-local-entry [env sym] (contains? (:locals env) sym))
(defn add-local [env sym] (update env :locals assoc sym ::unknown))

(defn ?meta [metao o]
  (if (instance? clojure.lang.IObj o)
    (cond-> o (meta metao) (vary-meta #(merge (meta metao) %)))
    o))

(declare -expand-all -expand-all-foreign -expand-all-foreign-try)

(defn traceable [f] (case (namespace f) ("hyperfiddle.electric.impl.runtime3" "missionary.core" "hyperfiddle.incseq") false #_else true))

(defn trace-crumb [o env]
  (let [ns (-> env :ns :name), {:keys [line column]} (meta o)]
    (str ns ":" line ":" column " " o)))

(defn electric-sym? [sym]
  (let [s (name sym)]
    (and (pos? (.length s))
      (Character/isUpperCase (.charAt s 0))
      (not (re-matches #"G__\d+" s))    ; default gensym generated symbols
      (not= "js" (namespace sym))
      (not= 'RCF__tap sym))))

(defn ?expand-macro [o env caller]
  (if (symbol? (first o))
    (let [o2 (?meta o (expand-macro env o))]
      (if (identical? o o2)
        (if (electric-sym? (first o))
          (recur (?meta o (cons `e/$ o)) env caller)
          (?meta o (cond->> (?meta o (list* (first o) (mapv #(caller (?meta o %) env) (next o))))
                     (and (or (::trace env) (::e/trace env)) (some-> (qualify-sym (first o) env) (traceable)))
                     (list `r/tracing (list 'quote (trace-crumb o env))))))
        (caller o2 env)))
    (?meta o (list* (caller (?meta o (first o)) env) (mapv #(caller (?meta o %) env) (next o))))))

(defmacro $ [F & args]
  `(::call ((::static-vars r/dispatch) '~F ~F ~@(map (fn [arg] `(::pure ~arg)) args))))

(defn -expand-let-bindings [bs env]
  (let [<env> (->box env)
        f (fn [bs [sym v]] (let [env (<env>)] (<env> (add-local env sym)) (conj bs sym (-expand-all-foreign v env))))
        bs (transduce (partition-all 2) (completing f) [] bs)]
    [bs (<env>)]))

(defn jvm-type? [sym] (try (.getJavaClass (clojure.lang.Compiler$VarExpr. nil sym)) (catch Throwable _)))

(declare analyze-cljs-symbol)

(def base-js-types '#{objects ints longs floats doubles chars shorts bytes booleans
                      int  long  float  double  char  short  byte
                      clj-nil any?
                      js/Object object js/String   string   js/Array   array
                      js/Number number js/Function function js/Boolean boolean})
(defn js-type-hint? [sym] (or (= 'js sym) (= "js" (namespace sym))))
(defn js-type? [sym env] (or (contains? base-js-types sym) (js-type-hint? sym) (analyze-cljs-symbol sym env)))

(defn- replace-incompatible-type-hint [sym] (vary-meta sym update :tag #(keyword "electric.unresolved" (name %))))

(defn ?untag [sym env]
  (if-some [tag (keep-if (-> sym meta :tag) symbol?)]
    (case (->env-type env)
      (:clj)  (cond-> sym (not (jvm-type? tag))    replace-incompatible-type-hint)
      (:cljs) (cond-> sym (not (js-type? tag env)) replace-incompatible-type-hint))
    sym))

(defn -expand-fn-arity [[bs & body :as o] env]
  (let [bs (mapv #(?untag % env) bs)]
    (?meta o (list bs (-expand-all-foreign (?meta body (cons 'do body)) (reduce add-local env bs))))))

(defn xplatform-condp [pred expr & clause*]
  (let [f (gensym "pred"), v (gensym "expr")]
    `(let* [~f ~pred, ~v ~expr]
       ~(let [[default & stack*] (loop [c* clause*, stack* ()]
                                   (if (seq c*)
                                     (cond (= :>> (second c*)) (recur (drop 3 c*) (cons (take 3 c*) stack*))
                                           (next c*) (recur (drop 2 c*) (cons (take 2 c*) stack*))
                                           :else (cons (first c*) stack*))
                                     (cons ::no stack*)))]
          (reduce (fn [ac nx] (case (count nx)
                                2 `(if (~f ~(nth nx 0) ~v) ~(nth nx 1) ~ac)
                                3 `(if-let [x# (~f ~(nth nx 0) ~v)] (~(nth nx 2) x#) ~ac)))
            (case default ::no `(throw (ex-info (str "No matching clause: " ~v) {})) #_else default)
            stack*)))))

(defn -expand-all-foreign [o env]
  (cond
    (and (seq? o) (seq o))
    (if (find-local-entry env (first o))
      (?meta o (list* (first o) (mapv (fn-> -expand-all-foreign env) (rest o))))
      (case (first o)
        (do) (if (nnext o)
               (?meta o (cons 'do (eduction (map (fn-> -expand-all-foreign env)) (next o))))
               (recur (?meta o (second o)) env))

        (let clojure.core/let cljs.core/let)
        (let [[_ bs & body] o] (recur (?meta o (list* 'let* (dst/destructure* bs) body)) env))

        (let* loop*) (let [[call bs & body] o, [bs2 env2] (-expand-let-bindings bs env)]
                       (?meta o (list call bs2 (-expand-all-foreign (?meta body (cons 'do body)) env2))))

        (case clojure.core/case)
        (let [[_ v & clauses] o
              has-default-clause? (odd? (count clauses))
              clauses2 (cond-> clauses has-default-clause? butlast)
              xpand (fn-> -expand-all-foreign env)]
          (?meta o (list* 'case (xpand v)
                     (cond-> (into [] (comp (partition-all 2) (mapcat (fn [[match expr]] [match (xpand expr)])))
                               clauses2)
                       has-default-clause? (conj (xpand (last clauses)))))))

        (condp clojure.core/condp cljs.core/condp) (recur (?meta o (apply xplatform-condp (next o))) env)

        (quote) o

        (fn*) (let [[?name more] (if (symbol? (second o)) [(second o) (nnext o)] [nil (next o)])
                    arities (cond-> more (vector? (first more)) list)]
                (?meta o (list* (into (if ?name ['fn* ?name] ['fn*]) (map (fn-> -expand-fn-arity env)) arities))))

        (letfn*) (let [[_ bs & body] o
                       env2 (reduce add-local env (eduction (take-nth 2) bs))
                       bs2 (->> bs (into [] (comp (partition-all 2)
                                              (mapcat (fn [[sym v]] [sym (-expand-all-foreign v env2)])))))]
                   (?meta o `(letfn* ~bs2 ~(-expand-all-foreign (cons 'do body) env2))))

        (try) (list* 'try (mapv (fn-> -expand-all-foreign-try env) (rest o)))

        (set!) (let [[_ t v] o] (list 'set!
                                  (-expand-all-foreign t (dissoc env ::trace))
                                  (-expand-all-foreign v env)))

        (binding clojure.core/binding)
        (let [[_ bs & body] o]
          (?meta o (list 'binding (into [] (comp (partition-all 2)
                                             (mapcat (fn [[sym v]] [sym (-expand-all-foreign v env)]))) bs)
                     (-expand-all-foreign (cons 'do body) env))))

        #_else (?expand-macro o env -expand-all-foreign)))

    (instance? cljs.tagged_literals.JSValue o)
    (cljs.tagged_literals.JSValue. (-expand-all-foreign (.-val ^cljs.tagged_literals.JSValue o) env))

    (map-entry? o) (clojure.lang.MapEntry. (-expand-all-foreign (key o) env) (-expand-all-foreign (val o) env))
    (coll? o) (?meta (meta o) (into (empty o) (map (fn-> -expand-all-foreign env)) o))
    :else o))

(defn -expand-all-foreign-try [o env]
  (if (seq? o)
    (if (find-local-entry env (first o))
      (?meta o (list* (first o) (mapv (fn-> -expand-all-foreign env) (rest o))))
      (case (first o)
        (catch) (let [[_ typ sym & body] o]
                  (list* 'catch typ sym (mapv (fn-> -expand-all-foreign (add-local env sym)) body)))
        #_else (-expand-all-foreign o env)))
    (-expand-all-foreign o env)))

(defn pretty-aliaser [alias-map]
  (fn [sym]
    (if-some [[_ ns-str] (find alias-map (namespace sym))]
      (cond (symbol? sym) (symbol ns-str (name sym))
            (keyword? sym) (keyword (str ":" ns-str) (name sym)) ; hack so we see ::foo, not :foo
            :else sym)
      sym)))

(tests
  ((pretty-aliaser {"foo.bar" "fb"}) 'foo.bar/baz) := 'fb/baz
  ((pretty-aliaser {"foo.bar" "fb"}) :foo.bar/baz) := (keyword ":fb" "baz")
  ((pretty-aliaser {"clojure.core" nil}) 'clojure.core/+) := '+
  )

(def pp-aliaser (pretty-aliaser {"hyperfiddle.electric.impl.runtime3" "r"
                                 "hyperfiddle.electric.impl.lang3" "lang"
                                 "hyperfiddle.electric-local-def3" "ldef"
                                 "hyperfiddle.electric3-test" "t"
                                 "hyperfiddle.electric3" "e"
                                 "hyperfiddle.incseq" "i"
                                 "missionary.core" "m"
                                 "clojure.core" nil}))

(let [rewrite #(cond-> % (or (symbol? %) (keyword? %)) pp-aliaser)]
  (defn pprint-source [source] (fipp.edn/pprint (cw/postwalk rewrite source) {:width 120})))

(defn +meta [env o]
  (if (instance? clojure.lang.IObj o)
    (cond-> o (::meta env) (vary-meta #(merge (::meta env) %)))
    o))

(defn -expand-all [o env]
  (let [env (cond-> env (meta o) (update ::meta merge (meta o))), +meta (fn [o] (+meta env o))]
    ;; (prn (meta o) (::meta env) o)
    (cond
      (and (seq? o) (seq o))
      (if (find-local-entry env (first o))
        (if (electric-sym? (first o))
          (recur (+meta (cons `$ o)) env)
          (+meta (list* (+meta (first o)) (mapv #(-expand-all (+meta %) env) (rest o)))))
        (case (first o)
          ;; (ns ns* deftype* defrecord* var)

          (do) (if (nnext o)
                 (let [body (mapv #(+meta (list `e/drain %)) (next o))
                       body (conj (pop body) (+meta (second (peek body))))] ; last arg isn't drained
                   (recur (+meta (cons `e/amb body)) env))
                 (recur (+meta (second o)) env))

          (let clojure.core/let cljs.core/let)
          (let [[_ bs & body] o] (recur (+meta (list* 'let* (dst/destructure* bs) body)) env))

          (let*) (let [[_ bs & body] o
                       <env> (->box env)
                       f (fn [bs [sym v]]
                           (let [env (<env>)] (<env> (add-local env sym)) (conj bs sym (-expand-all (+meta v) env))))
                       bs2 (transduce (partition-all 2) (completing f) [] bs)]
                   (+meta (list 'let* bs2 (-expand-all (+meta (cons 'do body)) (<env>)))))

          (loop*) (let [[_ bs & body] o
                        [bs2 env2] (reduce
                                     (fn [[bs env] [sym v]]
                                       [(conj bs sym (-expand-all (+meta v) env)) (add-local env sym)])
                                     [[] env]
                                     (partition-all 2 bs))]
                    (recur (+meta `(::call (r/bind-args (r/bind-self (::ctor (let [~@(interleave (take-nth 2 bs2)
                                                                                       (map (fn [i] `(::lookup ~i))
                                                                                         (range)))] ~@body)))
                                             ~@(map (fn [arg] `(::pure ~arg))
                                                 (take-nth 2 (next bs2))))))
                      env2))

          (recur) (recur (+meta `(::call (r/bind-args (::lookup :recur) ~@(map (fn [arg] `(::pure ~arg)) (next o))))) env)

          (case clojure.core/case)
          (let [[_ v & clauses] o
                has-default-clause? (odd? (count clauses))
                clauses2 (cond-> clauses has-default-clause? butlast)
                xpand #(-expand-all (+meta %) env)]
            (+meta (list* 'case (xpand v)
                     (cond-> (into [] (comp (partition-all 2) (mapcat (fn [[match expr]] [match (xpand expr)])))
                               clauses2)
                       has-default-clause? (conj (xpand (last clauses)))))))

          (if) (let [[_ test then else] o, xpand #(-expand-all (+meta %) env)]
                 (+meta (list 'case (xpand test) '(nil false) (xpand else) (xpand then))))

          (condp clojure.core/condp cljs.core/condp) (recur (+meta (apply xplatform-condp (next o))) env)

          (quote) o

          (fn*) (-expand-all-foreign o (dissoc env ::electric))

          (letfn*) (let [[_ bs & body] o
                         env2 (reduce add-local env (take-nth 2 bs))
                         bs2 (->> bs (into [] (comp (partition-all 2)
                                                (mapcat (fn [[sym v]] [sym (-expand-all-foreign v env2)])))))]
                     (recur (+meta `(let [~(vec (take-nth 2 bs2)) (::cc-letfn ~bs2)]
                                      ~(-expand-all (+meta (cons 'do body)) env2)))
                       env))

          (try) (throw (ex-info "try is TODO" {:o o})) #_(list* 'try (mapv (fn-> -all-in-try env) (rest o)))

          (js*) (let [[_ s & args] o, gs (repeatedly (count args) gensym)]
                  (recur (+meta `((fn* ([~@gs] (~'js* ~s ~@gs))) ~@args)) env))

          (binding clojure.core/binding)
          (let [[_ bs & body] o]
            (+meta (list 'binding (->> bs (into [] (comp (partition-all 2)
                                                     (mapcat (fn [[sym v]] [sym (-expand-all (+meta v) env)])))))
                     (-expand-all (+meta (cons 'do body)) env))))

          (set!) (+meta (list 'set! (-expand-all (+meta (nth o 1)) env) (-expand-all (+meta (nth o 2)) env)))

          (var) (recur (+meta `(clojure.core/find-var '~(-> o second resolve symbol))) env)

          (::ctor) (+meta (list ::ctor (list ::site nil (-expand-all (+meta (second o)) env))))

          (::site) (+meta (seq (conj (into [] (take 2) o)
                                 (-expand-all (+meta (cons 'do (drop 2 o))) (assoc env ::current (second o))))))

          #_else (?expand-macro o env -expand-all)))

      (instance? cljs.tagged_literals.JSValue o)
      (cljs.tagged_literals.JSValue. (-expand-all (+meta (.-val ^cljs.tagged_literals.JSValue o)) env))

      (map-entry? o) (+meta (clojure.lang.MapEntry. (-expand-all (key o) env) (-expand-all (+meta (val o)) env)))
      (coll? o) (+meta (into (+meta (empty o)) (map #(-expand-all (+meta %) env)) o))
      :else o)))

#_(defn -expand-all-in-try [o env]
    (if (seq? o)
      (if (find-local-entry env (first o))
        (list* (first o) (mapv (fn-> -expand-all env) (rest o)))
        (case (first o)
          (catch) (let [[_ typ sym & body] o, env2 (add-local env sym)]
                    (list* 'catch typ sym (mapv (fn-> -expand-all env2) body)))
          #_else (-expand-all o env)))
      (-expand-all o env)))

(defn expand-all [env o]
  (cljs-ana/analyze-nsT !a env (get-ns env))
  (-expand-all o (assoc env ::electric true)))

;;;;;;;;;;;;;;;;
;;; COMPILER ;;;
;;;;;;;;;;;;;;;;

(defn fail!
  ([env msg] (fail! env msg {}))
  ([env msg data] (throw (ex-info (str "\n" (get-ns env) (when-some [d (::def env)] (str "/" d)) ":" (-> env ::meta :line) ":" (-> env ::meta :column) "\n" msg)
                           (cond-> data (::def env) (assoc :in (::def env)), (::current env) (assoc :for (::current env)))))))

(defn cannot-resolve! [env form]
  (fail! env (str "I cannot resolve [" form "]")))

(defn ns-qualify [node] (if (namespace node) node (symbol (str *ns*) (str node))))

(tests
  (ns-qualify 'foo) := `foo
  (ns-qualify 'a/b) := 'a/b)

(defn bound-js-fn
  "Given a js global resolving to a function (e.g js/alert, js/console.log required-js-ns/js-fn), ensures it
  is called under the correct `this` context."
  [sym]
  (let [fields (str/split (name sym) #"\.")]
    `(.bind ~sym ~(symbol (namespace sym)
                    (if (seq (rest fields))
                      (str/join (interpose '. (butlast fields)))
                      "globalThis")))))

(defn resolve-static-field [sym]
  (when-some [ns (some-> (namespace sym) symbol)]
    (when-some [cls (resolve ns)]
      (when (class? cls)
        (clojure.lang.Reflector/getField cls (name sym) true)))))

(defn get-children-e [ts e] (-> ts :ave ::parent (get e)))
(defn ?get-child-e [ts e] (first (get-children-e ts e)))
(defn get-child-e [ts e] (ca/is (first (get-children-e ts e)) some? (str "no child for " e) {:e e, :nd (ts/->node ts e)}))
(defn get-root-e [ts] (get-child-e ts '_))

(defn ?add-source-map [{{::keys [->id]} :o :as ts} e form env]
  (let [mt (meta form)]
    (cond-> ts (:line mt) (ts/add {:db/id (->id), ::source-map-of e
                                   ::line (:line mt), ::column (:column mt)
                                   ::def (::def env), ::ns (get-ns env)}))))

(defn ?copy-source-map [{{::keys [->id]} :o :as ts} from-e to-e]
  (if-some [sm-e* (ts/find ts ::source-map-of from-e)]
    (ts/add ts (assoc (ts/->node ts (first sm-e*)) :db/id (->id), ::source-map-of to-e))
    ts))

(defn untwin [s]
  (if (= "cljs.core" (namespace s))
    (let [clj (symbol "clojure.core" (name s))]
      (if (resolve clj) clj s))
    s))

(tests
  (untwin 'cljs.core/prn) := 'clojure.core/prn
  (untwin 'a/b) := 'a/b
  (untwin 'a) := 'a
  (untwin 'cljs.core/not-in-clj) := 'cljs.core/not-in-clj)

(defn node? [mt] (::node mt))
(defn resolve-node [sym env]
  (case (->env-type env)
    :clj (when-some [^clojure.lang.Var vr (resolve env sym)]
           (when (-> vr meta node?) (symbol vr)))
    :cljs (when-some [vr (cljs-ana/find-var @!a sym (get-ns env))]
            ;; temporary hack
            ;; the commented out expression should work, seems the new cljs analyzer loses the metadata
            ;; so we check it on clj side, which is safe for a clj-server/cljs-client setup
            (when-some [vr (cljs-ana/safe-requiring-resolve (-> vr ::cljs-ana/name))]
              (when (-> vr meta node?) (symbol vr)))
            #_(when (-> vr ::cljs-ana/meta node?)
                (symbol (-> vr :name str))))))

(defmulti analyze-symbol (fn [env-type _sym _env] env-type))

(defn analyze-clj-symbol [sym env]
  (if (resolve-static-field sym)
    {::type ::static, ::sym sym}
    (when-some [v (some-> (find-ns (get-ns env)) (ns-resolve sym))]
      (if (var? v) {::type ::var, ::sym (symbol v) ::meta (meta v)} {::type ::static, ::sym sym}))))

(defmethod analyze-symbol :clj [_ sym env] (analyze-clj-symbol sym env))

(def implicit-cljs-nses '#{goog goog.object goog.string goog.array Math String})
(defn analyze-cljs-symbol [sym env]
  (if-some [v (cljs-ana/find-var @!a sym (get-ns env))]
    {::type ::var, ::sym (untwin (::cljs-ana/name v)), ::meta (::cljs-ana/meta v)}
    (if-some [quald (when (qualified-symbol? sym) (cljs-ana/ns-qualify @!a sym (get-ns env)))]
      {::type ::static, ::sym quald}
      (if (or (cljs-ana/referred? @!a sym (get-ns env)) (cljs-ana/js-call? @!a sym (get-ns env)))
        {::type ::static, ::sym sym}
        (when (cljs-ana/imported? @!a sym (get-ns env))
          {::type ::static, ::sym sym})))))

(defmethod analyze-symbol :cljs [_ sym env] (analyze-cljs-symbol sym env))

(defn resolve-symbol [sym env]
  (if-some [local (-> env :locals (get sym))]
    (if-some [uid (::electric-let local)]
      {::lang nil, ::type ::localref, ::sym sym, ::ref uid}
      {::lang nil, ::type ::local, ::sym sym})
    (if (= sym (::def env))
      {::lang nil, ::type ::self, ::sym sym}
      (if-some [nd (resolve-node sym env)]
        {::lang nil, ::type ::node, ::node nd}
        (let [ts (reduce-kv
                   (fn [ts p t] (if-some [v (analyze-symbol t sym env)] (ts/add ts (assoc v :db/id p ::lang t ::peer p)) ts))
                   (ts/->ts) (::peers env))]
          (letfn [(node [e] (ts/->node ts e))
                  (skip-auto-siting [nd] (dissoc nd ::peer))
                  (for-peer   [p] (node (first (ts/find ts ::peer p))))
                  (other-peer [p] (node (first (-> ts :ave ::peer (dissoc p) first second))))
                  (a-peer     []  (node (first (ts/key ts ::peer))))
                  (for-env    [t] (node (first (ts/find ts ::lang t))))
                  (no-peer-resolved? [] (nil? (ts/key ts ::lang)))
                  (every-peer-resolved? [ts env] (= (count (ts/key ts ::peer)) (count (::peers env))))]
            (if (no-peer-resolved?)
              (cannot-resolve! env sym)
              (if-some [c (::current env)]
                (if-some [res (for-peer c)]
                  (skip-auto-siting res)
                  (let [res (other-peer c)]
                    (if (= ::var (::type res)) res (cannot-resolve! env sym))))
                (if (every-peer-resolved? ts env)
                  (skip-auto-siting (for-env (->env-type env)))
                  (a-peer))))))))))

(defn e->uid [ts e] (ca/is (::uid (ts/->node ts e)) some? "node without uid" {:e e, :nd (ts/->node ts e)}))
(defn uid->e [ts uid] (first (ca/check #(= 1 (count %)) (ts/find ts ::uid uid))))
(defn ->bindlocal-value-e [ts e] (first (get-children-e ts e)))
(defn ->bindlocal-body-e [ts e] (second (get-children-e ts e)))
(defn ->localv-e [ts mklocal-uid] (get-child-e ts (uid->e ts mklocal-uid)))


(defn get-ret-e [ts e]
  (let [nd (get (:eav ts) e)]
    (case (::type nd)
      (::site ::sitable ::mklocal) (recur ts (get-child-e ts e))
      #_else e)))

(defn find-sitable-point-e [ts e]
  (loop [e e]
    (let [nd (ts/->node ts e)]
      (if (contains? nd ::site) e (some-> (::parent nd) recur)))))

(defn get-site [ts e] (ts/? ts (find-sitable-point-e ts e) ::site))

(defn get-local-site [ts localv-e]
  (let [ret-e (get-ret-e ts localv-e)]
    (loop [e ret-e]
      (let [nd (ts/->node ts e)]
        (if (contains? nd ::site)
          (::site nd)
          (case (::type nd)
            (::localref) (recur (->localv-e ts (::ref nd)))
            #_else (some-> (::parent nd) recur)))))))

(defn resolve-as-non-local [sym env] (resolve-symbol sym (update env :locals dissoc sym)))

(defn get-lookup-key [sym env]
  (if (symbol? sym)
    (let [it (resolve-as-non-local sym env)]
      (case (::type it)
        (::electric-var) (keyword (::sym it))
        (::var) (throw (ex-info (str "[" sym "] is not an electric var") {::form sym, ::resolved it}))
        (::node) (keyword (::node it))
        (::static) (throw (ex-info (str "`" sym "` did not resolve as a var") {::form sym}))
        #_else (keyword sym)))
    sym))

(declare analyze analyze-foreign wrap-foreign-for-electric)

(defn add-literal [{{::keys [->id]} :o :as ts} v e pe env]
  (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
    (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v v, ::site (::current env)})
    (?add-source-map e v env)))

(defn add-ap-literal [f args pe e env form {{::keys [->id ->uid]} :o :as ts}]
  (let [ce (->id)]
    (reduce (fn [ts form] (analyze form e env ts))
      (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap, ::uid (->uid), ::site (::current env)})
        #_(add-literal f ce e)
        (ts/add {:db/id ce, ::parent e, ::type ::pure, ::site (::current env)})
        (ts/add {:db/id (->id), ::parent ce, ::type ::literal, ::v f, ::site (::current env)})
        (?add-source-map e form env))
      args)))

(defn ->class-method-call [clazz method method-args pe env form {{::keys [->id]} :o :as ts}]
  (if (seq method-args)
    (let [f (let [margs (repeatedly (count method-args) gensym), meth (symbol (str clazz) (str method))]
              `(fn [~@margs] (~meth ~@margs)))]
      (add-ap-literal f method-args pe (->id) env form ts))
    (let [e (->id)]                     ; (. java.time.Instant now)
      (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
        (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form, ::site (::current env)})
        (?add-source-map e form env)))))

(defn meta-of-key [mp k] (-> mp keys set (get k) meta))
(defn gensym-with-local-meta [env k]
  (let [g (gensym (if (instance? clojure.lang.Named k) (name k) "o")), mt (meta-of-key (:locals env) k)]
    (?untag (with-meta g (merge mt (meta k))) env)))
(defn type-hint-as-js [sym] (vary-meta sym assoc :tag 'js))
(defn ?tag-as-js [env sym]
  (case (->env-type env)
    (:clj) sym
    (:cljs) (let [tag (-> sym meta :tag)]
              (cond-> sym (or (nil? tag) (not (js-type? tag env))) type-hint-as-js))))

(defn ->obj-method-call [o method method-args pe env form {{::keys [->id]} :o :as ts}]
  (let [f (let [[oo & margs] (mapv #(gensym-with-local-meta env %) (cons o method-args))
                oo (?tag-as-js env oo)]
            `(fn [~oo ~@margs] (. ~oo ~method ~@margs)))]
    (add-ap-literal f (cons o method-args) pe (->id) env form ts)))

(defn def-sym-in-cljs-compiler! [sym ns]
  (swap! @(requiring-resolve 'cljs.env/*compiler*)
    assoc-in [:cljs.analyzer/namespaces ns :defs sym] {:name sym}))

(defn reparent-children [ts from-e to-e]
  (reduce (fn [ts e] (ts/asc ts e ::parent to-e)) ts (ts/find ts ::parent from-e)))

(defn delete-point-recursively [ts e]
  (let [ts (ts/del ts e)]
    (if-some [ce (get-children-e ts e)]
      (reduce delete-point-recursively ts ce)
      ts)))

(defn move-point [ts from-e to-e]
  (let [pe (ts/? ts to-e ::parent)]
    (-> ts
      (delete-point-recursively to-e)
      (ts/add (assoc (ts/->node ts from-e) :db/id to-e, ::parent pe))
      (reparent-children from-e to-e)
      (ts/del from-e))))

(defn copy-point-recursively
  ([{{::keys [->id]} :o :as ts} from-e pe] (copy-point-recursively ts from-e pe (->id)))
  ([{{::keys [->uid]} :o :as ts} from-e pe to-e]
   (reduce (fn [ts e] (copy-point-recursively ts e to-e))
     (-> ts (ts/add (assoc (ts/->node ts from-e) :db/id to-e, ::parent pe, ::uid (->uid)))
       (?copy-source-map from-e to-e))
     (get-children-e ts from-e))))

(defn ?update-meta [env form] (cond-> env (meta form) (assoc ::meta (meta form))))

(defn my-turn? [env] (let [c (get (::peers env) (::current env))] (or (nil? c) (= c (->env-type env)))))

(defn field-access? [sym] (str/starts-with? (str sym) "-"))

(defn ?swap-out-foreign-host-type [f env]
  (case (->env-type env)
    :clj (if (and (symbol? f) (jvm-type? f)) f 'Object)
    :cljs (if (and (symbol? f) (js-type? f env)) f 'js/Object)))

(defn ->node [ts uid] (ts/->node ts (ts/find1 ts ::uid uid)))

(defn analyze [form pe env {{::keys [->id ->uid]} :o :as ts}]
  (let [env (?update-meta env form), ctor (::ctor-uid env)]
    (cond
      (and (seq? form) (seq form))
      (case (first form)
        (let*) (let [[_ bs bform] form]
                 (recur (?meta form
                          (reduce (fn [ac [k v]]
                                    (let [g (with-meta (gensym k) (meta k))]
                                      (?meta k
                                        `(::mklocal ~g (::bindlocal ~g ~v ~(?meta k `(::mklocal ~k (::bindlocal ~k ~g ~ac))))))))
                            bform (->> bs (partition 2) reverse)))
                   pe env ts))
        (::mklocal) (let [[_ k bform] form, e (->id), uid (->uid)
                          ts (-> ts (ts/add {:db/id e, ::ctor-local ctor, ::type ::mklocal
                                             ::k k, ::uid uid})
                               (?add-source-map e form env))]
                      (recur (?meta form bform) pe (update-in env [:locals k] assoc ::electric-let uid) ts))
        (::bindlocal) (let [[_ k v bform] form
                            mklocal-e (:db/id (->node ts (-> env :locals (get k) ::electric-let)))]
                        (recur (?meta form bform) pe env (analyze v mklocal-e env ts)))
        (::case_) (let [[_ test & brs] form
                        [default brs2] (if (odd? (count brs))
                                         [(last brs) (butlast brs)]
                                         [`(r/case-default-required) brs])
                        <br*> (->box []), track (fn [g] (-> (<br*>) (conj g) (<br*>)) g)
                        code (transduce (take-nth 2) (fn ([ac] (conj ac (track (gensym "default"))))
                                                       ([ac nx] (conj ac nx (track (gensym "branch"))))) [] brs2)]
                    (recur (?meta form
                             `((fn* ([test# ~@(<br*>)] (~'case test# ~@code)))
                               ~test ~@(-> (into [] (comp (take-nth 2) (map #(list ::ctor %))) (next brs2))
                                         (conj `(::ctor ~default)))))  pe env ts))
        (case) (recur (?meta form `(::call (::case_ ~@(next form)))) pe env ts)
        (quote) (let [e (->id)]
                  (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                    (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form, ::site (::current env)})))
        (fn*) (let [current (get (::peers env) (::current env))
                    [code arg* val*] (wrap-foreign-for-electric (analyze-foreign form env))]
                (if (or (nil? current) (= (->env-type env) current))
                  (if (seq arg*)
                    (add-ap-literal `(fn* ~arg* ~code) val* pe (->id) env form ts)
                    (add-literal ts (?meta form code) (->id) pe env))
                  (recur `[~@val*] pe env ts)))
        (::cc-letfn) (let [current (get (::peers env) (::current env))
                           [_ bs] form, lfn* `(letfn* ~bs ~(vec (take-nth 2 bs))), e (->id)
                           [code arg* val*] (wrap-foreign-for-electric (analyze-foreign lfn* env))
                           ts (?add-source-map ts e form env)]
                       (if (or (nil? current) (= (->env-type env) current))
                         (if (seq arg*)
                           (add-ap-literal `(fn* ~arg* ~code) val* pe e env form ts)
                           (add-literal ts code e pe env))
                         (recur `[~@arg*] pe env ts)))
        (new) (let [[_ f & args] form]
                (if (my-turn? env)
                  (let [f (?swap-out-foreign-host-type f env)
                        f (let [gs (repeatedly (count args) gensym)] `(fn [~@gs] (new ~f ~@gs)))]
                    (add-ap-literal f args pe (->id) env form ts))
                  (recur `[~@args] pe env ts)))
        ;; (. java.time.Instant now)
        ;; (. java.time.Instant ofEpochMilli 1)
        ;; (. java.time.Instant (ofEpochMilli 1))
        ;; (. java.time.Instant EPOCH)
        ;; (. java.time.Instant -EPOCH)
        ;; (. i1                isAfter i2)
        ;; (. i1                (isAfter i2))
        ;; (. pt                x)
        ;; (. pt                -x)
        (.) (let [me? (my-turn? env)]
              (cond
                (implicit-cljs-nses (second form)) ; (Math/abs -1) expanded to (. Math abs -1)
                (let [[_ clazz method & method-args] form] ; cljs fails on dot form, so we compile as class call
                  (->class-method-call clazz method method-args pe env form ts))

                (and (symbol? (second form)) (class? (resolve env (second form))))
                (if (seq? (nth form 2)) ; (. java.time.Instant (ofEpochMilli 1))
                  (if (= :clj (->env-type env))
                    (let [[_ clazz [method & method-args]] form]
                      (->class-method-call clazz method method-args pe env form ts))
                    (recur `[~@(next (nth form 2))] pe env ts))
                  (let [[_ clazz x & xs] form] ; (. java.time.instant opEpochMilli 1)
                    (if (= :clj (->env-type env))
                      (->class-method-call clazz x xs pe env form ts)
                      (recur `[~@xs] pe env ts))))

                (seq? (nth form 2))     ; (. i1 (isAfter i2))
                (let [[_ o [method & method-args]] form]
                  (if me?
                    (->obj-method-call o method method-args pe env form ts)
                    (recur `[~(second form) ~@(next (nth form 2))] pe env ts)))

                :else
                (let [[_ o x & xs] form]
                  (if (seq xs)          ; (. i1 isAfter i2)
                    (if me?
                      (->obj-method-call o x xs pe env form ts)
                      (recur `[~o ~@xs] pe env ts))
                    (if me?             ; (. pt x)
                      (if (field-access? x)
                        (add-ap-literal `(fn [oo#] (. oo# ~x)) [o] pe (->id) env form ts)
                        (->obj-method-call o x [] pe env form ts))
                      (recur [o] pe env ts))))))
        (binding clojure.core/binding) (let [[_ bs bform] form, gs (repeatedly (/ (count bs) 2) gensym)]
                                         (recur (if (seq bs)
                                                  (?meta form
                                                    `(let* [~@(interleave gs (take-nth 2 (next bs)))]
                                                       (::call ((::static-vars r/bind) (::ctor ~bform)
                                                                ~@(interleave
                                                                    (mapv #(get-lookup-key % env) (take-nth 2 bs))
                                                                    (mapv #(list ::pure %) gs))))))
                                                  (?meta form bform))
                                           pe env ts))
        (def) (let [[_ sym v] form]
                (case (->env-type env)
                  :clj (recur `((fn* ([x#] (def ~sym x#))) ~v) pe env ts)
                  :cljs (do (def-sym-in-cljs-compiler! sym (get-ns env))
                            (add-ap-literal `(fn [v#] (set! ~sym v#)) [v] pe (->id) env form ts))))
        (set!) (let [[_ target v] form] (recur (with-meta `((fn* ([v#] (set! ~target v#))) ~v) (meta form)) pe env ts))
        (::ctor) (let [e (->id), ce (->id), uid (->uid)]
                   (recur (second form) ce (assoc env ::ctor-uid uid, ::site nil)
                     (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                       (ts/add {:db/id ce, ::parent e, ::type ::ctor, ::uid uid, ::site nil})
                       (?add-source-map e form env))))
        (::call) (let [e (->id)] (recur (second form) e env
                                   (-> (ts/add ts {:db/id e, ::parent pe, ::type ::call, ::uid (->uid)
                                                   ::call-in-ctor ctor, ::site (::current env)})
                                     (?add-source-map e form env))))
        (::tag) (let [e (->id)] (recur (second form) e env
                                  (-> (ts/add ts {:db/id e, ::parent pe, ::type ::call, ::uid (->uid)
                                                  ::call-in-ctor ctor, ::call-type ::tag, ::site (::current env)})
                                    (?add-source-map e form env))))
        (::pure) (let [pure (with-meta (gensym "pure") {::dont-inline true})]
                   (recur `(let* [~pure ~(second form)] (::pure-gen ~pure)) pe env ts))
        (::pure-gen) (let [e (->id)]
                       (recur (second form) e env (-> (ts/add ts {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                                                    (?add-source-map e form env))))
        (::join) (let [e (->id)] (recur (second form) e env (-> (ts/add ts {:db/id e, ::parent pe, ::type ::join, ::site (::current env)})
                                                              (?add-source-map e form env))))
        (::site) (let [[_ site bform] form, current (::current env), env2 (assoc env ::current site)]
                   (if (or (nil? site) (= site current))
                     (recur bform pe env2 ts)
                     ;; Due to an early bad assumption only locals are considered for runtime nodes.
                     ;; Since any site change can result in a new node we wrap these sites in an implicit local.
                     ;; Electric aggressively inlines locals, so the generated code size will stay the same.
                     (let [g (gensym "site-local")]
                       (recur (?meta form `(::mklocal ~g (::bindlocal ~g ~form ~g))) pe env2 ts))))
        (::frame) (let [e (->id)] (-> ts
                                    (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                                    (ts/add {:db/id (->id), ::parent e, ::type ::frame, ::site (::current env)})))
        (::lookup) (let [[_ sym] form, e (->id)]
                     (-> ts
                       (ts/add {:db/id e, ::parent pe, ::type ::lookup, ::sym sym, ::site (::current env)})
                       (?add-source-map e form env)))
        (::static-vars) (recur (second form) pe (assoc env ::static-vars true) ts)
        (::debug) (recur (second form) pe (assoc env ::debug true) ts)
        (::sitable) (let [e (->id)] (recur (second form) e env (ts/add ts {:db/id e, ::parent pe, ::type ::sitable, ::site (::current env)})))
        (::k) ((second form) pe env ts)
        #_else (let [current (get (::peers env) (::current env)), [f & args] form]
                 (if (and (contains? #{nil :cljs} current) (symbol? f) (cljs-ana/js-call? @!a f (get-ns env)))
                   (add-ap-literal (case (->env-type env) :cljs (bound-js-fn f) :clj `r/cannot-resolve)
                     args pe (->id) env form ts)
                   (let [e (->id), uid (->uid)]
                     (reduce (fn [ts nx] (analyze nx e env ts))
                       (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap, ::uid uid, ::site (::current env)})
                         (?add-source-map e form env)) form)))))

      (instance? cljs.tagged_literals.JSValue form)
      (let [o (.-val ^cljs.tagged_literals.JSValue form)]
        (if (map? o)
          (recur (?meta form (cons `(::static-vars cljs.core/js-obj) (into [] (mapcat (fn [[k v]] [(name k) v])) o)))
            pe env ts)
          (recur (?meta form (cons `(::static-vars cljs.core/array) o)) pe env ts)))

      (vector? form) (recur (?meta form (cons `(::static-vars vector) form)) pe env ts)
      (map? form) (recur (?meta form (cons `(::static-vars array-map) (eduction cat form))) pe env ts) ; array-map to respect program order, map literals only.
      (set? form) (recur (?meta form (cons `(::static-vars hash-set) form)) pe env ts)

      (symbol? form)
      (let [e (->id), ret (resolve-symbol form env)]
        (-> (case (::type ret)
              (::localref) (ts/add ts {:db/id e, ::parent pe, ::type ::localref, ::ref (::ref ret)
                                       ::sym form, ::uid (->uid)})
              (::local) (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                          (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form, ::site (::current env)}))
              (::self) (let [ce (->id)]
                         (-> ts
                           (ts/add {:db/id e, ::parent pe, ::type ::lookup, ::sym (keyword (ns-qualify form)), ::site (::current env)})
                           (ts/add {:db/id ce, ::parent e, ::type ::pure, ::site (::current env)})
                           (ts/add {:db/id (->id), ::parent ce, ::type ::literal, ::v (list form), ::site (::current env)})))
              (::static ::var) (let [k (fn [pe env {{::keys [->id]} :o :as ts}]
                                         (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
                                           (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::site (::current env)
                                                    ::v (let [lang (::lang ret)]
                                                          (if (or (nil? lang) (= lang (->env-type env)))
                                                            form `r/cannot-resolve))})))]
                                 (if (::peer ret)
                                   (analyze (?meta form `(::site ~(::peer ret) (::sitable (::k ~k)))) pe env ts)
                                   (k pe env ts)))
              (::node) (ts/add ts {:db/id e, ::parent pe, ::type ::node, ::node (::node ret)})
              #_else (throw (ex-info (str "unknown symbol type " (::type ret)) (or ret {}))))
          (?add-source-map e form env)))

      :else
      (let [e (->id)]
        (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure, ::site (::current env)})
          (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form, ::site (::current env)})
          (?add-source-map e form env))))))

(defn add-foreign-local [env sym] (update env :locals update sym assoc ::electric-let nil))

(defn ->->id [] (let [!i (long-array [-1])] (fn [] (aset !i 0 (unchecked-inc (aget !i 0))))))

(defn addf [{{::keys [->id p ->pi]} :o :as ts} u more]
  (ts/add ts (assoc more :db/id (->id), ::u u, ::p p, ::i (->pi))))

(defn under-root [ts u ->i] (update ts :o assoc ::p u, ::->pi ->i))

(defn under [{{::keys [->u p ->pi]} :o :as ts} base f]
  (let [u (->u), ->i (->->id), ts (addf ts u base)]
    (-> ts (under-root u ->i) f (under-root p ->pi))))

(defn- add-invoke [ts form env]
  (under ts {::t ::invoke}
    (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts form))))

(defn ->foreign-class-method-call [ts clazz method x* env]
  (under ts {::t ::class-method-call, ::class clazz, ::method method}
    (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts x*))))

(defn ->foreign-method-call [ts o method x* env]
  (under ts {::t ::method-call, ::method method}
    (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (cons o x*)))))

(defn ->foreign-field-access [ts o field env]
  (under ts {::t ::field-access, ::field field}
    (fn [ts] (analyze-foreign ts o env))))

(defn analyze-foreign
  ([form env] (analyze-foreign (ts/->ts {::->id (->->id), ::->u (->->id), ::p -1, ::->pi (->->id)}) form env))
  ([{{::keys [->u p]} :o :as ts} form env]
   (cond (and (seq? form) (seq form))
         (case (first form)
           (let* loop*)
           (let [[l bs & body] form]
             (under ts {::t (case l (let*) ::let* (loop*) ::loop*)}
               (fn [ts]
                 (let [<env> (->box env)
                       f (fn [ts [sym v]]
                           (let [env (<env>)]
                             (<env> (add-foreign-local env sym))
                             (under ts {::t ::let*-sym, ::sym sym}
                               (fn [ts] (analyze-foreign ts v env)))))
                       ts (transduce (partition-all 2) (completing f) ts bs)]
                   (under ts {::t ::body}
                     (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx (<env>))) ts body)))))))

           (binding clojure.core/binding)
           (let [[_ bs & body] form]
             (under ts {::t ::binding}
               (fn [ts]
                 (-> (reduce (fn [ts [sym v]]
                               (under ts {::t ::binding-sym, ::sym sym}
                                 (fn [ts] (analyze-foreign ts v env))))
                       ts (eduction (partition-all 2) bs))
                   (under {::t ::body}
                     (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts body)))))))

           (quote) (addf ts (->u) {::t ::quote, ::v form})

           (fn*) (let [[?name arity+] (if (symbol? (second form)) [(second form) (nnext form)] [nil (next form)])
                       env2 (cond-> env ?name (add-foreign-local ?name))]
                   (under ts (cond-> {::t ::fn*} ?name (assoc ::name ?name))
                     (fn [ts]
                       (reduce (fn [ts [args & body]]
                                 (let [env3 (reduce add-foreign-local env2 args)]
                                   (under ts {::t ::fn*-arity, ::args args}
                                     (fn [ts]
                                       (reduce (fn [ts nx] (analyze-foreign ts nx env3)) ts body)))))
                         ts arity+))))

           (letfn*) (let [[_ bs & body] form
                          env (reduce add-foreign-local env (eduction (take-nth 2) bs))]
                      (under ts {::t ::letfn*}
                        (fn [ts]
                          (let [ts (reduce (fn [ts f] (analyze-foreign ts f env)) ts (eduction (take-nth 2) (next bs)))]
                            (under ts {::t ::body}
                              (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts body)))))))

           (.) (let [o (second form), x* (drop 2 form)
                     ;; (. java.time.Instant (ofEpochMilli 1)) vs. (. java.time.Instant ofEpochMilli 1)
                     [x x*] (if (seq? (first x*)) [(ffirst x*) (nfirst x*)] [(first x*) (next x*)])]
                 (if (symbol? o)
                   (if (find-local-entry env o)
                     (if (field-access? x)
                       (->foreign-field-access ts o x env)
                       (->foreign-method-call ts o x x* env))
                     (case (->peer-type env)
                       (:clj) (if-some [r (resolve env o)]
                                (cond (class? r) (->foreign-class-method-call ts o x x* env)
                                      (field-access? x) (->foreign-field-access ts o x env)
                                      :else (->foreign-method-call ts o x x* env))
                                (fail! env (str o " is a JS class, cannot use in JVM")))
                       (:cljs) (if (analyze-cljs-symbol o env)
                                 (cond (implicit-cljs-nses o) (->foreign-class-method-call ts o x x* env) ; clj macroexpands to (. Math abs), which cljs fails to call
                                       (field-access? x) (->foreign-field-access ts o x env)
                                       :else (->foreign-method-call ts o x x* env))
                                 (fail! env (str o " is a JVM class, cannot use in JS")))
                       #_else (case (->env-type env)
                                (:clj) (if-some [r (resolve env o)]
                                         (cond (class? r) (->foreign-class-method-call ts o x x* env)
                                               (field-access? x) (->foreign-field-access ts o x env)
                                               :else (->foreign-method-call ts o x x* env))
                                         (if (field-access? x)
                                           (->foreign-field-access ts o x env)
                                           (->foreign-method-call ts o x x* env)))
                                (:cljs) (if (analyze-cljs-symbol o env)
                                          (cond (implicit-cljs-nses o) (->foreign-class-method-call ts o x x* env)
                                                (field-access? x) (->foreign-field-access ts o x env)
                                                :else (->foreign-method-call ts o x x* env))
                                          (if (field-access? x)
                                            (->foreign-field-access ts (with-meta o {:tag 'js}) x env)
                                            (->foreign-method-call ts (with-meta o {:tag 'js}) x x* env))))))
                   (->foreign-method-call ts o x x* env)))

           (def) (under ts {::t ::def, ::sym (second form)}
                   (fn [ts] (analyze-foreign ts (nth form 2) env)))

           (set!) (under ts {::t ::set!}
                    (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           (new) (let [[_ f & args] form]
                   (if (my-turn? env)
                     (under ts {::t ::new}
                       (fn [ts]
                         (let [f (?swap-out-foreign-host-type f env)]
                           (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (cons f args)))))
                     (recur ts `[~@args] env)))

           (do) (under ts {::t ::do}
                  (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           (js*) (under ts {::t ::js*}
                   (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           (try) (under ts {::t ::try}
                   (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           (catch) (if (= ::try (::t (ts/->node ts p)))
                     (let [[_ typ sym & body] form
                           k (fn [typ]
                               (if (ts/find ts ::p p, ::t ::catch, ::ex-type typ)
                                 ts ; duplicate handler, can happen in lenient mode, e.g. (catch Exception) (catch Throwable) would both map to :default on cljs
                                 (under ts {::t ::catch, ::ex-type typ, ::sym sym}
                                   (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx (add-foreign-local env sym))) ts body)))))]
                       (case (->peer-type env)
                         (:clj) (cond (= :default typ) (k 'Throwable)
                                      (class? (resolve env typ)) (k typ)
                                      :else (fail! env (str typ " is a JS class, cannot catch on JVM")))
                         (:cljs) (if (or (= :default typ) (analyze-cljs-symbol typ env))
                                   (k typ)
                                   (fail! env (str typ " is a JVM class, cannot catch on JS")))
                         #_else (case (->env-type env)
                                  (:clj) (k (if (class? (resolve env typ)) typ 'Throwable))
                                  (:cljs) (k (if (or (= :default typ) (analyze-cljs-symbol typ env)) typ :default)))))
                     (add-invoke ts form env))

           (finally) (if (= ::try (::t (ts/->node ts p)))
                       (under ts {::t ::finally}
                         (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))
                       (add-invoke ts form env))

           (if) (under ts {::t ::if}
                  (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           (clojure.core/case case) (let [[_ test & branch*] form]
                                      (under ts {::t ::case}
                                        (fn [ts]
                                          (let [ts (under ts {::t ::case-test} (fn [ts] (analyze-foreign ts test env)))]
                                            (reduce (fn [ts nx]
                                                      (if (next nx) ; normal branch with test
                                                        (let [[test form] nx]
                                                          (under ts {::t ::case-branch, ::test test}
                                                            (fn [ts] (analyze-foreign ts form env))))
                                                        (under ts {::t ::case-default}
                                                          (fn [ts] (analyze-foreign ts (first nx) env)))))
                                              ts (eduction (partition-all 2) branch*))))))

           (var) (under ts {::t ::builtin-var} (fn [ts] (analyze-foreign ts (second form) env)))

           (throw) (under ts {::t ::throw} (fn [ts] (analyze-foreign ts (second form) env)))

           (recur) (under ts {::t ::recur}
                     (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (next form))))

           #_else (add-invoke ts form env))

         (instance? cljs.tagged_literals.JSValue form)
         (let [o (.-val ^cljs.tagged_literals.JSValue form)]
           (if (map? o)
             (under ts {::t ::js-map}
               (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (eduction cat o))))
             (under ts {::t ::js-array}
               (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts o)))))

         (map? form) (under ts {::t ::map}
                       (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts (eduction cat form))))

         (set? form) (under ts {::t ::set}
                       (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts form)))

         (vector? form) (under ts {::t ::vector}
                          (fn [ts] (reduce (fn [ts nx] (analyze-foreign ts nx env)) ts form)))

         (symbol? form) (let [ret (resolve-symbol form env)]
                          (case (::type ret)
                            (::localref) (addf ts (->u)
                                           {::t ::electric-local, ::sym form
                                            ::resolved (::sym ret), ::ref (::ref ret)})
                            (::local) (addf ts (->u)
                                        {::t ::local, ::sym form, ::resolved (::sym ret)})
                            (::static) (addf ts (->u)
                                         (if (= (::lang ret) (->env-type env))
                                           {::t ::static, ::sym form, ::resolved (::sym ret)}
                                           {::t ::var, ::sym form, ::resolved `r/cannot-resolve}))
                            (::self ::node) (addf ts (->u)
                                              {::t ::electric-var, ::sym form ::resolved (::node ret)
                                               ::meta (::meta ret)})
                            (::var) (addf ts (->u)
                                      {::t ::var, ::sym form, ::meta (::meta ret)
                                       ::resolved (let [lang (::lang ret)]
                                                    (if (or (nil? lang) (= lang (->env-type env)))
                                                      (::sym ret) `r/cannot-resolve))})
                            #_else (throw (ex-info (str "unknown symbol type " (::type ret)) (or ret {})))))

         :else (addf ts (->u) {::t ::literal, ::v form}))))

(defn emit-foreign
  ([ts] (emit-foreign ts (::u (ts/->node ts (ts/find1 ts ::p -1)))))
  ([ts u]
   (letfn [(->node [u] (ts/->node ts (ts/find1 ts ::u u)))
           (e->u [e] (::u (ts/->node ts e)))
           (order [u*] (sort-by (comp ::i ->node) u*))
           (find [& kvs] (order (eduction (map e->u) (apply ts/find ts kvs))))
           (find1 [& kvs] (e->u (apply ts/find1 ts kvs)))
           (? [u k] (get (->node u) k))
           (emit-foreign-arity [u] (cons (? u ::args) (eduction (map emit) (find ::p u))))
           (unname [v] (cond-> v (instance? clojure.lang.Named v) name))
           (emit-1 [sym u] (list sym (emit (find1 ::p u))))
           (emit-n [sym u] (list* sym (eduction (map emit) (find ::p u))))
           (emit [u]
             (let [nd (->node u)]
               (case (::t nd)
                 (::let*) (let [{sym* ::let*-sym, body ::body} (group-by #(? % ::t) (find ::p u))]
                            (list* 'let* (into [] (mapcat (fn [u] [(? u ::sym) (emit (find1 ::p u))])) sym*)
                              (eduction (map emit) (find ::p (first body)))))
                 (::loop*) (let [{sym* ::let*-sym, body ::body} (group-by #(? % ::t) (find ::p u))]
                             (list* 'loop* (into [] (mapcat (fn [u] [(? u ::sym) (emit (find1 ::p u))])) sym*)
                               (eduction (map emit) (find ::p (first body)))))
                 (::binding) (let [{sym* ::binding-sym, body ::body} (group-by #(? % ::t) (find ::p u))]
                               (list* 'binding (into [] (mapcat (fn [u] [(? u ::sym) (emit (find1 ::p u))])) sym*)
                                 (eduction (map emit) (find ::p (first body)))))
                 (::case) (let [{test ::case-test, branch* ::case-branch, default ::case-default}
                                (group-by #(? % ::t) (find ::p u))]
                            (cond-> (list* 'case (emit (find1 ::p (first test)))
                                      (into [] (mapcat (fn [u] [(? u ::test) (emit (find1 ::p u))])) branch*))
                              (seq default) (concat [(emit (find1 ::p (first default)))])))
                 (::quote) (::v nd)
                 (::literal) (::v nd)
                 (::fn*) (list* (into (cond-> ['fn*] (::name nd) (conj (::name nd)))
                                  (map emit-foreign-arity (find ::t ::fn*-arity, ::p u))))
                 (::letfn*) (let [{f* ::fn*, body ::body} (group-by #(? % ::t) (find ::p u))]
                              (list* 'letfn* (into [] (mapcat (fn [u] [(? u ::name) (emit u)])) f*)
                                (eduction (map emit) (find ::p (first body)))))
                 ;; (Math/abs -1) expands to (. Math abs -1) but the dot form fails on cljs
                 ;; so we generate (Math/abs -1)
                 (::class-method-call) (list* (symbol (str (::class nd)) (str (::method nd)))
                                         (eduction (map emit) (find ::p u)))
                 (::field-access) (list '. (emit (find1 ::p u)) (::field nd))
                 (::method-call) (let [[o & arg*] (find ::p u)]
                                   (list* '. (emit o) (::method nd) (eduction (map emit) arg*)))
                 (::def) (list 'def (? u ::sym) (emit (find1 ::p u)))
                 (::set!) (emit-n 'set! u)
                 (::new) (emit-n 'new u)
                 (::do) (emit-n 'do u)
                 (::try) (emit-n 'try u)
                 (::catch) (list* 'catch (::ex-type nd) (::sym nd) (eduction (map emit) (find ::p u)))
                 (::finally) (emit-n 'finally u)
                 (::throw) (emit-1 'throw u)
                 (::if) (emit-n 'if u)
                 (::builtin-var) (emit-1 'var u)
                 (::recur) (emit-n 'recur u)
                 (::js*) (emit-n 'js* u)
                 (::invoke) (map emit (find ::p u))
                 (::js-map) (list* 'js-object (eduction (map emit) (map unname) (find ::p u)))
                 (::js-array) (list* 'array (eduction (map emit) (map unname) (find ::p u)))
                 (::map) (apply array-map (eduction (map emit) (find ::p u))) ; array-map to respect program order, map literals only.
                 (::set) (set (eduction (map emit) (find ::p u)))
                 (::vector) (vec (eduction (map emit) (find ::p u)))
                 (::var) (if (= `r/cannot-resolve (::resolved nd)) `(r/cannot-resolve-fn '~(::sym nd))  (::sym nd))
                 (::electric-local ::local ::static ::electric-var) (::sym nd))))]
     (emit u))))

(defn wrap-foreign-for-electric
  ([ts] (wrap-foreign-for-electric ts #(gensym (str/replace % #"[/.]" "_"))))
  ([ts gen]
   (letfn [(->node [u] (ts/->node ts (ts/find1 ts ::u u)))
           (e->u [e] (::u (ts/->node ts e)))
           (order [u*] (sort-by (comp ::i ->node) u*))
           (find [& kvs] (when-some [found (apply ts/find ts kvs)] (order (eduction (map e->u) found))))
           (? [u k] (get (->node u) k))
           (?cannot-resolve [r s] (if (= `r/cannot-resolve r) `(r/cannot-resolve-fn '~s) r))]
     (let [<arg*> (->box []), <val*> (->box []), <dyn*> (->box []), <seen> (->box {})
           f (fn [ts u]
               (let [nd (->node u), r (::resolved nd), s (::sym nd), seen (<seen>)]
                 (if (:dynamic (::meta nd))
                   (if (seen r)
                     ts
                     (let [lex (gen (name r))]
                       (<arg*> (conj (<arg*>) lex))     (<val*> (conj (<val*>) (?cannot-resolve r s)))
                       (<dyn*> (into (<dyn*>) [s lex])) (<seen> (assoc seen r true))
                       ts))
                   (if-some [lex (seen r)]
                     (ts/asc ts (:db/id nd) ::sym lex)
                     (let [lex (with-meta (gen (name s)) (merge (::meta nd) (meta (::sym nd))))]
                       (<arg*> (conj (<arg*>) lex))
                       (<val*> (conj (<val*>) (?cannot-resolve r s)))
                       (<seen> (assoc seen r lex))
                       (ts/asc ts (:db/id nd) ::sym lex))))))
           xf (remove #(let [nd (->node %)] (and (zero? (::i nd)) (not= -1 (::p nd)) (= ::set! (? (::p nd) ::t)))))
           ts (transduce xf (completing f) ts (find ::t ::electric-var))
           arg* (<arg*>), val* (<val*>), dyn* (<dyn*>)
           code (cond->> (emit-foreign ts) (seq dyn*) (list 'binding dyn*))
           e-local* (into [] (comp (map #(? % ::sym)) (distinct)) (find ::t ::electric-local))]
       [code (into arg* e-local*) (into val* e-local*)]))))

(defn find-ctor-uid [ts e]
  (let [nd (ts/->node ts e)]
    (case (::type nd)
      (::ctor) (::uid nd)
      (::mklocal) (::ctor-local nd)
      #_else (some->> (::parent nd) (recur ts)))))

(defn- ts->reducible* [ts f init]
  (loop [ac init, es (cons (get-root-e ts) (set/difference (-> ts :eav keys set) (->> ts :ave ::parent vals (reduce into)))), seen #{}]
    (if (or (reduced? ac) (empty? es))
      (unreduced ac)
      (let [[e & es] es]
        (if (seen e)
          (recur ac es seen)
          (recur (f ac (ts/->node ts e)) (concat (get-children-e ts e) es) (conj seen e)))))))

(defn ts->reducible [ts]
  (reify clojure.lang.IReduce
    (reduce [_ f init] (ts->reducible* ts f init))
    (reduce [_ f] (ts->reducible* ts f (f)))))

(defn ->thunk [xs] `(fn* [] (~@xs)))

(defn tag-call? [ts e] (= ::tag (::call-type (ts/->node ts e))))

(defn ->code-meta [ts e]
  (loop [e e]
    (if-some [se (first (ts/find ts ::source-map-of e))]
      (dissoc (ts/->node ts se) :db/id ::source-map-of)
      (some-> (ts/? ts e ::parent) (recur)))))

(defn emit [ts e ctor-e env nm]
  (let [ctor-uid (e->uid ts ctor-e)]
    ((fn rec [e]
       (let [nd (get (:eav ts) e)]
         (case (::type nd)
           ::literal (::v nd)
           ::ap (list* `r/ap (list 'quote (or (->code-meta ts e) {})) (mapv rec (get-children-e ts e)))
           ::var (let [in (::resolved-in nd)]
                   (list* `r/lookup 'frame (keyword (::qualified-var nd))
                     (when (or (nil? in) (= in (->env-type env))) [(list `r/pure (::qualified-var nd))])))
           ::node (list `r/lookup 'frame (keyword (::node nd)) (list `r/pure (list `r/resolve 'frame (keyword (::node nd)))))
           ::join (list `r/join (list 'quote (or (->code-meta ts e) {})) (rec (get-child-e ts e)))
           ::pure (list `r/pure (list 'quote (or (->code-meta ts e) {})) (rec (get-child-e ts e)))
           ::comp ((or (::comp-fn nd) ->thunk) (eduction (map rec) (get-children-e ts e))) #_(list 'fn* '[] (doall (map rec (get-children-e ts e))))
           ::site (recur (get-child-e ts e))
           ::sitable (recur (get-child-e ts e))
           ::ctor (list* `r/ctor nm (::ctor-idx nd)
                    (mapv (fn [e]
                            (let [nd (ts/->node ts e)]
                              (case (::closed-over nd)
                                ::node (list `r/node 'frame
                                         (->> (ts/find1 ts ::ctor-node ctor-uid, ::ctor-ref (::closed-ref nd))
                                           (ts/->node ts) ::node-idx))
                                ::free (list `r/free 'frame
                                         (->> (ts/find1 ts ::ctor-free ctor-uid, ::closed-ref (::closed-ref nd))
                                           (ts/->node ts) ::free-idx)))))
                      (ts/find ts ::ctor-free (e->uid ts e))))
           ::call (if (tag-call? ts e)
                    (list `r/pure (list 'quote (or (->code-meta ts e) {})) (list `r/tag 'frame (ca/is (::call-idx nd) number? nd)))
                    (list `r/join (list 'quote (or (->code-meta ts e) {})) (list `r/call 'frame (ca/is (::call-idx nd) number? nd))))
           ::frame 'frame
           ::lookup (list* `r/lookup 'frame (::sym nd) (when-some [c (?get-child-e ts e)] (list (rec c))))
           ::mklocal (recur (get-ret-e ts (get-child-e ts e)))
           ::localref
           (if-some [node-e (first (ts/find ts ::ctor-node (e->uid ts ctor-e), ::ctor-ref (::ref nd)))]
             (list `r/node 'frame (::node-idx (ts/->node ts node-e)))
             (if-some [free-e (first (ts/find ts ::ctor-free (e->uid ts ctor-e), ::closed-ref (::ref nd)))]
               (list `r/free 'frame (::free-idx (ts/->node ts free-e)))
               (throw (ex-info "localref must be a node or free" nd))))
           #_else (throw (ex-info (str "cannot emit on " (pr-str (::type nd))) (or nd {}))))))
     e)))

(defn emit-node-init [ts ctor-e node-e env nm]
  (let [nd (get (:eav ts) node-e)
        ret-e (->> (::ctor-ref nd) (->localv-e ts) (get-ret-e ts))]
    (list `r/define-node 'frame (::node-idx nd) (list 'quote (->code-meta ts ret-e))
      (emit ts ret-e ctor-e env nm))))

(defn emit-call-init [ts ctor-e e env nm]
  (list `r/define-call 'frame (::call-idx (ts/->node ts e))
    (emit ts (get-ret-e ts (get-child-e ts e)) ctor-e env nm)))

(defn get-ordered-ctors-e [ts] (into [] (map (comp first second)) (->> ts :ave ::ctor-idx (sort-by first))))

(defn get-ordered-calls-e [ts ctor-uid]
  (->> (ts/find ts ::ctor-call ctor-uid) (sort-by #(::call-idx (ts/->node ts %)))))

(defn get-ordered-nodes-e [ts ctor-uid]
  (->> (ts/find ts ::ctor-node ctor-uid) (sort-by #(::node-idx (ts/->node ts %)))))

(defn ->mark-program-order [] (let [->i (->->id)] (fn [ts _go nd] (ts/asc ts (:db/id nd) ::pg-order (->i)))))

(defn emit-ctor [ts ctor-e env nm]
  (let [ret-e (get-ret-e ts (get-child-e ts ctor-e))
        ctor-uid (::uid (ts/->node ts ctor-e))
        nodes-e (get-ordered-nodes-e ts ctor-uid)
        calls-e (get-ordered-calls-e ts ctor-uid)]
    `(r/cdef ~(count (ts/find ts ::ctor-free ctor-uid))
       ~(mapv #(get-site ts (->> (ts/->node ts %) ::ctor-ref (->localv-e ts) (get-ret-e ts)))
          nodes-e)
       ~(mapv #(get-site ts %) calls-e)
       ~(get-site ts ret-e)
       ~(let [init-fn* (into [] (mapcat #((ts/? ts % ::init-fn) ts ctor-e env nm)) (ts/find ts ::ctor-let-init ctor-uid))
              body `[~@(let [node-inits (->> nodes-e
                                          (mapv (fn [e] [(->> e (ts/->node ts) ::ctor-ref (uid->e ts) (ts/->node ts) ::pg-order)
                                                         (emit-node-init ts ctor-e e env nm)])))
                             call-inits (->> calls-e
                                          (remove #(tag-call? ts %))
                                          (mapv (fn [e] [(->> e (ts/->node ts) ::pg-order)
                                                         (emit-call-init ts ctor-e e env nm)])))]
                         ;; with xforms would be
                         ;; (into [] (comp cat (x/sort-by first) (map second)) [node-inits call-inits])
                         (->> (concat node-inits call-inits) (sort-by first) (eduction (map second))))
                     ~(emit ts ret-e ctor-e env nm)]
              body (if (seq init-fn*) `[(let [~@init-fn*] ~@body)] body)]
          `(fn [~'frame] ~@body)))))

(defn rewrite [ts opti]
  (let [<seen> (->box #{})]
    (letfn [(opt [ts t nd] (if-some [o (or (opti t) (opti true))] (o ts go nd) ts))
            (go
              ([ts] ts)
              ([ts e]
               (if ((<seen>) e)
                 ts
                 (let [nd (ts/->node ts e), t (::type nd)]
                   (-> (<seen>) (conj e) (<seen>))
                   (case t
                     (::literal ::var ::lookup ::node ::frame) (opt ts t nd)
                     (::ap ::comp) (opt (reduce go ts (get-children-e ts e)) t nd)
                     (::site ::join ::pure ::call ::mklocal ::sitable) (opt (go ts (get-child-e ts e)) t nd)
                     (::ctor) (let [ts (transduce (mapcat #(get-children-e ts %))
                                         go ts (ts/find ts ::ctor-let-init (::uid nd)))]
                                (opt (go ts (get-child-e ts e)) t nd))
                     (::localref) (opt (go ts (uid->e ts (::ref nd))) t nd)
                     #_else (throw (ex-info (str "cannot rewrite on " (pr-str (::type nd))) (or nd {}))))))))]
      (go ts (get-root-e ts)))))

(defn emit-deps [ts]
  (let [<deps> (->box #{})]
    (rewrite ts {::node (fn [_ts _go nd] (-> (<deps>) (conj (::node nd)) (<deps>)) ts)})
    (<deps>)))

(defn emit-fn [ts e nm]
  ((fn rec [e]
     (let [nd (get (:eav ts) e)]
       (case (::type nd)
         ::ap (map rec (get-children-e ts e))
         (::pure ::site ::sitable) (rec (get-child-e ts e))
         ::comp ((or (::comp-fn nd) ->thunk) (eduction (map rec) (get-children-e ts e)))
         ::literal (::v nd)
         ::ctor `(r/ctor ~nm ~(::ctor-idx nd))
         ::mklocal (recur (get-ret-e ts (get-child-e ts e)))
         ::localref (recur (->> (::ref nd) (->localv-e ts) (get-ret-e ts))))))
   e))

(defn implode-point [ts e]              ; remove e, reparent child, keep e as id
  (let [nd (ts/->node ts e), ce (get-child-e ts e), cnd (ts/->node ts ce)]
    (-> ts (ts/del e) (ts/del ce) (ts/add (assoc cnd :db/id e, ::parent (::parent nd))) (reparent-children ce e))))

(defn wrap-point [{{::keys [->id]} :o :as ts} e wrap-nd] ; wrap e in another point `nd`, keeping order
  (let [nd (ts/->node ts e), new-e (->id)]
    (-> ts (ts/del e)
      (ts/add (merge wrap-nd (select-keys nd [:db/id ::parent])))
      (reparent-children e new-e)
      (ts/add (assoc nd :db/id new-e, ::parent e)))))

(defn get-program-order [ts e] (::pg-order (ts/->node ts e)))

(defn pprint-db [ts write]
  (write "\n")
  (letfn [(indent [depth] (write (str/join "" (repeat (* 2 depth) \space))))
          (describe [nd depth]
            (write (name (::type nd)))
            (write " (")
            (when (::uid nd) (write (::uid nd)))
            (when (::site nd) (write (-> (::site nd) name first)))
            (write ")")
            (write " ")
            (case (::type nd)
              (::literal) (write (::v nd))
              (::localref) (write (try (ts/? ts (uid->e ts (::ref nd)) ::k)
                                       (catch Throwable e (prn nd) 'DANGLING #_(throw e))))
              (::mklocal) (write (::k nd))
              (::site) (write (::site nd))
              (::comp) (write (or (::comp-fn nd) 'thunk))
              (::lookup) (write (::sym nd))
              (::ctor) (do (run! #(write (str " " (ts/? ts % ::closed-ref))) (ts/find ts ::ctor-free (::uid nd)))
                           (write "\n")
                           (run! #(go % (inc depth)) (ts/find ts ::type ::mklocal, ::ctor-local (::uid nd))))
              #_else nil)
            (write "\n"))
          (go [e depth]
              (indent depth) (describe (ts/->node ts e) depth)
              (run! #(go % (inc depth)) (get-children-e ts e)))]
    (run! #(go % 0) (or (not-empty (get-ordered-ctors-e ts))
                      (ts/find ts ::type ::ctor)))
    ts))

(defn analyze-electric [env {{::keys [->id]} :o :as ts}]
  (when (::print-analysis env) (run! prn (->> (:eav ts) vals)) (pprint-db ts print))
  (let [->sym (or (::->sym env) gensym)
        pure-fn? (fn pure-fn? [nd] (and (= ::literal (::type nd)) (symbol? (::v nd)) (pure-fns/pure-fns (qualify-sym (::v nd) env))))
        ap-of-pures (fn ap-of-pures [ts _go {ap-e :db/id, site ::site}]
                      (let [ce (get-children-e ts ap-e)
                            nd* (mapv #(ts/->node ts %) ce)
                            pure-cnt (transduce (keep #(when (= ::pure (::type %)) 1)) + 0 nd*)]
                        #_ (when (and (every? #(= ::pure (::type (ts/->node ts (get-ret-e ts %)))) ce)
                                   (not (every? #(= ::pure (ts/? ts % ::type)) ce)))
                             (prn 'pure-not-pure ap-uid)
                             (pprint-db ts print))
                        (cond
                          (and (> pure-cnt 1) (= pure-cnt (count nd*)))
                          (if (pure-fn? (->> ce first (get-ret-e ts) (get-child-e ts) (ts/->node ts)))
                            ;; (ap (pure vector) (pure 1) (pure 2)) -> (pure (comp-with list vector 1 2))
                            (-> (reduce (fn [ts ce]
                                          (let [pure-e (get-ret-e ts ce)]
                                            (implode-point ts pure-e)))
                                  (ts/asc ts ap-e ::type ::comp, ::comp-fn list*, ::site site) ce)
                              (wrap-point ap-e {::type ::pure, ::site site}))
                            ;; (ap (pure x) (pure y) (pure z)) -> (ap (pure (comp-with ->call x y z)))
                            (let [pure-e (->id), comp-e (->id)]
                              (reduce (fn [ts e]
                                        (let [ce (->> e (get-ret-e ts) (get-child-e ts))
                                              cnd (ts/->node ts ce), newe (->id)]
                                          (-> ts
                                            (ts/add (assoc cnd :db/id newe, ::parent comp-e))
                                            (reparent-children ce newe)
                                            (delete-point-recursively e))))
                                (-> ts
                                  (ts/add {:db/id pure-e, ::parent ap-e, ::type ::pure, ::site site})
                                  (ts/add {:db/id comp-e, ::parent pure-e, ::type ::comp, ::site site})
                                  (?copy-source-map ap-e pure-e))
                                ce)))

                          (> pure-cnt 1)
                          (let [<arg*> (->box []), fsym (->sym "init-fn"), init-e (->id)
                                ts (reduce (fn [ts e]
                                             (if (= ::pure (ts/? ts e ::type))
                                               (do (-> (<arg*>) (conj [:pure (->sym "init-pure") (get-child-e ts e)]) (<arg*>))
                                                   (ts/asc ts e ::parent init-e))
                                               (let [s (->sym "arg")]
                                                 (-> (<arg*>) (conj [:arg s]) (<arg*>))
                                                 ts)))   ts ce)
                                ctor-uid (find-ctor-uid ts ap-e)
                                e (- (->id))]
                            (-> ts (ts/add {:db/id init-e, ::ctor-let-init ctor-uid,
                                            ::init-fn (fn [ts ctor-e env nm]
                                                        (let [tagged (fn tagged [tag] (fn [v] (= tag (first v))))
                                                              arg* (into [] (comp (filter (tagged :arg)) (map second)) (<arg*>))
                                                              pure* (into [] (comp (filter (tagged :pure))
                                                                               (mapcat (fn [[_t sym e]] [sym (emit ts e ctor-e env nm)]))) (<arg*>))]
                                                          `[~fsym (let* ~pure* (fn ~fsym ~arg* ~(doall (map #(nth % 1) (<arg*>)))))]))})
                              (ts/add {:db/id e, ::parent ap-e, ::type ::pure, ::site site})
                              (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v fsym, ::site site})))

                          :else ts)))
        ->ctor-idx (->->id)
        seen (volatile! #{})
        mark-used-ctors (fn mark-used-ctors [ts e]
                          (if (@seen e)
                            ts
                            (let [nd (get (:eav ts) e)]
                              (vswap! seen conj e)
                              (case (::type nd)
                                (::literal ::var ::lookup ::node ::frame) ts
                                (::ap ::comp) (reduce mark-used-ctors ts (get-children-e ts e))
                                (::site ::join ::pure ::call ::mklocal ::sitable) (recur ts (get-child-e ts e))
                                (::ctor) (if (::ctor-idx nd)
                                           ts
                                           (recur (ts/asc ts e ::ctor-idx (->ctor-idx)) (get-child-e ts e)))
                                (::localref) (recur ts (->> (::ref nd) (->localv-e ts) (get-ret-e ts)))
                                #_else (throw (ex-info (str "cannot mark-used-ctors on " (pr-str (::type nd))) (or nd {})))))))
        ts (-> ts (rewrite {true (->mark-program-order)}) (mark-used-ctors (get-root-e ts)))
        ctors-uid (mapv #(e->uid ts %) (get-ordered-ctors-e ts))
        has-node? (fn has-node? [ts uid] (ts/find ts ::ctor-ref uid))
        ensure-node (fn ensure-node [ts uid]
                      (let [nd (->node ts uid)
                            ctor-uid (::ctor-local nd) #_(e->uid ts (find-ctor-e ts (uid->e ts uid)))]
                        (cond-> ts (not (has-node? ts uid))
                                (ts/add {:db/id (->id) ::ctor-node ctor-uid, ::ctor-ref uid}))))
        ;; pctor-uid not strictly necessary in fns below, good for debugging
        ensure-free-node (fn ensure-free-node [ts uid ctor-uid pctor-uid]
                           (cond-> ts (not (ts/find ts ::ctor-free ctor-uid, ::closed-ref uid))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-uid, ::closed-ref uid
                                            ::closed-over ::node, ::pctor pctor-uid})))
        ensure-free-free (fn ensure-free-free [ts uid ctor-uid pctor-uid]
                           (cond-> ts (not (ts/find ts ::ctor-free ctor-uid, ::closed-ref uid))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-uid, ::closed-ref uid
                                            ::closed-over ::free, ::pctor pctor-uid})))
        ensure-free-frees (fn ensure-free-frees [ts uid ctors-uid]
                            (reduce (fn [ts [pctor-uid ctor-uid]]
                                      (ensure-free-free ts uid ctor-uid pctor-uid))
                              ts (partition 2 1 ctors-uid)))
        order-nodes (fn order-nodes [ts]
                      (reduce (fn [ts nodes-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::node-idx (->idx)))
                                    ts (sort-by #(get-program-order ts (uid->e ts (::ctor-ref (ts/->node ts %)))) nodes-e))))
                        ts (-> ts :ave ::ctor-node vals)))
        order-frees (fn order-frees [ts]
                      (reduce (fn [ts frees-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::free-idx (->idx)))
                                    ts (sort-by #(::pg-order (ts/->node ts %)) frees-e))))
                        ts (-> ts :ave ::ctor-free vals)))
        inline-locals (fn inline-locals [ts]
                        (reduce (fn [ts mklocal-uid]
                                  (let [mklocal-nd (ca/is (ts/->node ts (uid->e ts mklocal-uid)) (comp #{::mklocal} ::type))
                                        localrefs-e (mapv #(uid->e ts %) (::used-refs mklocal-nd))
                                        localref-e (first (ca/check #(= 1 (count %)) localrefs-e {:refs localrefs-e, :mklocal-nd mklocal-nd}))
                                        localv-e (->localv-e ts mklocal-uid)]
                                    (-> ts (move-point localv-e localref-e) (ts/del (:db/id mklocal-nd)))))
                          ts (->> ts :ave ::used-refs vals (reduce into)
                               (mapv #(e->uid ts %))
                               (remove #(has-node? ts %)))))
        literal-node? (fn [ts mklocal-uid]
                        (let [localv-e (->localv-e ts mklocal-uid)]
                          (and (= ::pure (ts/? ts localv-e ::type))
                            (let [nd (ts/->node ts (get-child-e ts localv-e))]
                              (and (= ::literal (::type nd))
                                (let [v (::v nd)]
                                  (or (string? v) (keyword? v) (number? v) (boolean? v) (nil? v)
                                    (and (seq? v) (= 'quote (first v))))))))))
        delete-ctor-nodes (fn delete-ctor-nodes [ts mklocal-uid]
                            (reduce ts/del ts (ts/find ts ::ctor-ref mklocal-uid)))
        delete-ctor-frees (fn delete-ctor-frees [ts mklocal-uid]
                            (reduce ts/del ts (ts/find ts ::closed-ref mklocal-uid)))
        inline-mklocal (fn inline-mklocal [ts mklocal-uid]
                         (let [mklocal-nd (ts/->node ts (uid->e ts mklocal-uid))
                               localrefs-e (ts/find ts ::type ::localref, ::ref mklocal-uid)
                               localv-e (->localv-e ts mklocal-uid)]
                           (-> (reduce (fn [ts localref-e]
                                         (let [pe (ts/? ts localref-e ::parent)]
                                           (-> ts
                                             (ts/del localref-e)
                                             (copy-point-recursively localv-e pe localref-e))))
                                 ts localrefs-e)
                             (delete-point-recursively (:db/id mklocal-nd))
                             (delete-ctor-nodes mklocal-uid)
                             (delete-ctor-frees mklocal-uid))))
        inline-literals (fn inline-literals [ts]
                          (reduce inline-mklocal ts
                            (eduction (map #(e->uid ts %)) (filter #(literal-node? ts %))
                              (ts/find ts ::type ::mklocal))))
        in-a-call? (fn in-a-call? [ts ref-e mklocal-e]
                     (loop [e (::parent (ts/->node ts ref-e))]
                       (when-let [nd (ts/->node ts e)]
                         (case (::type nd)
                           ::call e
                           ::ctor nil
                           #_else (when (not= e mklocal-e) (recur (::parent nd)))))))
        reroute-local-aliases (fn reroute-local-aliases [ts]
                                (reduce (fn [ts bl-e]
                                          (let [v-e (get-child-e ts bl-e), v-nd (ts/->node ts v-e)]
                                            (if (= ::localref (::type v-nd))
                                              (-> (let [bl-nd (ts/->node ts bl-e)]
                                                    (reduce (fn [ts lr-e] (ts/asc ts lr-e ::ref (::ref v-nd)))
                                                      ts
                                                      (ts/find ts ::type ::localref, ::ref (::uid bl-nd))))
                                                (delete-point-recursively bl-e))
                                              ts)))
                                  ts (ts/find ts ::type ::mklocal)))
        locals (fn locals [ts _go {e :db/id :as nd}]
                 (let [mklocal-uid (::ref nd), mklocal-e (uid->e ts mklocal-uid)
                       mklocal-nd (ts/->node ts mklocal-e)
                       localv-e (->localv-e ts mklocal-uid)
                       ts (cond-> ts (::dont-inline (meta (::k mklocal-nd)))
                                  (ensure-node mklocal-uid))
                       ts (if-some [call-e (in-a-call? ts e mklocal-e)]
                            (-> ts (ts/upd mklocal-e ::in-call #(conj (or % #{}) (e->uid ts call-e)))
                              (ensure-node mklocal-uid))
                            ts)
                       ts (ts/upd ts mklocal-e ::used-refs #(conj (or % #{}) (::uid nd)))
                       ctor (::ctor-local mklocal-nd)
                       ctor-uid* (loop [ctor-uid* '(), e e]
                                   (if-some [ctor-uid (find-ctor-uid ts e)]
                                     (if (= ctor-uid ctor)
                                       ctor-uid*
                                       (recur (cons ctor-uid ctor-uid*) (ts/? ts (uid->e ts ctor-uid) ::parent)))
                                     ctor-uid*))]
                   (if (seq ctor-uid*)  ; closed over
                     (-> ts (ensure-node mklocal-uid)
                       (ensure-free-node mklocal-uid (first ctor-uid*) ctor)
                       (ensure-free-frees mklocal-uid ctor-uid*))
                     (cond-> ts
                       (or (= 1 (count (::used-refs mklocal-nd))) ; before inc, now it's 2
                         (when-some [pt-e (find-sitable-point-e ts e)]
                           (not= (get-site ts pt-e) (get-local-site ts localv-e))))
                       (ensure-node mklocal-uid)))))
        ->call-idx (let [mp (zipmap ctors-uid (repeatedly ->->id))]
                     (fn ->call-idx [ctor-uid] ((get mp ctor-uid))))
        index-calls (fn [ts]
                      (reduce (fn [ts e] (ts/asc ts e ::call-idx (->call-idx (::ctor-call (ts/->node ts e)))))
                        ts (sort-by #(get-program-order ts %) (->> ts :ave ::ctor-call vals (reduce into)))))
        inline-return-node (fn inline-return-node [ts ret-e mklocal-uid]
                             (let [v-e (->localv-e ts mklocal-uid) v-nd (ts/->node ts v-e)
                                   pe (ts/? ts ret-e ::parent)]
                               (-> ts (ts/del ret-e) (ts/del v-e)
                                 (ts/add (assoc v-nd :db/id ret-e, ::parent pe))
                                 (?copy-source-map v-e ret-e)
                                 (reparent-children v-e ret-e)
                                 (delete-point-recursively (uid->e ts mklocal-uid))
                                 (delete-ctor-nodes mklocal-uid)
                                 (delete-ctor-frees mklocal-uid))))
        ?inline-return-node (fn ?inline-return-node [ts {ctor-e :db/id, ctor-uid ::uid}]
                              (let [ret-e (get-ret-e ts (get-child-e ts ctor-e)), ret-nd (ts/->node ts ret-e)]
                                (if (and (= ::localref (::type ret-nd))
                                      (let [mklocal-nd (ts/->node ts (ts/find1 ts ::uid (::ref ret-nd)))]
                                        (and (= ctor-uid (::ctor-local mklocal-nd))
                                          (= 1 (count (::used-refs mklocal-nd))))))
                                  (inline-return-node ts ret-e (::ref ret-nd))
                                  ts)))
        inline-return-nodes (fn inline-return-nodes [ts]
                              (rewrite ts {::ctor (fn [ts _go nd] (?inline-return-node ts nd))}))
        expand-cannot-resolve (fn [ts]
                                (reduce (fn [ts e]
                                          (let [nd (ts/->node ts e), ce (->id)]
                                            (-> ts (ts/asc e ::type ::ap)
                                              (ts/add {:db/id ce, ::parent e, ::type ::pure})
                                              (ts/add {:db/id (->id), ::parent ce, ::type ::literal
                                                       ::v `(fn* [] (r/cannot-resolve-fn '~(::var nd)))}))))
                                  ts (ts/find ts ::qualified-var `r/cannot-resolve)))
        ts (-> ts (rewrite {::call (fn [ts _go nd] (ts/asc ts (:db/id nd) ::ctor-call (::call-in-ctor nd)))})
             index-calls reroute-local-aliases (rewrite {::localref locals}) inline-locals inline-literals
             order-nodes order-frees (rewrite {::ap ap-of-pures}) inline-return-nodes expand-cannot-resolve)]
    (when (::print-db env)
      (run! prn (ts->reducible ts))
      (pprint-db ts print))
    ts))

(defn compile* [nm env ts]
  (let [ts (analyze-electric env ts)
        ret `(fn
               ([] {0 (r/ctor ~nm 0)})
               ([idx#]
                (case idx#
                  ~@(->> (get-ordered-ctors-e ts)
                      (map #(emit-ctor ts % env nm))
                      (interleave (range))))))]
    (when (and (::print-clj-source env) (= :clj (->env-type env))) (pprint-source ret))
    (when (and (::print-cljs-source env) (= :cljs (->env-type env))) (pprint-source ret))
    ret))

(defn ->ts [] (ts/->ts {::->id (->->id), ::->uid (->->id)}))

(defn compile [nm form env]
  (let [expanded (expand-all env `(::ctor ~form))]
    (when (::print-expansion env) (fipp.edn/pprint expanded))
    (compile* nm env
      (analyze expanded
        '_ env (->ts)))))

(defn ->source [env root-key efn]
  (let [expanded (expand-all env efn)
        _ (when (::print-expansion env) (fipp.edn/pprint expanded))
        ts (analyze expanded '_ env (->ts))
        ts (analyze-electric env ts)
        ctors (mapv #(emit-ctor ts % env root-key) (get-ordered-ctors-e ts))
        deps-set (emit-deps ts)
        deps (into {} (map (fn [dep] [(keyword dep) dep])) deps-set)
        source `(fn ([] ~(emit-fn ts (get-root-e ts) root-key))
                  ([idx#] (case idx# ~@(interleave (range) ctors)))
                  ([get# deps#] ~deps))]
    (when (and (::print-clj-source env) (= :clj (->env-type env))) (pprint-source source))
    (when (and (::print-cljs-source env) (= :cljs (->env-type env))) (pprint-source source))
    source))
