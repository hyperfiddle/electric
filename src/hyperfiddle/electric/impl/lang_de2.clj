(ns hyperfiddle.electric.impl.lang-de2
  (:refer-clojure :exclude [compile])
  (:require [cljs.analyzer :as cljs-ana]
            [cljs.core]
            [cljs.env]
            [clojure.string :as str]
            [contrib.assert :as ca]
            [contrib.debug]
            [contrib.triple-store :as ts]
            [dom-top.core :refer [loopr]]
            [hyperfiddle.electric :as-alias e]
            [hyperfiddle.electric.impl.analyzer :as ana]
            [hyperfiddle.electric.impl.expand :as expand]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

;;;;;;;;;;;;;;;;
;;; EXPANDER ;;;
;;;;;;;;;;;;;;;;

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

(defn find-local-entry [env sym] (find (:locals env) sym))
(defn add-local [env sym] (update env :locals assoc sym ::unknown))

(def ^:dynamic *electric* true)

(defn ?meta [metao o]
  (if (instance? clojure.lang.IObj o)
    (cond-> o (meta metao) (vary-meta #(merge (meta metao) %)))
    o))

(defn -expand-all [o env]
  (cond
    (and (seq? o) (seq o))
    (if (find-local-entry env (first o))
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
                 (?meta o (list 'let* bs2 (-expand-all (?meta body (cons 'do body)) env2))))

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
                                      (list syms (-expand-all (cons 'do body) (reduce add-local env syms))))))
                             arities))))

        (letfn*) (let [[_ bs & body] o
                       env2 (reduce add-local env (take-nth 2 bs))
                       xpand (fn-> -expand-all env2)
                       bs2 (into [] (comp (partition-all 2)
                                      (mapcat (fn [[sym v]] [sym (binding [*electric* false] (xpand v))])))
                             bs)]
                   (?meta o `(let* [~(vec (take-nth 2 bs2)) (::letfn ~bs2)] ~(-expand-all (cons 'do body) env2))))

        ;; TODO expand `do`
        (try) (throw (ex-info "try is TODO" {:o o})) #_(list* 'try (mapv (fn-> -all-in-try env) (rest o)))

        (binding clojure.core/binding)
        (let [[_ bs & body] o]
          (?meta o (list 'binding (into [] (comp (partition-all 2) (mapcat (fn [[sym v]] [sym (-expand-all v env)]))) bs)
                     (-expand-all (cons 'do body) env))))

        (set!) (if *electric*
                 (recur (?meta o `((fn* [v#] (set! ~(nth o 1) v#)) ~(nth o 2))) env)
                 (?meta o (list 'set! (-expand-all (nth o 1) env) (-expand-all (nth o 2) env))))

        (::site) (?meta o (seq (conj (into [] (take 2) o)
                                   (-expand-all (cons 'do (drop 2 o)) (assoc env ::current (second o))))))

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
      (if (find-local-entry env (first o))
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

;;;;;;;;;;;;;;;;
;;; COMPILER ;;;
;;;;;;;;;;;;;;;;

(defn mksym [x & xs]
  (if (or (symbol? x) (keyword? x))
    (symbol (namespace x) (apply str (name x) (map name (flatten xs))))
    (symbol (apply str (name x) (map name (flatten xs))))))

(defn fail!
  ([env msg] (fail! env msg {}))
  ([env msg data] (throw (ex-info (str "in" (some->> (::def env) (str " ")) ": " (-> env ::last peek pr-str) "\n" msg)
                           (merge {:form (-> env ::last pop peek) :in (::def env) :for ((juxt ::me ::current) env)} data)))))

(defn get-them [env] (-> env ::peers keys set (disj (::current env)) first))

(defn cannot-resolve! [env form]
  (fail! env (str "I cannot resolve " "`"form"`"
               (when-let [them (get-them env)]
                 (let [site (name them)]
                   (str ", maybe it's defined only on the " site "?"
                     \newline "If `" form "` is supposed to be a macro, you might need to :refer it in the :require-macros clause."))))
    {:locals (keys (:locals env))}))

(defn ns-qualify [node] (if (namespace node) node (symbol (str *ns*) (str node))))

(tests
  (ns-qualify 'foo) := `foo
  (ns-qualify 'a/b) := 'a/b)

(defn qualify-sym-in-var-node "If ast node is `:var`, update :form to be a fully qualified symbol" [env ast]
  (if (and (= :var (:op ast)) (not (-> ast :env :def-var)))
    (assoc ast :form (case (get (::peers env) (::current env))
                       :clj  (symbol (str (:ns (:meta ast))) (str (:name (:meta ast))))
                       :cljs (:name (:info ast))))
    ast))

(defn find-local [f env] "TODO" nil)
(defn find-electric-local [o env] "TODO" nil)

(defn ->meta [o env] (merge (::meta (find-electric-local o env)) (meta o)))

(defn closure
  "Analyze a cc/fn form, looking for electric defs and electric lexical bindings references.
  Rewrites the cc/fn form into a closure over electric dynamic and lexical scopes.
  Return a pair [closure form, references to close over].

  e.g.:
  (let [x 1]
    (binding [y 2]
      (fn [arg] [x y arg])))

   =>
  [(fn [x123 y123]
     (fn [& rest-args123]
       (binding [y y123]
         (let [x x123]
           (apply (fn [arg] [x y arg]) rest-args123)))))
   [x y]]
  "
  [env form]
  (let [refered-evars   (atom {})
        refered-lexical (atom {})
        edef?           (fn [ast] (or (#{::node ::node-signifier} (-> ast :meta ::type))
                                    (#{::node ::node-signifier} (-> ast :info :meta ::type))))
        dynamic?        (fn [ast] (or (:assignable? ast) ; clj
                                    (:dynamic (:meta (:info ast))) ; cljs
                                    ))
        lexical?        (fn [ast] (or (::provided? ast) ; clj
                                    (::provided? (:info ast)) ;cljs
                                    ))
        namespaced?     (fn [ast] (qualified-symbol? (:form ast)))
        safe-let-name   (fn [sym] (if (qualified-symbol? sym)
                                    (symbol (str/replace (str (munge sym)) #"\." "_"))
                                    sym))
        record-lexical! (fn [{:keys [form]}]
                          (swap! refered-lexical assoc (with-meta form (->meta form env))
                            (gensym (name form))))
        record-edef!    (fn [{:keys [form] :as ast}]
                          (if (dynamic? ast)
                            (swap! refered-evars assoc form #_(ana/var-name ast) (gensym (name form)))
                            (record-lexical! ast)))
        env             (update env :locals update-vals #(if (map? %) (assoc % ::provided? true) {::provided? true}))
        rewrite-ast     (fn [ast]
                          (cond
                            (edef? ast)    (do (record-edef! ast)
                                               (cond (dynamic? ast)    (qualify-sym-in-var-node env ast)
                                                     (namespaced? ast) (update ast :form safe-let-name)
                                                     :else             ast))
                            (lexical? ast) (do (record-lexical! ast) ast)
                            :else          (qualify-sym-in-var-node env ast)))
        form            (case (get (::peers env) (::current env))
                          :clj  (-> (ana/analyze-clj env form)
                                  (ana/walk-clj rewrite-ast)
                                  (ana/emit-clj))
                          :cljs (-> (binding [cljs.analyzer/*cljs-warning-handlers*
                                              [(fn [_warning-type _env _extra])]]
                                      (ana/analyze-cljs env form))
                                  (ana/walk-cljs rewrite-ast)
                                  (ana/emit-cljs)))
        rest-args-sym   (gensym "rest-args")
        all-syms        (merge @refered-evars @refered-lexical)
        [syms gensyms]  [(keys all-syms) (vals all-syms)]
        fn?             (and (seq? form) (#{'fn 'fn* 'clojure.core/fn 'clojure.core/fn* 'cljs.core/fn 'cljs.core/fn*} (first form)))
        form            (if fn?
                          `(apply ~form ~rest-args-sym)
                          form)
        form            (if (seq @refered-lexical)
                          `(let [~@(flatten (map (fn [[k v]] [(safe-let-name k) v]) @refered-lexical))]
                             ~form)
                          form)
        form            (if (seq @refered-evars)
                          `(binding [~@(flatten (seq @refered-evars))]
                             ~form)
                          form)
        form            (if fn?
                          `(fn [~@gensyms] (fn [~'& ~rest-args-sym] ~form))
                          `(fn [~@gensyms] ~form))]
    [form syms]))

(defn bound-js-fn
  "Given a js global resolving to a function (e.g js/alert, js/console.log required-js-ns/js-fn), ensures it
  is called under the correct `this` context."
  [sym]
  (let [fields (str/split (name sym) #"\.")]
    `(.bind ~sym ~(symbol (namespace sym)
                    (if (seq (rest fields))
                      (str/join (interpose '. (butlast fields)))
                      "globalThis")))))

(defn class-constructor-call? [env f] (and (symbol? f) (not (find-local f env))))
(defn with-interop-locals [env syms] (update env :locals merge (zipmap syms (repeat {}))))

(defn resolve-static-field [sym]
  (when-some [ns (some-> (namespace sym) symbol)]
    (when-some [cls (resolve ns)]
      (when (class? cls)
        (clojure.lang.Reflector/getField cls (name sym) true)))))

(defn get-children-e [ts e] (-> ts :ave ::parent (get e)))
(defn get-root-e [ts] (first (get-children-e ts '_)))

(defn find-let-ref [sym pe ts]
  (loop [pe pe]
    (when pe
      (let [p (ts/get-entity ts pe)]
        (if (and (= ::let (::type p)) (= sym (::sym p)))
          pe
          (recur (::parent p)))))))

(defn ?add-source-map [{{::keys [->id]} :o :as ts} pe form]
  (let [mt (meta form)]
    (cond-> ts (:line mt) (ts/add {:db/id (->id), ::source-map-of pe, ::line (:line mt), ::column (:column mt)}))))

(defn analyze [form pe {{::keys [env ->id]} :o :as ts}]
  (cond
    (and (seq? form) (seq form))
    (case (first form)
      (let*) (let [[_ bs bform] form]
               (loopr [ts ts, pe pe]
                 [[s v] (eduction (partition-all 2) bs)]
                 (let [e (->id)]
                   (recur (analyze v e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::let, ::sym s})
                                         (update-in [:o ::env :locals s] assoc ::electric-let true, :db/id e)
                                         (?add-source-map e form))) e))
                 (analyze bform pe ts)))
      (case) (let [[_ test & brs] form
                   [default brs2] (if (odd? (count brs)) [(last brs) (butlast brs)] [:TODO brs])]
               (loopr [bs [], mp {}]
                 [[v br] (partition 2 brs2)]
                 (let [b (gensym "case-val")]
                   (recur (conj bs b `(::ctor ~br))
                     (reduce (fn [ac nx] (assoc ac (list 'quote nx) b)) mp (if (seq v) v [v]))))
                 (recur (?meta form `(let* ~bs (::call (~mp ~test (::ctor ~default))))) pe ts)))
      (quote) (ts/add ts {:db/id (->id), ::parent pe, ::type ::static, ::v form})
      (fn*) (let [e (->id), ce (->id)
                  [form refs] (closure env form)
                  ts2 (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
                        (?add-source-map e form)
                        (ts/add {:db/id ce, ::parent e, ::type ::static, ::v form}))]
              (reduce (fn [ts nx] (analyze nx e ts)) ts2 refs))
      (::ctor) (let [e (->id)] (recur (second form) e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ctor})
                                                        (?add-source-map e form))))
      (::call) (let [e (->id)] (recur (second form) e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::call})
                                                        (?add-source-map e form))))
      (::pure) (let [e (->id)] (recur (second form) e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::pure})
                                                        (?add-source-map e form))))
      (::join) (let [e (->id)] (recur (second form) e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::join})
                                                        (?add-source-map e form))))
      (::site) (let [[_ site bform] form, e (->id)]
                 (recur bform e (-> (ts/add ts {:db/id e, ::parent pe, ::type ::site, ::site site})
                                  (?add-source-map e form))))
      #_else (let [e (->id)]
               (reduce (fn [ts nx] (analyze nx e ts)) (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
                                                        (?add-source-map e form)) form)))

    (vector? form) (recur (?meta form (cons `vector form)) pe ts)
    (map? form) (recur (?meta form (cons `hash-map (eduction cat form))) pe ts)

    (symbol? form)
    (let [e (->id)]
      (if-some [lr-e (find-let-ref form pe ts)]
        (-> (ts/add ts {:db/id e, ::parent pe, ::type ::let-ref, ::ref lr-e, ::sym form})
          (?add-source-map e form))
        (case (get (::peers env) (::current env))
          :clj (if (resolve-static-field form)
                 (-> (ts/add ts {:db/id e, ::parent pe, ::type ::static, ::v form})
                   (?add-source-map e form))
                 (if-some [v (resolve form)]
                   (if (var? v)
                     (-> (ts/add ts {:db/id e, ::parent pe, ::type ::var, ::var (symbol v)})
                       (?add-source-map e form))
                     (-> (ts/add ts {:db/id e, ::parent pe, ::type ::static, ::v form})
                       (?add-source-map e form)))
                   (cannot-resolve! env form)))
          :cljs (expand/resolve-cljs env form)
          #_else (throw (ex-info (str "unknown site: " (get (::peers env) (::current env))) {:env env})))))

    :else
    (let [e (->id)]
      (-> (ts/add ts {:db/id e, ::parent pe, ::type ::static, ::v form})
        (?add-source-map e form)))))

(defn ->->id [] (let [!i (long-array [-1])] (fn [] (aset !i 0 (unchecked-inc (aget !i 0))))))

(defn get-site [ts e]
  (loop [pe (::parent (get (:eav ts) e))]
    (and pe
      (let [nd (get (:eav ts) pe)] (if (= ::site (::type nd)) (::site nd) (recur (::parent nd)))))))

(defn compile
  ([nm form env]
   (let [{{::keys [env ->id]} :o :as ts} (analyze (expand-all env form) '_ (ts/->ts {::->id (->->id), ::env env}))
         find-return-node-e (fn [ts e]
                              (let [nd (get (:eav ts) e)]
                                (case (::type nd)
                                  ::let (recur ts (second (get-children-e ts e)))
                                  ::site (recur ts (first (get-children-e ts e)))
                                  #_else e)))
         ret-e (find-return-node-e ts (get-root-e ts))
         ->ref-id (->->id)
         count-nodes (fn count-nodes [ts e]
                       (let [nd (get (:eav ts) e)]
                         (case (::type nd)
                           ::static ts
                           ::ap (reduce count-nodes ts (get-children-e ts e))
                           ::site (recur ts (first (get-children-e ts e)))
                           ::var ts
                           ::let-ref
                           (let [used (::used (get (:eav ts) (::ref nd)))]
                             (cond-> (ts/upd ts (::ref nd) ::used #(conj (or % #{}) e))
                               (nil? used) (recur (find-return-node-e ts (first (get-children-e ts (::ref nd))))))))))
         index-nodes (fn index-nodes [ts e]
                       (let [nd (get (:eav ts) e)]
                         (case (::type nd)
                           ::static ts
                           ::ap (reduce index-nodes ts (get-children-e ts e))
                           ::site (recur ts (first (get-children-e ts e)))
                           ::var ts
                           ::let-ref (recur (if-some [used (::used (get (:eav ts) (::ref nd)))]
                                              (if (or (> (count used) 1)
                                                    (not= (get-site ts e) (get-site ts (::ref nd))))
                                                (ts/upd ts (::ref nd) ::refidx #(or % (->ref-id)))
                                                ts)
                                              ts)
                                       (find-return-node-e ts (first (get-children-e ts (::ref nd)))))
                           #_else (throw (ex-info (str "cannot index-nodes " (::type nd)) nd)))))
         ts (-> ts (count-nodes ret-e) (index-nodes ret-e))
         gen (fn gen [ts e]
               (let [nd (get (:eav ts) e)]
                 (case (::type nd)
                   ::static (list `r/pure (::v nd))
                   ::ap (list* `r/ap (mapv #(gen ts %) (get-children-e ts e)))
                   ::var (list `r/lookup 'frame (keyword (::var nd)) (list `r/pure (::var nd)))
                   ::let (recur ts (find-return-node-e ts (second (get-children-e ts e))))
                   ::let-ref (if-some [idx (::refidx (get (:eav ts) (::ref nd)))]
                               (list `r/node 'frame idx)
                               (gen ts (find-return-node-e ts (first (get-children-e ts (::ref nd)))))))))
         nodes (mapv (fn [[idx es]] [idx (first es)]) (sort-by first (::refidx (:ave ts))))
         gen-node-init (fn gen-node-init [ts]
                         (mapv (fn [[idx e]] (list `r/define-node 'frame idx
                                               (gen ts (find-return-node-e ts (first (get-children-e ts e))))))
                           nodes))
         ]
     ;; (run! prn (->> ts :eav vals (sort-by :db/id)))
     (let [e (find-return-node-e ts (get-root-e ts))]
       `[(r/cdef 0 ~(mapv #(get-site ts (second %)) nodes) [] ~(get-site ts e)
           (fn [~'frame] ~@(gen-node-init ts) ~(gen ts e)))])
     )))
