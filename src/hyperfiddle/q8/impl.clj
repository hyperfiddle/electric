(ns hyperfiddle.q8.impl
       (:refer-clojure :exclude [bound?])
       (:require
        [cljs.analyzer :as cljs]
        [clojure.set :as set]
        [clojure.string :as str]
        [clojure.tools.analyzer.jvm :as clj]
        [hfdl.lang :as p]
        [hyperfiddle.api :as hf] ;; should it be :as-alias?
        [hyperfiddle.q8.lib :refer [read-only? reference render set-ref!]]
        [hyperfiddle.spec :as spec]
        [hyperfiddle.walk :as walk])
       (:import
        (clojure.lang IObj Box Var Compiler)))

(defn meta-keyword? [k] (= ::MetaKeyword (::type (meta k))))

(defn kw [named]
  (if (meta-keyword? named)
    named
    (vary-meta (symbol named) assoc ::type ::MetaKeyword)))

(defn props? [form]
  (and (seq? form)
       (= '. (second form))
       (even? (count (drop 2 form)))
       (every? meta-keyword? (map first (partition 2 (drop 2 form))))))

(defn parse-props [[_form _dot & props]] (apply hash-map props))

(defn parse-hydrated-defaults [form] form)

(defn parse [form]
  (walk/postwalk (fn [form]
                   (cond
                     (keyword? form) (kw form)
                     (props? form)   (vary-meta (first form) merge (parse-props form))
                     (seq? form)     (parse-hydrated-defaults form)
                     :else           form))
                 form))

(defn categorize [node]
  (condp #(%1 %2) node
    symbol?       :symbol
    seq?          :seq
    vector?       :vector
    map?          :map))

(defn node-point [node]
  (case (categorize node)
    :symbol      node
    :seq         (recur (first node))
    :vector      nil ;; Can’t refer to edges form
    :map         (recur (ffirst node))))

(defn node-hash [node]
  (case (categorize node)
    (:symbol :seq) (hash [node (meta node)])
    :vector        (hash (map node-hash node))
    :map           (hash [(node-hash (key (first node))) (node-hash (key (first node)))])))

(defn meta? [x] (instance? IObj x))

(defn node-name [node] (some-> (node-point node) (str/replace  #"\/" "_")))

(defn identifier
  ([node] (identifier node true))
  (^:deprecated [node alias?]
   (when-let [id (if (and alias? (meta? node) (get (meta node) `hf/as))
                   (get (meta node) `hf/as)
                   (case (categorize node)
                     :symbol (symbol (str "nav__" (node-name node) "_" (node-hash node)))
                     :seq    (symbol (str "call__" (node-name node) "_" (node-hash node)))
                     :vector (symbol (str "collect__" (node-hash node)))
                     :map    (symbol (str "traverse__" (node-name node) "_" (node-hash (key (first node))) "->" (node-hash (val (first node)))))))]
     (vary-meta id assoc ::node node))))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))))

(defn resolve-ref [deps ref]
  (if (contains? deps ref)
    (key (find deps ref)) ;; symbol in deps has extra meta
    (first (filter #(= ref (node-point (::node (meta %)))) (keys deps)))))

(defn references [deps form]
  (letfn [(collect [r form] (cond
                              ;; recursion
                              (map? form)                   (apply set/union (map (partial collect r) form))
                              (map-entry? form)             (-> (collect r (key form)) (collect (val form)))
                              (vector? form)                (apply set/union (map (partial collect r) form))
                              (seq? form)                   (apply set/union (map (partial collect r) (rest form)))
                              ;; collect
                              (and (reference? form)
                                   (resolve-ref deps form)) (conj r form)
                              :else                         r))]
    (if (and (meta? form) (seq (meta form)))
      (if (meta-keyword? form)
        (collect #{} (dissoc (meta form) `hf/as))
        (-> (collect #{} (with-meta form nil))
            (collect (dissoc (meta form) `hf/as))))
      (case (categorize form)
        :seq    (apply set/union (map (partial collect #{}) (rest form)))
        :symbol (collect #{} (meta form))
        :map    (references deps (ffirst form))
        #{}))))

(defn children [node]
  (case (categorize node)
    :vector node
    :map    (children (val (first node)))
    nil))

(defn dependencies
  ([node] (dependencies {} node))
  ([r node]
   (if (nil? node)
     r
     (let [children (children node)
           r        (assoc r (identifier node) (set (mapv identifier children)))]
       (cond
         (= node children)  (reduce dependencies r children)
         (vector? children) (reduce dependencies (assoc r (identifier node) #{(identifier children)}
                                                        (identifier children) (set (mapv identifier children))) children)
         :else              (reduce dependencies r children))))))

(defn reverse-deps [deps]
  (let [points (apply set/union (set (keys deps)) (vals deps))
        r      (zipmap points (repeat (count points) #{}))]
    (reduce-kv (fn [r dep deps]
                 (reduce (fn [r' dep'] (update r' dep' conj dep))
                         r deps))
               r deps)))

(defn ancestors' [deps⁻¹ point]
  (let [parents (get deps⁻¹ point)]
    (if (seq parents)
      (apply set/union parents (map (partial ancestors' deps⁻¹) parents))
      parents)))

(defn card-n? [node]
  (when-let [point (node-point (::node (meta node)))]
    (= ::spec/many (spec/cardinality point))))

(defn crossed-card-n-boundary
  "Returns a set of card-n nodes, `a` have to cross to refer to `b`. Which is forbidden."
  [deps referencing-dep refered-dep]
  (let [deps⁻¹             (reverse-deps deps)
        card-n-ancestors-a (set (filter card-n? (ancestors' deps⁻¹ referencing-dep)))
        card-n-ancestors-b (set (filter card-n? (ancestors' deps⁻¹ refered-dep)))]
    (set/difference card-n-ancestors-b card-n-ancestors-a)))

(defn check-card-n-crossed-boundaries! [deps node]
  (let [refs    (references deps (::node (meta node)))
        crosses (->> refs
                     (map (fn [ref] (resolve-ref deps ref)))
                     (filter (fn [dep] (symbol? (node-point (::node (meta dep))))))
                     (map (fn [dep] (crossed-card-n-boundary deps node dep)))
                     (apply set/union))]
    (when (seq crosses)
      (throw (ex-info "Can't reference through cardinality many"
                      {:references         refs
                       :referencing-node   (::node (meta node))
                       :crossed-boundaries (set (map (comp ::node meta) crosses))})))))

(defn ancestor? "is a ancestor of b" [deps⁻¹ a b] (contains? (ancestors' deps⁻¹ b) a))

(defn closest-common-parent [deps point-a point-b]
  (let [deps⁻¹ (reverse-deps deps)]
    (first (sort (comparator (complement (partial ancestor? deps⁻¹)))
                 (set/intersection (ancestors' deps⁻¹ point-a) (ancestors' deps⁻¹ point-b))))))

(defn tag-ref-points [deps]
  (reduce-kv (fn [r k _v]
               (reduce (fn [r ref]
                         (let [ref-point (resolve-ref deps ref)]
                           (if-let [parent (closest-common-parent deps k ref-point)]
                             (let [actual (key (find r parent))]
                               (-> (dissoc r actual) ;; assoc on an existing key will reuse the key, ignoring new metas (https://github.com/clojure/clojure/blob/84811650bb33846c7212f391468f9661819b906b/src/jvm/clojure/lang/PersistentHashMap.java#L705)
                                   (assoc (vary-meta actual update ::refs (fnil conj #{}) ref-point) (get r parent))))
                             r))) r (references deps (::node (meta k)))))
             deps deps))

(defn add-ref-deps [deps]
  (let [deps (tag-ref-points deps)] ;; must happen BEFORE we register reference dependencies
    (reduce-kv (fn [r k v]
                 (if-some [refs (seq (references deps (::node (meta k))))]
                   (do
                     (check-card-n-crossed-boundaries! deps k)
                     (assoc r k (into v (filter some? (map (partial resolve-ref deps) refs)))))
                   r))
               deps deps)))

(defn rank [deps point]
  (let [children (get deps point)]
    (if (= #{} children)
      0
      (apply max (map #(inc (rank deps %)) children)))))

(defn toposort [deps]
  (reduce-kv (fn [r k _v] (assoc r k (rank deps k)))
             {} deps))

;;;;;;;;;;
;; EMIT ;;
;;;;;;;;;;

(defn symbolic [node]
  (case (categorize node)
    :symbol (if (meta-keyword? node)
              (keyword node)
              node)
    :map    (symbolic (ffirst node))
    :seq    (list 'quote node)))

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(defn escape [form]
  (cond (quoted? form) form
        (symbol? form) (list 'quote form)
        (seq? form)    (list 'quote form)
        :else          form))

(defn replace* "Recursive `clojure.core/replace`"
  [smap coll]
  (if (empty? smap) coll (walk/prewalk (fn [form]
                                         (if (quoted? form)
                                           (reduced form)
                                           (if-let [subst (get smap form)]
                                             (reduced subst)
                                             form)))
                                       coll)))

(defn emit-inputs [deps node]
  (walk/postwalk (fn [form]
                   (let [form' form
                         form' (or (and (symbol? form) (::free? (meta form)) (not (resolve-ref deps form))
                                        `(get hf/args ~(list 'quote form')))
                                   form')
                         form' (or (and (symbol? form) (`hf/default (meta form))
                                        `(or ~form' (unquote ~(`hf/default (meta form)))))
                                   form')]
                     form'))
                 node))

(defn emit-continuation [deps node]
  (replace* {(identifier node) (identifier node false)} ;; TODO clarify alias vs real name
            (case (categorize node)
              :symbol `(hf/nav hf/entity ~(keyword node))
              :seq    (if (quoted? node)
                        `(apply list ~(second node))
                        (cons `p/$ (emit-inputs deps node)))
              :vector (into {} (map (fn [child] [(symbolic child) (identifier child false)]) node)))))

(defn referencing [deps node]
  (let [id (identifier node)]
    (->> (get (reverse-deps deps) id)
         (filter (fn [parent] (contains? (references deps (::node (meta parent))) id)))
         (set))))

;; (referencing (add-ref-deps (dependencies (parse '[{:user/gender [(:db/ident . ::hf/as gender)]}
;;                                                   {(:user/shirt-size . ::hf/options (shirt-sizes gender)) [:db/ident]}])))
;;              (parse '(:db/ident . ::hf/as gender)))

(defn- emit-reference-set! [deps node expr]
  (if (seq (referencing deps node))
    `(p/$ set-ref! '~(identifier node) ~expr)
    expr))

(defn map-keys [f m] (reduce-kv (fn [r k v] (assoc r (f k) v)) (empty m) m))

(defn maybe-update [k f & args]
  (let [m    (last args)
        args (butlast args)]
    (if (contains? m k) (apply update m k f args) m)))


(defn normalize-fn [form]
  (cond (symbol? form) form
        (seq? form)    (case (first form)
                         var         (recur (second form))
                         hfdl.lang/$ (rest form)
                         form)
        :else          form))

(declare emit-node)

;; FIXME WRONG! should be done during the deps phase. options are a node and depends on the continuation at point.
;; during emit phase we should emit a let binding for options and refer to it from ::hf/options.
;; Should also bypass user-provided renderers
(defn ^:deprecated emit-options [deps continuation options]
  (if (some? continuation) ;; FIXME should be a child dep in deps map
    `#'(binding [read-only? true]
         ~(emit-node deps {(normalize-fn options) continuation}))
    (emit-inputs deps options)))

(defn emit-props
  ([deps node] (emit-props deps node nil))
  ([deps node continuation]
   (->> (dissoc (meta node) ::type)
        (map-keys keyword)
        (maybe-update ::hf/as #(list 'quote %))
        (maybe-update ::hf/options #(emit-options deps continuation %))
        (replace* (into {} (map (fn [ref] [ref `(unquote (get hf/refs '~ref))]) (references deps node))))
        (walk/postwalk (fn [form] (if (meta-keyword? form) (keyword form) form))))))

(defn join [sexpr]
  (if (and (seq? sexpr) (#{'unquote `p/$} (first sexpr)))
    sexpr
    `(unquote ~sexpr)))

(defn cardinality [node]
  (let [point (node-point node)]
    (spec/cardinality (if (meta-keyword? point) (keyword point) point))))

(defn emit-register-refs [deps point]
  (let [point (key (find deps point))]
    (if-some [refs (seq (::refs (meta point)))]
      `(assoc hf/refs ~@(mapcat (fn [ref] `['~ref (reference nil)]) refs))
      `hf/refs)))

(defn emit-node [deps node] ;; FIXME should be `point`, not `node`
  (case (categorize node)
    :symbol `(binding [hf/props     ~(emit-props deps node)
                       hf/attribute ~(symbolic node)
                       hf/value     #'~(emit-reference-set! deps node (join (emit-continuation deps node)))]
               (unquote render))
    :vector `(binding [hf/refs    ~(emit-register-refs deps (identifier node))
                       hf/columns ~(mapv symbolic node)
                       hf/value   #'~(emit-reference-set! deps node (emit-continuation deps node))]
               (unquote render))
    :map    (let [[clef value] (first node)
                  traverse     (emit-reference-set! deps clef (join (emit-continuation deps clef)))
                  inputs       (->> (tree-seq coll? identity (symbolic node))
                                    (filter (fn [form] (and (symbol? form) (::free? (meta form)))))
                                    (map symbolic)
                                    (mapv escape))]
              `(binding [hf/props     ~(emit-props deps clef value)
                         hf/attribute ~(symbolic node)
                         hf/inputs    ~inputs
                         hf/args      ~(into {} (map (fn [input] [input nil]) inputs)) ;; protect children from parent scope.
                         ]
                 ~(if (= ::spec/many (cardinality node))
                    `(binding [hf/columns ~(mapv symbolic value)
                               hf/value   #'(p/for [e# ~traverse]
                                              #'(binding [hf/entity e#]
                                                  (unquote ~(identifier value false))))]
                       (unquote render))
                    `(binding [hf/entity ~traverse]
                       (unquote ~(identifier value false))))))
    :seq (if (quoted? node) ;; link?
           `(binding [hf/props     ~(emit-props deps node)
                      hf/value     #'~(emit-continuation deps node)]
              (unquote render))
           (throw (ex-info "not implemented (inline-call)" {:node node}))
           )))

;; (emit (add-ref-deps (dependencies (parse '[(:db/id . ::hf/as id) '(google id)]))))


(defn emit [deps]
  (let [bindings (->> (toposort deps)
                      (sort-by val <)
                      (map (fn [[point _]] (let [node (::node (meta point))]
                                             [(identifier node false) `#'~(emit-node deps node)]))))]
    `(let [~@(mapcat identity bindings)] (unquote ~(first (last bindings))))))

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

(defn resolve-ns-alias [env sym]
  (let [ns  (namespace sym)
        nom (name sym)]
    (when-let [qualified (get (::aliases env) ns)]
      (symbol qualified nom))))


(defn resolve-var [env sym]
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

(defn resolve-runtime "
  Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if the var doesn't exist or
  is a macro or special form."
  [env sym]
  (if (:js-globals env)
    (let [b (Box. true)
          v (cljs/resolve-var env sym (fn [env prefix suffix] (cljs/confirm-var-exists env prefix suffix (fn [_ _ _]
                                                                                                           #_(tap> [sym env])
                                                                                                           (set! (.-val b) false)))))]
      ;; (tap> [sym (.-val b) v])
      (if (.-val b)
        (:name v)
        (when-let [^Var v (resolve-var env sym)]
          (.toSymbol v))))
    (let [v (resolve-var env sym)]
      (or (when (instance? Var v) (.toSymbol ^Var v))
          (when (instance? Class v) (symbol (.getName ^Class v)))))))

(defn resolve' [env sym]
  (assert (symbol? sym) (str "Can’t resolve " (pr-str sym)))
  (doto (resolve-runtime env sym) (tap>))
  #_(if (:js-globals env)
    (let [b (Box. true)
          v (cljs.analyzer/resolve-var env sym (fn [env prefix suffix] (cljs.analyzer/confirm-var-exists env prefix suffix (fn [_ _ _] (tap> [:missing sym]) (set! (.-val b) false)))))]
      (let [res (when (.-val b) (:name v) sym)]
        (tap> [sym '-> res 'in v])
        res))
    (or (resolve-ns-alias env sym)
        (let [?var (get (::globals env) sym)]
          (when (var? ?var)
            (symbol ?var))))))

(defn local? [env sym] (contains? (:locals env) sym))

(defn normalize-env [env]
  (if (:js-globals env)
    env {:ns (ns-name *ns*) :locals env}))

(defn make-env [&env]
  (if (:js-globals &env)
    &env
    (normalize-env (into {} (reduce-kv (fn [r k v] (if (instance? clojure.lang.Compiler$LocalBinding v)
                                                     (assoc r (.-sym v) (.-sym v))
                                                     (assoc r k v))) {} &env)))
    #_(-> (assoc env ::globals (ns-map *ns*))
          (assoc ::aliases (reduce-kv (fn [r k v] (assoc r (str k) (str v))) {} (ns-aliases *ns*)))
          (vary-meta assoc ::type ::env))))

(def special? '#{try finally loop* do letfn* if new let* fn* recur set! . var quote catch throw def})

(defn resolve-syms [env form]
  (walk/prewalk (fn [form]
                  (cond
                    (quoted? form)           (reduced form)
                    (and (symbol? form)
                         (not (#{'.} form))) (do (tap> [:will-resolve form])
                                                 (if-let [var (resolve' env form)]
                                                     (symbol var)
                                                     (if (local? env form)
                                                       form
                                                       (if-not (special? form)
                                                         (vary-meta form assoc ::free? true)
                                                         form))))
                    :else                    form))
                form))

(defn hfql [&env form]
  (let [form (if (map? form) [form] form)] ;; Wrap top level maps to collect their keys. […] is in charge of collecting.
    (emit (add-ref-deps (dependencies (parse (resolve-syms (make-env &env) form)))))))

