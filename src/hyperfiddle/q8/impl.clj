(ns ^{:clj-kondo/config '{:lint-as {hyperfiddle.q8.impl/condf clojure.core/condp}}}
    hyperfiddle.q8.impl
  (:refer-clojure :exclude [bound? munge ancestors])
  (:require
   [cljs.analyzer :as cljs]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.analyzer.jvm :as clj]
   [clojure.tools.reader :as reader]
   [hfdl.lang :as p]
   [hyperfiddle.api :as hf] ;; should it be :as-alias?
   [hyperfiddle.q8.lib :refer [read-only? reference render set-ref!]]
   [hyperfiddle.spec :as spec]
   [hyperfiddle.walk :as walk])
  (:import
   (clojure.lang IObj Box Var Compiler)))

;; syntax https://gist.github.com/dustingetz/20fbb48773130cf4f91c9d0fc373a737

(defmacro ^{:style/indent 1} condf [expr & clauses] `(condp #(%1 %2) ~expr ~@clauses))

(defn quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn replace* "Recursive `clojure.core/replace`"
  [smap coll]
  (if (empty? smap) coll (walk/prewalk (fn [form]
                                         (if (quoted? form)
                                           (reduced form)
                                           (if-let [subst (get smap form)]
                                             (reduced subst)
                                             form)))
                                       coll)))

(defmacro ^{:style/indent 1} post-cond [expr & clauses] (let [form (gensym)] `(walk/postwalk (fn [~form] (condf ~form ~@(replace* {expr form} clauses))) ~expr)))

(defn meta-keyword? [sym] (= ::MetaKeyword (::type (meta sym))))

(defn meta-keyword
  ([named] (meta-keyword named nil))
  ([named metas] (if (meta-keyword? named) named (vary-meta (symbol named) merge metas {::type ::MetaKeyword}))))

(defn props? [form]
  (and (seq? form)
       (= 3 (count form))
       (= 'props (first form))
       (map? (last form))))

(defn parse-props [[_props form metas]] (vary-meta form merge metas))

(defn parse-hydrated-defaults [form] form)

(defn parse [form]
  (post-cond form
    keyword? (meta-keyword form)
    props?   (parse-props form)
    ;; (seq? form)     (parse-hydrated-defaults form)
    form))

(defn categorize [form]
  (condf form
    symbol?  :symbol
    seq?     :seq
    vector?  :vector
    map?     :map
    nil))

(defn identifier [form]
  (case (categorize form)
    :symbol form
    :seq    (recur (first form))
    :map    (recur (ffirst form))
    nil))

(defn form-hash [form]
  (case (categorize form)
    (:symbol :seq) (hash [form (meta form)])
    :vector        (hash (map form-hash form))
    :map           (hash [(form-hash (key (first form))) (form-hash (val (first form)))])))

(defn munge [point] (str/replace (str point) #"\/" "_"))

(defn meta? [x] (instance? IObj x))

(defn describe [form]
  (let [id (identifier form)]
    (when-let [p (case (categorize form)
                   :symbol {::role :nav}
                   :seq    {::role :call}
                   :vector {::role :collect}
                   :map    {::role :traverse}
                   nil)]
      (assoc p ::id id, ::form form))))

(defn point [form]
  (when-let [d (describe form)]
    (let [{::keys [role id form from to]} d
          p                               (case role
                                            (:nav :call :traverse) (str (name role) "__" (munge id) "_" (form-hash form))
                                            :collect               (str (name role) "__" (form-hash form)))]
      (vary-meta (symbol p) merge d))))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))))

(defn form  [point] (::form (meta point)))
(defn alias [point] (`hf/as (meta (form point))))

;; FIXME missing user/gender ref
(defn resolve-ref [deps ref-sym]
  (if (contains? deps ref-sym)
    (key (find deps ref-sym)) ;; symbol in deps has extra meta
    (first (filter #(= ref-sym (alias %)) (keys deps)))))

(defn references [deps point]
  (let [form (form point)]
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
      (let [[form metas] (if (meta? form) [(with-meta form nil) (dissoc (meta form) `hf/as)] [form nil])]
        (case (categorize form)
          :symbol (collect #{} metas)
          :seq    (apply set/union (map (partial collect #{}) (rest form)))
          ;; :map    (references deps (ffirst form))
          #{})))))

(defn props-children [deps form]
  (->> (meta form)
       (remove (fn [[_k v]] (or (quoted? v)
                                (empty? (references deps (point v))))))
       (map val)))

(defn children [deps form]
  (case (categorize form)
    :vector        form ;; TODO #1 not clear
    :map           (mapcat identity (seq form))
    (:seq :symbol) (props-children deps form)
    nil))

(defn dependencies
  ([form] (dependencies (dependencies {} form) form))
  ([r form]
   (if (nil? form)
     r
     (let [r        (assoc r (point form) #{})
           children (children r form)
           r        (assoc r (point form) (set (mapv point children)))]
       (cond
         (= form children)  (reduce dependencies r children) ;; TODO same #1 not clear
         (vector? children) (reduce dependencies (assoc r (point form) #{(point children)}
                                                        (point children) (set (mapv point children))) children)
         :else              (reduce dependencies r children))))))

(defn reverse-deps [deps]
  (let [points (apply set/union (set (keys deps)) (vals deps))
        r      (zipmap points (repeat (count points) #{}))]
    (reduce-kv (fn [r dep deps]
                 (reduce (fn [r' dep'] (update r' dep' conj dep))
                         r deps))
               r deps)))

(defn ancestors [deps⁻¹ point]
  (let [parents (get deps⁻¹ point)]
    (if (seq parents)
      (apply set/union parents (map (partial ancestors deps⁻¹) parents))
      parents)))

(defn ancestor? "is a ancestor of b" [deps⁻¹ a b] (contains? (ancestors deps⁻¹ b) a))

(defn card-n? [point]
  (when-let [id (identifier (form point))]
    (= ::spec/many (spec/cardinality id))))

(defn crossed-card-n-boundary
  "Returns a set of card-n nodes, `a` have to cross to refer to `b`. Which is forbidden."
  [deps referencing-dep refered-dep]
  (let [deps⁻¹             (reverse-deps deps)
        card-n-ancestors-a (set (filter card-n? (ancestors deps⁻¹ referencing-dep)))
        card-n-ancestors-b (set (filter card-n? (ancestors deps⁻¹ refered-dep)))]
    (set/difference card-n-ancestors-b card-n-ancestors-a)))

(defn check-card-n-crossed-boundaries! [deps point]
  (let [refs    (references deps point)
        crosses (->> refs
                     (map #(resolve-ref deps %))
                     (map (fn [dep] (crossed-card-n-boundary deps point dep)))
                     (apply set/union))]
    (when (seq crosses)
      (throw (ex-info "Can't reference through cardinality many"
                      {:references         refs
                       :referencing-node   (form point)
                       :crossed-boundaries (set (map form crosses))})))))

(defn closest-common-parent [deps point-a point-b]
  (let [deps⁻¹ (reverse-deps deps)]
    (first (sort (comparator (complement (partial ancestor? deps⁻¹)))
                 (set/intersection (ancestors deps⁻¹ point-a) (ancestors deps⁻¹ point-b))))))

(defn tag-ref-points [deps]
  (reduce-kv (fn [r point _children]
               (reduce (fn [r ref]
                         (let [refered-point (resolve-ref deps ref)]
                           (if-let [parent (closest-common-parent deps point refered-point)]
                             (let [actual-point (key (find r parent))]
                               (-> (dissoc r actual-point) ;; assoc on an existing key will reuse the key, ignoring new metas (https://github.com/clojure/clojure/blob/84811650bb33846c7212f391468f9661819b906b/src/jvm/clojure/lang/PersistentHashMap.java#L705)
                                   (assoc (vary-meta actual-point update ::refs (fnil conj #{}) refered-point) (get r parent))))
                             r))) r (references deps point)))
             deps deps))

(defn add-ref-deps [deps]
  (let [deps (tag-ref-points deps)] ;; must happen BEFORE we register reference dependencies
    (reduce-kv (fn [r point children]
                 (if-some [refs (seq (references deps point))]
                   (do
                     (check-card-n-crossed-boundaries! deps point)
                     (assoc r point (into children (filter some? (map (partial resolve-ref deps) refs)))))
                   r))
               deps deps)))

(defn rank [deps point]
  (let [children (get deps point)]
    (if (= #{} children)
      0
      (apply max (map #(inc (rank deps %)) children)))))

(defn toposort [deps] (reduce-kv (fn [r point _children] (assoc r point (rank deps point))) {} deps))

(add-ref-deps (dependencies (parse )))


(hyperfiddle.q8.viz/viz! '{(hyperfiddle.q8.tests/submissions needle) [{:dustingetz/gender [(props :db/ident {::hf/as gender})]}
                                                                      {(props :dustingetz/shirt-size {::hf/options (shirt-sizes gender needle)})
                                                                       [:db/ident]}]})


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

(defn escape [form]
  (condf form
    quoted? form
    symbol? (list 'quote form)
    seq?    (list 'quote form)
    :else   form))

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
  (let [point (point node)]
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

(defn emit [deps]
  (let [bindings (->> (toposort deps)
                      (sort-by val <)
                      (map (fn [[point _]] (let [node (::node (meta point))]
                                             [(identifier node false) `#'~(emit-node deps node)]))))]
    `(let [~@(mapcat identity bindings)] (unquote ~(first (last bindings))))))

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

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
(defn- resolve-syms [env form]
  (walk/prewalk (fn [form]
                  (condf form
                    quoted? (reduced form)
                    symbol? (cond
                              (local? env form)   form
                              (clj/specials form) form
                              :else               (if-let [var (resolve-runtime env form)]
                                                    (symbol var)
                                                    (vary-meta form assoc ::free? true)))
                    :else          form))
                form))

(defn- make-env [&env]
  (if (:js-globals &env)
    &env
    {:ns     (ns-name *ns*)
     :locals (into {} (reduce-kv (fn [r k v] (if (instance? clojure.lang.Compiler$LocalBinding v)
                                               (assoc r (.-sym v) (.-sym v))
                                               (assoc r k v))) {} &env))}))
(defn hfql [&env form]
  (let [form (if (map? form) [form] form)] ;; Wrap top level maps to collect their keys. […] is in charge of collecting.
    (emit (add-ref-deps (dependencies (parse (resolve-syms (make-env &env) form)))))))

