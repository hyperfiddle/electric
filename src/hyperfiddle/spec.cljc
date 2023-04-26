(ns hyperfiddle.spec
  (:require [clojure.core.protocols :as ccp]
            [clojure.datafy :refer [datafy nav]]
            #?(:clj [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])
            [contrib.walk :as walk]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

(defn spec-type [spec]
  (let [desc (s/describe spec)]
    (cond
      (symbol? desc) ::predicate
      (seq? desc)    (keyword (namespace ::_) (name (first desc)))
      :else          (assert false (str "Unknown spec description " desc)))))


(defn renamespace [mapping ident]
  (assert (ident? ident))
  (if (qualified-ident? ident)
    (let [ns (namespace ident)
          ns (mapping ns ns)]
      (if (keyword? ident)
        (keyword ns (name ident))
        (symbol ns  (name ident))))
    ident))

(defn form [spec]
  (when spec
    (walk/prewalk (fn [x] (if (ident? x) (renamespace {"cljs.spec.alpha" "clojure.spec.alpha"
                                                       "cljs.core"       "clojure.core"} x) x))
      (s/form spec))))

(declare datafy* spec)
(defn extend-spec [spec] (when spec (vary-meta spec assoc `ccp/datafy datafy*)))

;; FIXME get-spec fails to find specs at cljs macroexpansion time. Specs
;; compiled by cljs compiler are stored in `cljs.spec.alpha/registry-ref`,
;; whereas `s/get-spec` refers to the JVM equivalent ns at macroexpansion.
;; Current workaround is a `:require-macros` on the spec-defining ns (cljs or
;; cljc), so the spec is registered on both side. We might want to detect the
;; compilation target and resolve into the corresponding registry.
(defn get-spec "Like s/get-spec, but resolve aliases" [ident]
  (when-let [s (s/get-spec ident)]
    (extend-spec (if (ident? s) (vary-meta (get-spec s) assoc ::alias ident) s))))

(defn parse [form]
  (if (seq? form)
    (case (first form)
      (clojure.spec.alpha/cat
        clojure.spec.alpha/alt
        clojure.spec.alpha/or) (let [kvs  (partition 2 (rest form))
                                     keys (mapv first kvs)]
                                 {::type   (keyword (namespace ::_) (name (first form)))
                                  ::keys   keys
                                  ::values (mapv second kvs)})
      clojure.spec.alpha/+     {::type ::+
                                ::pred (second form)}
      clojure.core/fn          {::type        ::predicate
                                ::form        form
                                ::description form})
    (cond
      (ident? form) (or (datafy (get-spec form)) {::type        ::predicate
                                                  ::form        form
                                                  ::description form}))))

(defn datafy* [spec]
  (let [{:keys [::alias ::s/name]} (meta spec)
        type                       (spec-type spec)]
    (-> (merge {::name        (or alias name)
                ::type        type
                ::form        (form spec)
                ::description (s/describe spec)}
          (when (some? alias)
            {::alias name})
          (case type
            (::and ::predicate) {}      ; nothing to add
            ::keys              (let [[_ _ req _ opt _ req-un _ opt-un] (form spec)]
                                  {::req    req
                                   ::opt    opt
                                   ::req-un req-un
                                   ::opt-un opt-un})
            ::fspec             {::args (hyperfiddle.spec/spec (:args spec))
                                 ::ret  (hyperfiddle.spec/spec (:ret spec))}
            (::cat ::alt ::or)  (parse (form spec))
            ::coll-of           (let [[_ pred & opts] (form spec)]
                                  {::pred pred
                                   ::opts (apply hash-map opts)})
            ;; FIXME add ::nilable
            ))
      (with-meta (merge (dissoc (meta spec) `ccp/datafy :clojure.datafy/obj :clojure.datafy/class)
                   {`ccp/nav
                    (fn [x k v]
                      (case k ; reverse data back to object, to be datafied again by caller
                        ::alias  (hyperfiddle.spec/spec name)
                        ::args   (hyperfiddle.spec/spec (:args spec))
                        ::ret    (hyperfiddle.spec/spec (:ret spec))
                        ::values (with-meta (::values x)
                                   {`ccp/datafy (fn [_]
                                                  (mapv (fn [key value] (assoc (parse value) ::key key)) (::keys x) (::values x)))})
                        ;; TODO implement ::keys
                        (::req ::opt ::req-un ::opt-un
                               ::predicates ; s/and, s/&
                               ) (throw (ex-info "Not implemented yet. Please log a ticket" {:nav k, ::type type}))
                        v))})))))

(defn spec [spec-or-ident]
  (when spec-or-ident
    (assert (or (s/spec? spec-or-ident) (qualified-ident? spec-or-ident))
      (str "Expected a clojure spec object or a qualified identifier for a defined spec. Given " spec-or-ident))
    (cond (s/spec? spec-or-ident) (with-meta (form spec-or-ident) {`ccp/datafy (fn [_] (datafy (extend-spec spec-or-ident)))})
          (qualified-ident? spec-or-ident) (when-let [s (get-spec spec-or-ident)]
                                             (with-meta (form s) {`ccp/datafy (fn [_] (datafy s))})))))

;; #?(:cljs (spec `wip.orders-datascript/orders))

(defn reflect [spec-or-ident]
  (assert (or (s/spec? spec-or-ident) (qualified-ident? spec-or-ident))
    (str "Expected a clojure spec object or a qualified identifier for a defined spec. Given " spec-or-ident))
  (cond (s/spec? spec-or-ident) (datafy (extend-spec spec-or-ident))
        (qualified-ident? spec-or-ident) (datafy (get-spec spec-or-ident))))

(tests
  (s/def ::_pred string?)

  (spec ::_pred) := 'clojure.core/string?

  (datafy (spec ::_pred))
  := {::name        ::_pred,
      ::type        ::predicate,
      ::form        'clojure.core/string?
      ::description 'string?}
  )

(tests
  (s/def ::_alias ::_pred)

  (spec ::_alias) := 'clojure.core/string?

  (datafy (spec ::_alias))
  := {::name        ::_alias,
      ::type        ::predicate,
      ::alias       ::_pred,
      ::form        'clojure.core/string?
      ::description 'string?}
  )

(tests
  (let [alias (as-> (spec ::_alias) %
                (datafy %)
                (nav % ::alias (::alias %)))]

    alias := 'clojure.core/string?

    (datafy alias)
    := '{::name        ::_pred,
         ::type        ::predicate,
         ::form        clojure.core/string?
         ::description string?})
  )


(tests

  (s/def ::_keys (s/keys :req [::_pred] :req-un [::req-un] :opt [::_alias] :opt-un [::opt-un]))

  (spec ::_keys ) := '(clojure.spec.alpha/keys :req [::_pred] :opt [::_alias] :req-un [::req-un] :opt-un [::opt-un])

  (datafy (spec ::_keys))
  := {::name        ::_keys,
      ::type        ::keys,
      ::form        '(clojure.spec.alpha/keys
                       :req [::_pred] :opt [::_alias] :req-un [::req-un] :opt-un [::opt-un]),
      ::description '(keys :req [::_pred] :opt [::_alias] :req-un [::req-un] :opt-un [::opt-un])
      ::req         [::_pred],
      ::opt         [::_alias],
      ::req-un      [::req-un],
      ::opt-un      [::opt-un]}
  )


(tests

  (s/fdef many-foo :args (s/cat :arg1 string? :arg2 ::_pred)
          :ret (s/coll-of ::_pred :kind vector?))

  (spec `many-foo) := '(clojure.spec.alpha/fspec :args (clojure.spec.alpha/cat :arg1 clojure.core/string? :arg2 ::_pred)
                         :ret (clojure.spec.alpha/coll-of ::_pred :kind clojure.core/vector?)
                         :fn nil)

  (datafy (spec `many-foo))
  :=
  '{::name        hyperfiddle.spec/many-foo,
    ::type        ::fspec,
    ::form        (clojure.spec.alpha/fspec :args (clojure.spec.alpha/cat :arg1 clojure.core/string? :arg2 ::_pred)
             :ret (clojure.spec.alpha/coll-of ::_pred :kind clojure.core/vector?)
             :fn nil),
    ::description (fspec :args (cat :arg1 string? :arg2 ::_pred) :ret (coll-of ::_pred :kind vector?) :fn nil),
    ::args        (clojure.spec.alpha/cat :arg1 clojure.core/string? :arg2 ::_pred),
    ::ret         (clojure.spec.alpha/coll-of ::_pred :kind clojure.core/vector?)}
  )

(defn cardinality-many?
  "Guess the cardinality of a speced function or keyword."
  [ident]
  (let [s (datafy (spec ident))]
    (when-let [type (case (::type s)
                      ::fspec (::type (datafy (nav s ::ret (::ret s))))
                      (::type s))]
      (= ::coll-of type))))


(tests

  (s/fdef one-foo :ret ::_pred)

  (cardinality-many? `many-foo) := true
  (cardinality-many? `one-foo)  := false)

(defn args [ident] ; TODO support multiple arities
  (let [s (datafy (get-spec ident))]
    (when (= ::fspec (::type s))
      (nav s ::args (::args s)))))

(tests
  (args `many-foo) := '(clojure.spec.alpha/cat :arg1 clojure.core/string? :arg2 :hyperfiddle.spec/_pred)

  (as-> (datafy (args `many-foo)) %
    (nav % ::values (::values %))
    (datafy %))
  :=
  '[{::type        ::predicate,
     ::form        clojure.core/string?,
     ::description clojure.core/string?,
     ::key         :arg1}
    {::name        ::_pred,
     ::type        ::predicate,
     ::form        clojure.core/string?,
     ::description string?,
     ::key         :arg2}])

(defn arg [ident arg-key]
  (as-> (args ident) %
    (datafy %)
    (nav % ::values (::values %))
    (datafy %)
    (filter #(= arg-key (::key %)) %)
    (first %)))

(tests
  (arg `many-foo :arg2)
  :=
  '{::name        ::_pred,
    ::type        ::predicate,
    ::form        clojure.core/string?,
    ::description string?,
    ::key         :arg2})

;; ----

(def types
  ;;  pred                  type                           valueType
  [['clojure.core/boolean? :hyperfiddle.spec.type/boolean :db.type/boolean]
   ['clojure.core/double?  :hyperfiddle.spec.type/double  :db.type/double]
   ['clojure.core/float?   :hyperfiddle.spec.type/float   :db.type/float]
   ['clojure.core/decimal? :hyperfiddle.spec.type/bigdec  :db.type/bigdec]
   ['clojure.core/inst?    :hyperfiddle.spec.type/instant :db.type/instant]
   ['clojure.core/keyword? :hyperfiddle.spec.type/keyword :db.type/keyword]
   ['clojure.core/string?  :hyperfiddle.spec.type/string  :db.type/string]
   ['clojure.core/uri?     :hyperfiddle.spec.type/uri     :db.type/uri]
   ['clojure.core/uuid?    :hyperfiddle.spec.type/uuid    :db.type/uuid]
   ['clojure.core/map?     :hyperfiddle.spec.type/ref     :db.type/ref]
   ['clojure.core/ref?     :hyperfiddle.spec.type/ref     :db.type/ref]
   ['clojure.core/symbol?  :hyperfiddle.spec.type/symbol  :db.type/symbol]
   ['clojure.core/integer? :hyperfiddle.spec.type/long    :db.type/long]
   ['clojure.core/number?  :hyperfiddle.spec.type/long    :db.type/long]
   ['clojure.core/nat-int? :hyperfiddle.spec.type/long    :db.type/long]
   ['clojure.core/int?     :hyperfiddle.spec.type/long    :db.type/long]
   ['clojure.core/pos-int? :hyperfiddle.spec.type/long    :db.type/long]])

(def pred->type (zipmap (map first types) (map second types)))
(def valueType->type (zipmap (map #(get % 2) types) (map second types)))
(def valueType->pred (zipmap (map #(get % 2) types) (map first types)))

(defn type-of
  ([spec] (let [spec (datafy (hyperfiddle.spec/spec spec))]
            (case (::type spec)
              ::fspec (pred->type (::ret spec))
              (pred->type (::form spec)))))
  ([spec argument] (pred->type (::form (arg spec argument)))))

(defn valueType-of [schema attr]
  (valueType->type (:db.valueType (get schema attr))))

(comment
  (datafy (hyperfiddle.spec/spec :user.datafy-fs/kind)) )

(defn explain-fspec-data [fspec & args]
  (let [cat (:args (get-spec fspec))]
    (assert cat "explain-fspec-data is implemented for function spec arguments only (s/fdef :args).")
    (dissoc (s/explain-data cat args) :clojure.spec.alpha/spec)))

(defn reformat-explain-data [ed]
  (when (and (map? ed) (contains? ed :clojure.spec.alpha/problems))
    (let [problems (->> (:clojure.spec.alpha/problems ed)
                     (sort-by #(- (count (:in %))))
                     (sort-by #(- (count (:path %)))))]
      (->> problems
        (map (fn [{:keys [path pred val reason via in] :as prob}]
               {:val      val
                :in       in
                :path     path
                :spec     (last via)
                :reason   (or reason (s/abbrev pred))
                :problems (dissoc prob :path :pred :val :reason :via :in)}))
        (group-by :path)
        (not-empty)))))

(comment

  (reformat-explain-data (explain-data `swinged.rosie.account/change-email [1 ""]))

  (reformat-explain-data (s/explain-data int? 1.1))


  (s/explain-str ))
