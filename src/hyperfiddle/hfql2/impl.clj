(ns hyperfiddle.hfql2.impl
  "Parses an HFQL expression into a graph (using datascript as the graph data
  structure), then reorder the graph and emit photon code."
  (:refer-clojure :exclude [munge ancestors])
  (:require [datascript.core :as d] ; An HFQL expr is easily represented as a graph
            [hyperfiddle.api :as hf]        ; db, Renderer, e a v model
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :as c] ; var resolution
            ;; [hyperfiddle.photon.debug :as dbg]       ; for tests
            [hyperfiddle.queries-test :refer [orders order shirt-sizes]] ; for tests
            [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
            [hyperfiddle.spec :as spec] ; extract cardinality from fn specs
            [clojure.string :as str]))

;; * TODO List
;; *
;; ** DONE drop :node/parent for :node/children
;; *
;; ** DONE prop nodes should be unique, drop :prop/node
;; *
;; ** DONE provide simple accessors for
;; *
;; *** DONE child render points
;; *
;; *** DONE properties, as collection and direct access by name
;; *
;; ** DONE attach function as a property of :call, not a child
;; *
;; *
;; ** TODO add support for lexical scope
;; ** TODO add support for inputs (.)

(comment
  (do (set! *print-namespace-maps* false)
      (set! *print-meta* true)
      (rcf/enable!))
  )

;;;;;;;;;;;;;;;
;; Accessors ;;
;;;;;;;;;;;;;;;

(defn nodes "Resolve all ids to AST nodes" [db eids] (set (map #(d/entity db %) eids)))

(defn parent [point]
  (->> (:node/_children point)
    (sort-by :db/id) ; Rewriting the graph can create loops, but a parent always come before its child in the expression.
    (first)))

(defn children "For a group, return points in the group, in order. For a traversal, return the continuation."
  [point]
  (->> (:node/children point)
    (filter #(= :render-point (:node/type %)))
    (remove :prop/key) ; props are not syntactically children, they are like metas.
    (sort-by :node/position)))

(defn arguments "For a :call node, return its arguments, in order."
  [node]
  (assert (= :call (:node/form-type node)) "Can only get arguments of a :call node")
  (->> (:node/children node)
    (filter #(#{:argument} (:node/type %)))
    (sort-by :node/position)))

(defn props "Get props of a given point"
  ([point] (sort-by :node/position (filter :prop/key (:node/children point))))
  ([prop-key point] (first (filter #(= prop-key (:prop/key %)) (props point)))))

;;;;;;;;;;;;
;; Passes ;;  Transform an HFQL graph step by step. A pass is a funciton (env db -> db)
;;;;;;;;;;;;

;;; symbol-pass

(defn- munge* [s] (str/replace s #"[\.\/]" "_"))
(defn munge [ident] (let [name (munge* (name ident))]
                      (if-let [ns (namespace ident)]
                        (str (munge* ns) "_" name)
                        name)))

(defn lexical-symbol "Given a node, return a simple symbol to be used as a lexical binding"
  [node]
  (case (:node/form-type node)
    :keyword (symbol (str (munge (:node/form node)) "_" (:db/id node)))
    :call    (let [f (:function/name node)]
               (cond (keyword? f) (symbol (str (munge f) "_call"))
                     (symbol? f)  (let [name (symbol (str (munge* (name (:function/name node))) "_" (:db/id node)))]
                                    (case (:prop/key node)
                                      ::hf/options (symbol (str name "_options"))
                                      name))))
    :group   (symbol (str "group_" (:db/id node)))
    (throw (ex-info "lexical-symbol — Don’t know how to compute symbolic form" {:node (d/touch node)}))))

(defn symbolic-form "Given a node return an symbolic representation of a form.
  e.g. :db/id               -> :db/id
       foo                  -> ns/foo
       (foo (props bar {})) -> (ns/foo bar)
       [:db/id foo]         -> [:db/id foo]"
  [node]
  (case (:node/type node)
    :function     (:function/name node)
    :argument     (:node/form node)
    :render-point (case (:node/form-type node)
                    :keyword (:node/form node)
                    :call    (list 'quote (cons (:function/name node) (map symbolic-form (arguments node))))
                    :group   (mapv symbolic-form (children node)))
    (throw (ex-info "lexical-symbol — Don’t know how to compute symbolic form" {:node (d/touch node)}))))

(defn compute-points-symbol-pass
  "For each point, compute its lexical symbol and symbolic form"  ; NOTE A point is where we can render
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :render-point]] db))  ; find all points
       (map (fn [{:keys [db/id] :as point}]
              (let [tx    {:db/id              id,
                           :node/symbol        (lexical-symbol point)
                           :node/symbolic-form (symbolic-form point)}
                    alias (:prop/value (props ::hf/as point))]
                (case (:node/form-type point)
                  :keyword (assoc tx :node/name (or alias (symbol (:node/form point))))
                  :call    (assoc tx :node/name (or alias (symbol (name (first (:node/form point))))))
                  tx))))
       (d/db-with db)))
;;; end symbol-pass

(defn resolve-functions-pass "For each function node, resolve the function symbol in env and extract the var name (qualified)."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :call]] db))
       (map (fn [node]
              (let [f (first (:node/form node))]
                (cond (symbol? f)  (if-let [var (c/resolve-var env f)]
                                     {:db/id         (:db/id node)
                                      :function/var  (c/get-var var)
                                      :function/name (c/var-name var)}
                                     (throw (ex-info "Cannot resolve function" {:function (:function/symbol f)})))
                      (keyword? f) {:db/id         (:db/id node)
                                    :function/name f}
                      :else        (throw (ex-info "Can only call functions or keywords" {:function f}))))))
       (d/db-with db)))

(defn resolve-cardinalities-pass "For each function node, infer cardinality (::spec/one or ::spec/many) from the function spec."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :function/name]] db))
       (map (fn [node]
              (if-let [card (spec/cardinality (:function/name node))]
                {:db/id                (:db/id node)
                 :function/cardinality card}
                (throw (ex-info "Unknown function cardinality, please define a spec." {:function (:function/name node)})))))
       (d/db-with db)))

(defn move-props-to-group-pass "Move props on a traversal to its continuation (a group).
  e.g. {(props (foo) {…}) [:db/id]} -> {(foo) (props [:db/id] {…})}.

  We move props to the group because the group is rendered, not the traversal itself.

  Having props on both the traversal and the group does not make sense. These are equivalent:
  - {(props :entity/tags {::hf/options (all-tags)}) [:db/id :db/ident]}
  - {:entity/tags (props [:db/id :db/ident] {::hf/options (all-tags)})}
  - (props {:entity/tags [:db/id :db/ident]} {::hf/options (all-tags)})

  "
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :group]] db))  ; find all groups
       (mapcat (fn [node]
                 (let [parent (parent node)] ; get group’s parent
                   (case (:node/type parent)
                     :render-point (->> (props parent)  ; if parent is a point, get parent’s props
                                     (mapcat (fn [prop] ; for each prop
                                               [[:db/retract (:db/id parent) :node/children (:db/id prop)] ; detach itself from parent
                                                {:db/id          (:db/id prop)
                                                 :node/_children (:db/id node)} ; state the prop is now a child of this group node
                                                ])))
                     nil))))
       (d/db-with db)))

;; NOTE this pass seems redundant with :node/children, but as we rewrite the
;; graph, a point might have dependencies that are not it’s own children. e.g.
;; lexical scope reference.
(defn compute-point-dependencies-pass "For each point, compute on which other points this point depend on."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :render-point]] db)) ; for each point
       (mapcat (fn [node]
                 (let [parent (parent node)] ; get the parent
                   (case (:node/type parent)
                     :render-point [{:db/id           (:db/id parent)
                                     :node/dependencies (:db/id node)}] ; state this node depends on its child
                     nil))))
       (d/db-with db)))

(defn compute-function-call-dependencies-pass "State a function always depends on its args"
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :call]] db)) ; for each point
    (mapcat (fn [node]
              (->> (arguments node)
                (map (fn [arg] {:db/id             (:db/id node)
                                :node/dependencies (:db/id arg)})))))
    (d/db-with db)))

(defn compute-options-dependencies-pass
  "::hf/options is a special prop. It’s a point which get it’s parent
  continuation, such that hf/options always produce data in the same shape as
  the point it is attached to."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :prop/key ::hf/options]] db)) ; find all ::hf/options prop nodes
       (mapcat (fn [node]
                 (let [parent-node (parent node)]
                   (case (:node/type parent-node)
                     :render-point (when (= :group (:node/form-type parent-node)) ; if its parent is a group point
                                     [;; the parent does not depend on the options anymore. But it is still
                                      ;; the parent of the options, such that rendering the group will render
                                      ;; the call to hf/options.
                                      [:db/retract (:db/id parent-node) :node/dependencies (:db/id node)]
                                      ;; options now depends on the group and has it as children, such that
                                      ;; rendering the options calls into the continuation.
                                      {:db/id             (:db/id node)
                                       :node/dependencies (:db/id parent-node)
                                       :node/children     (:db/id parent-node)}
                                      ;; the parent of the group also depend on the prop in order to bind it to `options
                                      (when-let [grandparent (parent parent-node)]
                                        {:db/id (:db/id grandparent)
                                         :node/dependencies (:db/id node)})
                                      ;; NOTE: we end up with a cycle: the group has options as a child, the
                                      ;; options has the group as a child. However, dependencies are not
                                      ;; circular. :node/children is meant to represent the HFQL sexpr
                                      ;; parent/child relationship, not the graph. So this is OK.
                                      ])
                     nil))))
       (d/db-with db)))

(defn get-root "Return the only node with no parent. The top-most expression node." [db]
  (d/entity db (d/q '[:find ?e . :where [?e] (not [_ :node/children ?e])] db)))  ; find a node which is not a child

(defn compute-scopes-pass
  "An HFQL scope is a lexical scope. A point can refer to another point as long as
  this point is not declared under a cardinality many dependency. E.g.

  {(submissions \"\") [{:order/gender [:db/ident]} ; `ident` is accessible under the `submissions` scope
                       (props {:order/shirt-size [:db/ident]} {::hf/options (shirt-sizes ident)}]  ; ident can be refered here
   (genders) [:db/ident]}               ; this `ident` is not accessible to shirt-sizes because `genders` is cardinality many. No ambiguity.


  "[env db]
  (let [root   (get-root db)
        scopes (set (nodes db (d/q '[:find [?e ...] :where [?e :function/cardinality ::spec/many]] db)))] ; find all cardinality many functions
    (->> (disj (nodes db (d/q '[:find [?e ...] :where [?e :node/type :render-point]] db)) root) ; get all points but the root
         (mapcat (fn [{:keys [db/id] :as node}]
                   (loop [node (parent node)] ; walk ancestors up to the root
                     (cond
                       (nil? node)             [{:db/id      id              ; if this node does not have a cardinality
                                                 :node/scope (:db/id root)}] ; many parent it is in the root scope.
                       (contains? scopes node) [{:db/id      id              ; otherwise it is in the scope of it’s closest
                                                 :node/scope (:db/id node)}] ; cardinality many parent.
                       :else                   (recur (parent node))))))
         (d/db-with db))))

(defn find-nodes-by-name
  "Look up for nodes named by `symbol` in the `point`’s scope, then in the parent scope, recursively.
  If there is more than one match, they are guaranteed to belong to the same
  scope. `point` cannot be a match (meaning a point cannot refer to itself)."
  [point symbol]
  (when-let [scope (:node/scope point)]
    (if-let [matches (->> (disj (:node/_scope scope) point) ; get all nodes but the current one, in the current scope
                       (filter #(= symbol (:node/name %)))
                       (seq))]
      (set matches)
      (recur scope symbol)))) ; look up in parent scope

(defn resolve-lexical-references-pass
  "For all function arguments, resolve the node they might refer to in lexical
  scope. Record the dependency between the argument and the graph node, and
  rewrite the argument symbol to point to the graph node."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (mapcat (fn [{:keys [node/form db/id] :as node}]
           (when (symbol? form)
             (when-some [matches (seq (find-nodes-by-name (parent node) form))]
               (case (count matches)
                 0 nil
                 1 [{:db/id id, :node/dependencies (:db/id (first matches)), :node/reference (:db/id (first matches))}]
                 (throw (ex-info (str "Ambiguous reference. `" form "` refers to more than one expression.") {:matches (map :node/form matches)})))))))
    (d/db-with db)))

(defmacro floop "For when an infinite loop is a flop. Rewrite loop into a recursive call so you get a Stackoverflow to diagnose." [bindings & body]
  (let [pairs  (partition 2 bindings)
        args   (map first pairs)
        params (map second pairs)
        rec-point (gensym "recur_")]
    `((fn ~rec-point [~@args] ~@(clojure.walk/postwalk (fn [x] (if (= 'recur x) rec-point x)) body)) ~@params)))

(defn ancestors "Return a set of transitive parents of a node" [node]
  (loop [node node
         r    #{}]
    (cond (nil? node)           r
          (some? (parent node)) (recur (parent node) (conj r (parent node)))
          :else                 r)))

(defn find-join-target-point "Find the closest point we need to join to resolve a reference." [node]
  (let [ancestors (ancestors node)]
    (loop [ref (:node/reference node)
           prev ref]
      (when (some? ref)
        (if (contains? ancestors ref) prev
            (recur (parent ref) ref))))))

(defn reference-path "Return a path (can be passed to get-in), between two parent/child nodes." [parent child]
  (assert (contains? (ancestors child) parent))
  (loop [node child
         r ()]
    (let [r (if (= :group (:node/form-type node)) r (cons (:node/symbolic-form node) r))]
      (if (= node parent) (vec (rest r))
          (recur (hyperfiddle.hfql2.impl/parent node) r)))))

(defn compute-reference-join-path-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/reference]] db))
    (mapcat (fn [node]
              (let [join-node (find-join-target-point node)]
                [{:db/id               (:db/id node)
                  :node/reference      (:db/id join-node)
                  :node/reference-path (reference-path join-node (:node/reference node))
                  :node/dependencies   (:db/id join-node)}
                 [:db/retract (:db/id node) :node/dependencies (:db/id (:node/reference node))]
                 {:db/id                         (:db/id join-node)
                  :node/remember-entity-at-point true}])))
    (d/db-with db)))

(defn decide-how-node-should-render-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :render-point]] db))
    (map (fn [node] (if (= ::spec/many (:function/cardinality node))
                      {:db/id          (:db/id node)
                       :node/render-as ::hf/table}
                      {:db/id          (:db/id node)
                       :node/render-as (case (:node/form-type node)
                                         :group ::hf/form
                                         ::hf/field)})))
    (d/db-with db)))

;; Future work to support (:db/id %) form and provide defaults or (foo %) to pass current entity to function.
#_(defn rename-%-to-entity-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument] [?e :node/form %]] db))
    (map (fn [node] {:db/id (:db/id node) :node/form `hf/entity}))
    (d/db-with db)))

(defn compute-columns-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :group]] db))
    (mapcat (fn [{:keys [db/id] :as node}]
              (let [columns (:node/symbolic-form node)]
                [{:db/id id, :node/columns columns}
                 (when-let [parent (parent node)]
                   (when (= ::spec/many (:function/cardinality parent))
                     {:db/id (:db/id parent), :node/columns columns}))])))
    (d/db-with db)))

;; Passes to apply in order.
(def passes [#'resolve-functions-pass
             #'resolve-cardinalities-pass
             #'move-props-to-group-pass
             #'compute-point-dependencies-pass
             #'compute-function-call-dependencies-pass
             #'compute-scopes-pass
             #'compute-options-dependencies-pass
             #'compute-points-symbol-pass
             #'resolve-lexical-references-pass
             #'compute-reference-join-path-pass
             #'decide-how-node-should-render-pass
             #'compute-columns-pass
             ;; #'rename-%-to-entity-pass ; future work
             ])

(defn apply-passes [passes env db] (reduce (fn [db pass] (pass env db)) db passes))

;;;;;;;;;;;;;;;;;;;;;
;; Parse & Analyze ;;
;;;;;;;;;;;;;;;;;;;;;

(defn props? "State if an sexp is `(props …)" [form] (and (seq? form) (= 'props (first form))))

(declare parse)

(defn parse-props "Parse a props map declared with (props <form> {k1 v1, …}). `parent-id` must be the node id the props are attached to."
  [gen-id parent-id props]
  (->> (map-indexed vector props) ; we want to emit code in the same order as the hfql expr is read.
    (reduce (fn [r [index [k v]]] ; for each k v pair of the map, in seq order.
                 (case k
                   ::hf/options (into r (-> (parse gen-id parent-id v)
                                          (update 0 assoc :prop/key k, :node/position index)))
                   (conj r {:db/id          (gen-id) ; create a node of type :prop with k and v
                            :node/type      :prop
                            :node/_children parent-id
                            :prop/key       k
                            :prop/value     v
                            :node/position  index})))
      [])))

(defn parse-arg "Parse a function argument into a tx. `parent-id` is supposed to be the function node’s id."
  [gen-id parent-id form]
  (cond
    ;; Props on args are allowed, for instance an argument could render as a select-option.
    (props? form) (let [[_ form props] form
                        arg            (parse-arg gen-id parent-id form)] ; parse the arg
                    (if (empty? props) [arg]
                        (into [arg] (parse-props gen-id (-> arg first :db/id) props))  ; attach props a children of the arg
                        ))
    :else         [{:db/id          (gen-id)
                    :node/_children parent-id
                    :node/type      :argument
                    :node/form      form}]))

(defn parse
  "Parse an HFQL form into a datascript tx, representing a graph."
  ([form] (parse (partial swap! (atom 0) dec) nil form))
  ([gen-id parent-id form]
   (cond
     ;; Props are extra info attached to a form, like metadata. Some props are
     ;; interpreted, like ::hf/options.
     (props? form)   (let [[_ form props] form
                           parsed         (parse gen-id parent-id form)] ; parse the form
                       (if (empty? props) parsed
                           (into parsed (parse-props gen-id (-> parsed first :db/id) props)  ; parse and attach props as children of the form
                             )))
     ;; A vector represents a group. It usually renders as a form (card one) or a table (card many).
     (vector? form)  (let [id (gen-id)]
                       (into [{:db/id          id
                               :node/_children parent-id ; means "is a child of parent-id"
                               :node/form      form
                               :node/type      :render-point
                               :node/form-type :group}]
                         (->> (map (partial parse gen-id id) form)  ; parse each member of the group
                           (map-indexed (fn [idx tx] (update tx 0 assoc :node/position idx)))  ; record their position (we want colums and fields in the same order as the HFQL expression.
                           (mapcat identity) ; concat into a single tx
                           )))
     ;; A map represents a continuation on a render point: {point continuation}.
     ;; The left hand side is always a render point. TODO what about view-defaults in fn args?
     (map? form)     (if (= 1 (count form)) ; It can only contain one key-value pair.
                       (let [[k v]        (first form)
                             render-point (parse gen-id parent-id k)
                             id           (-> render-point first :db/id)
                             continuation (parse gen-id id v)] ; parse and attach continuation as a children of the render point
                         (into render-point continuation))
                       (throw (ex-info "In HFQL, a map can only contain one key-value pair." form)))
     ;; A keyword is a render point if it’s not a function argument. It has no continuation.
     (keyword? form) [{:db/id          (gen-id)
                       :node/_children parent-id
                       :node/type      :render-point
                       :node/form      form
                       :node/form-type :keyword}]
     ;; A symbol is a render point (transformed to a call) if it’s part of a group.
     (symbol? form) [{:db/id          (gen-id)
                      :node/_children parent-id
                      :node/type      :render-point
                      :node/form      form
                      :node/form-type :symbol}]
     ;; A seq is a function call if it’s not a props form. It can be a render point or a prop value.
     (seq? form)     (let [id (gen-id)]
                       (into [{:db/id          id
                               :node/_children parent-id
                               :node/type      :render-point
                               :node/form      form ; a later pass will extract and resolve the function
                               :node/form-type :call}]
                         (->> (map #(parse-arg gen-id id %) (rest form)) ; parse all args
                           (map-indexed (fn [idx tx] (update tx 0 assoc :node/position idx))) ; preserve order
                           (mapcat identity)) ; concat into single tx
                         )))))

(defn to-graph [tx]
  (d/db-with @(d/create-conn {:db/id             {:db/unique :db.unique/identity}
                              :node/children     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} ; syntactical children in expr
                              :node/dependencies {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} ; computed graph dependencies
                              :node/scope        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one} ; HFQL implicit lexical scope, bounded by card many points
                              :node/reference    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one} ; A function arg might refer to another point
                              })
             (map (fn [entity]
                    (if (nil? (:node/_children entity))
                      (dissoc entity :node/_children)
                      entity))
                  tx)))

(defn analyze "Produce a graph from an HFQL edn form." [form]
  (let [form   (if (map? form) [form] form)
        gen-id (partial swap! (atom 0) dec)]
    (to-graph (parse gen-id nil form))))

(defn graph "Return graph nodes as edn"
  ([form] (graph {} form))
  ([env form]
   (let [db (apply-passes passes (c/normalize-env env) (analyze form))]
     (->> (d/q '[:find [?e ...] :where [?e]] db)
          (nodes db)
          (sort-by :db/id)
          (mapv #(into {:db/id (:db/id %)} (d/touch %)))))))

;;;;;;;;;;
;; Emit ;;
;;;;;;;;;;

(defn dependencies "Return a set of transitive dependencies of a render point" [point]
  (let [deps (:node/dependencies point #{})]
    (into deps (mapcat dependencies deps))))

(defn sort-by-dependency "Sort a collection of render points by their dependency. If a depends on b, then
  b comes before a. Used to order lexical bindings."
  [points] (sort (fn [x y]
                   (cond ((dependencies y) x) -1 ; "Is x part of y’s dependencies?" aka "Does y depends on x?"
                         ((dependencies x) y) 1  ; "clever" use of a set as a function
                         :else                (< (:db/id x) (:db/id y))) ; if no dependency, sort by position in the HFQL expr
                   ) points))

(declare emit)

(defn scope-bindings "Return a toposorted list of lexical bindings in the scope of the given render point"
  [point]
  (mapcat (fn [point] [(:node/symbol point) `(p/fn [] ~(emit point))]) (sort-by-dependency (:node/_scope point))))

(defn add-scope-bindings
  "If the given render point has dependencies in this scope, emit them as lexical bindings."
  [point emitted-form]
  (if-some [bindings (seq (scope-bindings point))]
    `(let [~'entities (atom {}) ~@bindings] ~emitted-form)
    emitted-form))

(defn maybe-update "Like `update` but applies `f` only if `k` is present in the map."
  [m k f]
  (if (contains? m k) (update m k f) m))

(defn emit-props "Emit a map of {prop-key prop-value} for a given render point." [point]
  (let [props-map (->> (props point)
                    (map (fn [{:keys [prop/key prop/value]}]
                           [key (case key
                                  ::hf/options `(p/fn [] (binding [hf/bypass-renderer true] (hf/options.)))
                                  ::hf/as (list 'quote value)
                                  value)]))
                    (into {}))]
    (cond-> props-map
      (:node/render-as point)               (assoc ::hf/render-as (:node/render-as point))
      (:node/columns point)                 (assoc ::hf/columns (:node/columns point))
      (not= :group (:node/form-type point)) (assoc ::hf/attribute (:node/symbolic-form point)))))

(defn emit-call [point]
  (cons (:function/name point)
    (->> (arguments point)
      (map (fn [node]
             (if-let [ref (:node/reference node)]
               `(binding [hf/entity          (get @~'entities '~(:node/symbol ref))  ; FIXME entities is shadowed by card many, lookup should walk up a stack (can store parent atom is special key in atom)
                          hf/bypass-renderer true]
                  (get-in (new ~(:node/symbol ref)) ~(:node/reference-path node))) ; FIXME only join the required path, lazily.
               (:node/form node)))))))

(defn remember! [atom k v] (swap! atom assoc k v) v)

(defn emit "Emit clojure code for a render point and its dependencies." [point]
  (assert (= :render-point (:node/type point)) "emit - not a renderable point")
  (case (:node/form-type point)
    :keyword (let [attribute (:node/form point)
                   value     `(hyperfiddle.hfql2/nav! hf/*$* hf/entity ~attribute)
                   form      (if-some [continuation (first (children point))] ; if there is a continuation
                               (add-scope-bindings point
                                 `(binding [hf/entity ~value
                                            ;; we pass options as a dynamic binding because a
                                            ;; group depends on options to emit props and
                                            ;; options depends on the group to emit the
                                            ;; continuation.
                                            hf/options   ~(:node/symbol (props ::hf/options continuation))
                                            ]
                                    (new ~(:node/symbol continuation))))
                               (let [render-form `(hf/Render. (p/fn [] ~value) ~(emit-props point))]
                                 (if-let [options (:node/symbol (props ::hf/options point))]
                                   (add-scope-bindings point `(binding [hf/options ~options] ~render-form))
                                   (add-scope-bindings point render-form))))]
               (if (:node/remember-entity-at-point point)
                 `(do (swap! ~'entities assoc '~(:node/symbol point) hf/entity)
                      ~form)
                 form))
    :call    (let [cardinality (:function/cardinality point)]
               (if-some [continuation (first (children point))]
                 (let [nav `(new ~(:node/symbol continuation))]
                   (case cardinality ; TODO What if cardinality is unknown at compile time?
                     ::spec/one  `(binding [hf/entity ~(:node/form point)] ;; TODO what about props?
                                    ~(add-scope-bindings point nav))
                     ::spec/many `(hf/Render.
                                    (p/fn []
                                      (p/for [e# ~(emit-call point)]
                                        (p/fn []
                                          (binding [hf/entity e#]
                                            ~(add-scope-bindings point nav)))))
                                    ~(emit-props point)))) ; TODO rendering call should account for args deps
                 ;; if there is no continuation, just emit the call.
                 (emit-call point)
                 ))
    :group   (add-scope-bindings point
               `(hf/Render. (p/fn [] ~(reduce (fn [r point] (assoc r (:node/symbolic-form point) (:node/symbol point)))
                                        {} (children point)))
                  ~(emit-props point)))))

;;;;;;;;;;;
;; Macro ;;
;;;;;;;;;;;

(defn hfql* [env form]
  (->> (analyze form)
       (apply-passes passes (c/normalize-env env))
       (get-root)
       (emit)))

(defmacro hfql [form] (hfql* &env form))

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(tests
 (with (p/run (tap (binding [hf/entity 9]
                     (hfql :db/id) ))))
 % := 9)

(tests
 (with (p/run (tap (binding [hf/entity 9] (hfql [:db/id]) ))))
 % := {:db/id 9})

;; (get-root (analyze [:db/id]))

(p/def string-renderer (p/fn [>v _props] (str (new >v))))

(tests
 "hf/render"
 (with (p/run (tap (binding [hf/entity 9] (hfql (props :db/id {::hf/render string-renderer})) ))))
 % := "9")

(tests
 "hf/render inline"
 (with (p/run (tap (binding [hf/entity 9] (hfql (props :db/id {::hf/render (p/fn [>v _props] (str (new >v)))}))))))
 % := "9")

(tests
 (with (p/run (tap (binding [hf/entity 9] (hfql [(props :db/id {::hf/render string-renderer})])))))
 % := {:db/id "9"})

(tests
 (with (p/run (binding [hf/entity 9] (tap (hfql {:order/gender [:db/ident]}) ))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (with (p/run (binding [hf/entity 9] (tap (hfql [{:order/gender [:db/ident]}])))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (with (p/run (tap (hfql {(order "") [:db/id]}))))
 % := '{(hyperfiddle.queries-test/order "") {:db/id 9}})

(tests
 "Two levels of nesting"
 (with (p/run (tap (hfql {(order "") [{:order/shirt-size [:db/ident]}]}))))
 % := {'(hyperfiddle.queries-test/order "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
 "multiplicity many"
 (with (p/run (tap (hfql {(orders "") [:db/id]}))))
 % := {'(hyperfiddle.queries-test/orders "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
 (with (p/run (tap (hfql {(orders "") [(props :db/id {::hf/render string-renderer})]}))))
 % := {'(hyperfiddle.queries-test/orders "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(p/defn throwing-renderer [V props] (throw (ex-info "I fail" {})))
(p/defn ignoring-renderer [V props] "ignored")

(tests
  (p/run (tap (binding [hf/entity 9]
                (hfql [{(props :order/gender {::hf/render ignoring-renderer}) [(props :db/ident {::hf/render throwing-renderer})]}]))))
  % := {:order/gender "ignored"} ; note it didn’t throw
  )


(p/defn Select-option-renderer [>v props]
  (into [:select {:value (new hf/Join-all (new >v))}]
    (p/for [e (new (::hf/options props))]
      [:option e])))

(tests
  (with (p/run (tap (binding [hf/entity 9]
                      (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                       ::hf/options (shirt-sizes :order/female "")})]) ))))
  % := {:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})

(tests
  "hf/options inherit parent pullexpr"
  (with (p/run
          (tap (binding [hf/entity 9]
                 (hfql [{(props :order/shirt-size {::hf/render  Select-option-renderer
                                                   ::hf/options (shirt-sizes :order/female "")})
                         [:db/ident]}]) 
                 ))))
  % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                           [:option #:db{:ident :order/womens-small}]
                           [:option #:db{:ident :order/womens-medium}]
                           [:option #:db{:ident :order/womens-large}]]})

;; (graph '[{(props :order/shirt-size {::hf/render  Select-option-renderer
;;                                     ::hf/options (shirt-sizes :order/female "")})
;;           [:db/ident]}])

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/entity 9]
                      (hfql [{:order/gender [:db/ident]}
                             (props :order/shirt-size {::hf/render  Select-option-renderer
                                                       ::hf/options (shirt-sizes db/ident "")})]) ))))
  % := {:order/gender {:db/ident :order/female}
        :order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})


(tests
  "Argument reference under card n"
  (with (p/run (tap (binding [hf/entity 9]
                      (hfql {(orders "") [{:order/gender [:db/ident]}
                                          (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                    ::hf/options (shirt-sizes db/ident "")})]}) ))))
  % := '{(hyperfiddle.queries-test/orders "")
         [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
           :order/gender {:db/ident :order/female}}
          {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}
          {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}]})

(tests
  "Argument reference under card n"
  (with (p/run (tap (binding [hf/entity 9]
                      (hfql {(orders "") [{:order/gender [:db/ident]}
                                          (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                    ::hf/options (shirt-sizes db/ident "")})]}) ))))
  % := '{(hyperfiddle.queries-test/orders "")
         [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
           :order/gender {:db/ident :order/female}}
          {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}
          {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}]})

(tests
  "lexical env"
  (let [needle1  "alice"
        needle2  "small"]
    (with (p/run (tap (binding [hf/entity 9]
                        (hfql {(orders needle1) [:order/email
                                                 {:order/gender [(props :db/ident {::hf/as gender})]}
                                                 {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                            ::hf/options (shirt-sizes gender needle2)})
                                                  [:db/ident]}]}) )))))
  % := '{(hyperfiddle.queries-test/orders needle1)
         [{:order/shirt-size
           [:select
            {:value {:db/ident :order/womens-large}}
            [:option {:db/ident :order/womens-small}]],
           :order/gender {:db/ident :order/female},
           :order/email "alice@example.com"}]})
