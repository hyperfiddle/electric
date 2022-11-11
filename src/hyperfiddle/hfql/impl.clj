(ns hyperfiddle.hfql.impl
  "Parses an HFQL expression into a graph (using datascript as the graph data
  structure), then reorder the graph and emit photon code."
  (:refer-clojure :exclude [munge ancestors])
  (:require [datascript.core :as d] ; An HFQL expr is easily represented as a graph
            [hyperfiddle.api :as-alias hf] ; db, Renderer, e a v model
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :as c] ; var resolution
            [hyperfiddle.spec :as spec] ; extract cardinality from fn specs
            [clojure.string :as str]
            [clojure.datafy :refer [datafy]]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.runtime :as r]))

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
    (:keyword :symbol) (symbol (str (munge (:node/form node)) "_" (:db/id node)))
    :call    (let [f (:function/name node)]
               (cond (keyword? f) (symbol (str (munge f) "_call"))
                     (symbol? f)  (let [name (symbol (str (munge* (name (:function/name node))) "_" (:db/id node)))]
                                    (case (:prop/key node)
                                      ::hf/options (symbol (str name "_options"))
                                      ::hf/link    (symbol (str name "_link"))
                                      name))))
    :group   (symbol (str "group_" (:db/id node)))
    (case (:node/type node)
      :argument (let [function (parent node)]
                  (symbol (str (:node/symbol function) "_" (name (:spec/name node)) "_" (:db/id node))))
      (throw (ex-info "lexical-symbol — Don’t know how to compute symbolic form" {:node (d/touch node)})))))

(defn symbolic-form "Given a node return an symbolic representation of a form.
  e.g. :db/id               -> :db/id
       foo                  -> ns/foo
       (foo (props bar {})) -> (ns/foo bar)
       [:db/id foo]         -> [:db/id foo]"
  [node]
  (case (:node/type node)
    :function     (:function/name node)
    :argument     (:node/form node) ; spec arg name
    :render-point (case (:node/form-type node)
                    :keyword (:node/form node)
                    :symbol  (list 'quote (:function/name node))
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

(defn compute-arg-symbol-pass
  "For each fn argument, compute its lexical symbol and symbolic form"
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (map (fn [{:keys [db/id] :as point}]
           {:db/id              id,
            :node/symbol        (lexical-symbol point)
            ;; args symbolic form are their raw form: (orders "") is unique and contextual (orders needle) is not.
            ;; :node/symbolic-form (symbolic-form point)
            }))
    (d/db-with db)))
;;; end symbol-pass

(defn resolve-functions-pass "For each function node, resolve the function symbol in env and extract the var name (qualified)."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where (or [?e :node/form-type :call] [?e :node/form-type :symbol])] db))
       (map (fn [node]
              (let [f (case (:node/form-type node)
                        :symbol (:node/form node)
                        :call   (first (:node/form node)))]
                (cond (symbol? f)  (if-let [var (c/resolve-var env f)]
                                     {:db/id         (:db/id node)
                                      :function/var  (c/get-var var)
                                      :function/name (c/var-name var)}
                                     (throw (ex-info "Cannot resolve function" {:function (:function/symbol f)})))
                      (keyword? f) {:db/id         (:db/id node)
                                    :function/name f}
                      :else        (throw (ex-info "Can only call functions or keywords" {:function f}))))))
       (d/db-with db)))

(defn resolve-cardinalities-pass "For each function node, infer cardinality (::hf/one or ::hf/many) from the function spec."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where (or [?e :function/name] [?e :node/form-type :keyword])] db))
       (map (fn [node]
              (let [spec (or (:function/name node) (:node/form node))]
                (if-some [many? (spec/cardinality-many? spec)]
                  {:db/id            (:db/id node)
                   :node/cardinality (if many? ::hf/many ::hf/one)}
                  (case spec
                    (:db/id :db/ident) {:db/id (:db/id node), :node/cardinality ::hf/one}
                    (when (symbol? spec)
                      (throw (ex-info "Unknown cardinality, please define a spec." {:missing spec}))))))))
       (d/db-with db)))

(defn resolve-arguments-spec-pass "For each function argument, infer argument spec info."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :call]] db))
    (mapcat (fn [node] ; for each function call
              (let [spec-args (::spec/keys (datafy (spec/args (:function/name node))))]
                (mapv (fn [arg]
                        {:db/id     (:db/id arg)
                         :spec/name (nth spec-args (:node/position arg))})
                  (arguments node)))))
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
        scopes (set (nodes db (d/q '[:find [?e ...] :where [?e :node/cardinality ::hf/many]] db)))] ; find all cardinality many functions
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

(defn compute-argument-scope-pass "Compute the scope of all fn arguments. A function arg is in the scope of it’s function."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (map (fn [{:keys [db/id] :as node}]
           {:db/id      id
            :node/scope (:db/id (:node/scope (parent node)))}))
    (d/db-with db)))

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
          (recur (hyperfiddle.hfql.impl/parent node) r)))))

(defn compute-reference-join-path-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/reference]] db))
    (mapcat (fn [node]
              (let [join-node (find-join-target-point node)]
                [{:db/id               (:db/id node)
                  :node/reference      (:db/id join-node)
                  :node/reference-path (if (= join-node (:node/reference node)) ; happens when a prop node refers to the node it’s attached to
                                         []
                                         (reference-path join-node (:node/reference node)))
                  :node/dependencies   (:db/id join-node)}
                 [:db/retract (:db/id node) :node/dependencies (:db/id (:node/reference node))]
                 {:db/id                         (:db/id join-node)
                  :node/remember-entity-at-point true}])))
    (d/db-with db)))

(defn decide-how-node-should-render-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :render-point]] db))
    (map (fn [node] (if (= ::hf/many (:node/cardinality node))
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
                (into [{:db/id id, :node/columns columns}]
                  (map (fn [parent]
                         ;; (when (= ::hf/many (:node/cardinality parent)))
                         {:db/id (:db/id parent), :node/columns columns})
                    (:node/_children node)  ; look for all parents (including hf/options)
                    )))))
    (d/db-with db)))

(defn handle-free-inputs-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (mapcat (fn [node]
              (when (= '. (:node/form node))
                (let [tempid (- (:db/id node))]
                  [{:db/id             (:db/id node)
                    :node/input        tempid
                    :node/dependencies tempid
                    :node/children     tempid}
                   {:db/id       tempid
                    :node/type   :input
                    :node/symbol (symbol (str (:node/symbol node) "_input"))
                    :node/scope  (:db/id (:node/scope node))}]))))
    (d/db-with db)))

;; ranking ---
(defn dependencies "Return a set of transitive dependencies of a render point" [point]
  (let [deps (:node/dependencies point #{})]
    (into deps (mapcat dependencies deps))))

(defn rank-in-scope "Given a node, compute it’s rank (0 = no dependency) in the node’s scope" [node]
  (let [scope         (:node/scope node)
        deps-in-scope (filter #(= scope (:node/scope %)) (dependencies node))]
    (if (empty? deps-in-scope) 0 (+ 1 (apply max (map rank-in-scope deps-in-scope))))))

(defn compute-ranks-in-scope-pass [env db]
  (let [rank-in-scope (memoize rank-in-scope)]
    (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/scope]] db))
      (group-by :node/scope)
      (vals)
      (mapcat (fn [nodes] (map (juxt :db/id rank-in-scope) nodes)))
      (into {})
      (reduce-kv (fn [r id rank] (conj r {:db/id id, :node/scope-rank rank})) [])
      (d/db-with db))))

;; end ranking ---

(defn entity-symbol [point] (symbol (str "e_" (:db/id point))))

(defn input-path
  "An input path is a unique indentifier for an input in the HFQL runtime
  tree. An HFQL tree’s input state can be persisted in the form of a map {path
  value}. A node path is the breadcrumbs from root to node.
  eg.: [`(orders needle) e :order/shirt-size `(shirt-sizes gender .) :needle], where
  e is the value of entity id in scope, identifying a unique input under cardinality many."
  [point]
  (loop [account-for-cardinality false
         path                    ()
         point                   point]
    (if (nil? point)
      (vec path)
      (case (:node/type point)
        :argument (recur false (cons (:spec/name point) path) (parent point))
        (case (:node/form-type point)
          :group (recur true path (parent point))
          :call  (if (and (= ::hf/many (:node/cardinality point)) account-for-cardinality)
                   (recur true (list* (:node/symbolic-form point) (entity-symbol point) path) (parent point))
                   (recur true (cons (:node/symbolic-form point) path) (parent point)))
          (if-let [form (:node/symbolic-form point)]
            (recur true (cons form path) (parent point))
            (recur true path (parent point))))))))

(defn compute-input-path-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :input]] db))
    (mapcat (fn [node] [{:db/id      (:db/id node)
                         :input/path (input-path node)}]))
    (d/db-with db)))

(defn hf-link-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :prop/key ::hf/link]] db))))

;; Passes to apply in order.
(def passes [#'resolve-functions-pass
             #'resolve-cardinalities-pass
             #'resolve-arguments-spec-pass
             #'move-props-to-group-pass
             #'compute-point-dependencies-pass
             #'compute-function-call-dependencies-pass
             #'compute-scopes-pass
             #'compute-argument-scope-pass
             #'compute-options-dependencies-pass
             #'compute-points-symbol-pass
             #'compute-arg-symbol-pass
             #'resolve-lexical-references-pass
             #'compute-reference-join-path-pass
             #'decide-how-node-should-render-pass
             #'compute-columns-pass
             #'handle-free-inputs-pass
             #'compute-ranks-in-scope-pass
             ;; #'rename-%-to-entity-pass ; future work
             #'compute-input-path-pass
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
                   ::hf/link    (if (seq? v)
                                  (into r (-> (parse gen-id parent-id v) (update 0 assoc :prop/key k, :node/position index)))
                                  (throw (ex-info "Linking with a symbol is not support yet. Use `(page arg)` syntax for now." {})))
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
                              :node/input        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one} ; A function arg might allocate an input
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

(defn sort-by-rank "Sort a collection of render points by their rank (dependencies in scope). Used to order lexical bindings."
  [points] (sort (fn [x y]
                   (cond (< (:node/scope-rank x) (:node/scope-rank y)) -1 ; not using subtraction for readability
                         (> (:node/scope-rank x) (:node/scope-rank y)) 1
                         :else                (< (:db/id x) (:db/id y))) ; if same rank, sort by position in the HFQL expr
                   ) points))

(declare emit)

(defn scope-bindings "Return a toposorted list of lexical bindings in the scope of the given render point"
  [point]
  (mapcat (fn [point] [(:node/symbol point) (emit point)]) (sort-by-rank (:node/_scope point))))

(defn add-scope-bindings
  "If the given render point has dependencies in this scope, emit them as lexical bindings."
  [point emitted-form]
  (if-some [bindings (seq (scope-bindings point))]
    `(let [~'entities  (atom {})
           ~'<entities (p/watch ~'entities)
           ~@bindings] ~emitted-form)
    emitted-form))

(defn maybe-update "Like `update` but applies `f` only if `k` is present in the map."
  [m k f]
  (if (contains? m k) (update m k f) m))

(defn emit-props "Emit a map of {prop-key prop-value} for a given render point." [point]
  (let [props-map (->> (props point)
                    (map (fn [{:keys [prop/key prop/value] :as prop}]
                           [key (case key
                                  ::hf/options `hf/options
                                  ::hf/as      (list 'quote value)
                                  ::hf/link    (:node/symbol prop)
                                  value)]))
                    (into {}))
        args      (when (= :call (:node/form-type point)) (arguments point))]
    (cond-> props-map
      (:node/render-as point)               (assoc ::hf/render-as (:node/render-as point))
      (:node/columns point)                 (assoc ::hf/columns (:node/columns point))
      (not= :group (:node/form-type point)) (assoc ::hf/attribute (:node/symbolic-form point))
      (:input/path point)                   (assoc ::hf/path (:input/path point))
      (seq args)                            (assoc ::hf/arguments (mapv (fn [arg]
                                                                          (let [path (:input/path (:node/input arg))]
                                                                            [(:spec/name arg)
                                                                             (merge
                                                                               {::hf/read (:node/symbol arg)
                                                                                ::hf/path path}
                                                                               (when-let [input (:node/input arg)]
                                                                                 {::hf/write (:node/symbol input)}))])) args)))))

(def ^:dynamic *bindings*)

(defn emit-call [point]
  (let [f    (:function/name point)
        args (map (fn [node] `(unreduced (new ~(:node/symbol node)))) (arguments point))]
    (case (:prop/key point)
      ::hf/link `(list '~f ~@args)
      (cons `(p/partial-dynamic ~*bindings* ~f) args))))

(defn emit-argument [node]
  (if-let [ref (:node/reference node)]
    (if (empty? (:node/reference-path node))
      `(p/fn [] (binding [hf/bypass-renderer true] (new hf/value)))
      `(p/fn []
         (if-let [e# (get ~'<entities '~(:node/symbol ref))]
           (binding [hf/entity e# ; FIXME beginning of hf/context. entities is shadowed by card many, lookup should walk up a stack (can store parent atom is special key in atom)
                     hf/bypass-renderer true]
             (when-not (number? hf/entity) (throw (ex-info "not an entity ID" {:value hf/entity})))
             (new ; FIXME photon bug? had to wrap into a p/fn to get hf/entity to have the correct binding
               (p/fn []
                 (get-in (new ~(:node/symbol ref)) ~(:node/reference-path node)))))
           (throw r/pending)))) ; FIXME only join the required path, lazily.
    (if-let [input (:node/input node)]
      `(p/fn [] (get-in hf/route ~(:input/path input)))
      `(p/fn [] ~(:node/form node)))))

(defn hf-options-symbol [point continuation]
  (let [opts (:node/symbol (props ::hf/options continuation))]
    (when-not (= opts (:node/symbol point))
      opts)))

(defn emit "Emit clojure code for a render point and its dependencies." [point]
  (case (:node/type point)
    :render-point
    `(p/fn []
       ~(let [gen-continuation (fn [value continuation]
                                 `(binding [hf/entity  ~value
                                            ;; we pass options as a dynamic binding because a
                                            ;; group depends on options to emit props and
                                            ;; options depends on the group to emit the
                                            ;; continuation (circular ref).
                                            ~@(when-let [options (hf-options-symbol point continuation)]
                                                `[hf/options ~options])]
                                    (when-not (number? hf/entity) (throw (ex-info "not an entity ID" {:value hf/entity})))
                                    ~(add-scope-bindings point `(new ~(:node/symbol continuation)))))]
          (case (:node/form-type point)
            (:keyword :symbol)
            (let [attribute (:node/form point)
                  value     (case (:node/form-type point)
                              :keyword `(hf/*nav!* hf/db hf/entity ~attribute)
                              :symbol  `((p/partial-dynamic ~*bindings* ~(:function/name point)) hf/entity))
                  form      (if-some [continuation (first (children point))]
                              (let [entity                (gensym "entity")
                                    card-one-continuation `(p/fn [~entity] ~(gen-continuation entity continuation))]
                                (case (:node/cardinality point)
                                  ::hf/one  (gen-continuation value continuation)
                                  ::hf/many `(hf/FanOut. ~card-one-continuation ~value ~(emit-props point))
                                  `(let [value#                 ~value
                                         Card-one-continuation# ~card-one-continuation]
                                     (case (hf/*cardinality* hf/*schema* hf/db ~attribute)
                                       ::hf/one (new Card-one-continuation# value#)
                                       ::hf/many (hf/FanOut. Card-one-continuation# value# ~(merge (emit-props point) {::hf/render-as ::hf/infer}
                                                                                              (when-let [options (hf-options-symbol point continuation)]
                                                                                                {::hf/options options})))))))
                              ;; No continuation, so cardinality doesn’t matter, we produce a final value.
                              `(binding [hf/value   (p/fn [] (reduced ~value))
                                         hf/options ~(:node/symbol (props ::hf/options point))]
                                 (hf/Render. hf/value ~(emit-props point))))]
              (if (:node/remember-entity-at-point point)
                `(do (swap! ~'entities assoc '~(:node/symbol point) hf/entity) ;; TODO premise of hf/context
                     ~form)
                form))
            :call  (let [value (emit-call point)]
                     (if-some [continuation (first (children point))]
                       (case (:node/cardinality point)
                         ::hf/one  (gen-continuation value continuation)
                         ::hf/many `(hf/FanOut. (p/fn [~(entity-symbol point)] ~(gen-continuation (entity-symbol point) continuation))
                                      ~value
                                      ~(emit-props point)))
                       ;; if there is no continuation, just emit the call. e.g. links
                       value))
            :group (add-scope-bindings point
                     `(binding [hf/value (p/fn [] ~(reduce (fn [r point] (assoc r (:node/symbolic-form point) (:node/symbol point)))
                                                     {} (children point)))]
                        (hf/Render. hf/value ~(emit-props point)))))))
    :argument (emit-argument point)
    :input    `(atom nil)
    (assert false (str "emit - not a renderable point " (:node/type point)))))

;;;;;;;;;;;
;; Macro ;;
;;;;;;;;;;;

(defn hfql* [env bindings form]
  (binding [*bindings* bindings]
    (->> (analyze form)
      (apply-passes passes (c/normalize-env env))
      (get-root)
      (emit)
      (list 'new) ; emit produces a p/fn
      )))

(defmacro hfql [form] (hfql* &env form))
