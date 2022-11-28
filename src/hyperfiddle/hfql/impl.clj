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
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.hfql :as-alias hfql]))

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
  (case (:node/type point)
    :argument (first (:node/_arguments point))
    (cond
      (:prop/key point) (first (:node/_props point))
      :else 
      (->> (or (seq (:node/_children point)) (seq (:node/_props point)))
        (sort-by :db/id) ; Rewriting the graph can create loops, but a parent always come before its child in the expression.
        (first)))))

(defn arguments "For a :call node, return its arguments, in order."
  [node]
  (assert (= :call (:node/form-type node)) "Can only get arguments of a :call node")
  (sort-by :node/position (:node/arguments node)))

(defn props "Get props of a given point"
  ([point] (sort-by :node/position (:node/props point)))
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
               (cond (keyword? f) (symbol (str (munge f) "_call_" (:db/id node)))
                     (symbol? f)  (let [name (symbol (str (munge* (name (:function/name node))) "_" (:db/id node)))]
                                    (case (:prop/key node)
                                      ::hf/options (symbol (str name "_options"))
                                      ::hf/link    (symbol (str name "_link"))
                                      name))))
    (case (:node/type node)
      :argument (let [function (parent node)]
                  (symbol (str (:node/symbol function) "_" (some-> (:spec/name node) name) "_" (:db/id node))))
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
                    :call    (list 'quote (cons (:function/name node) (map symbolic-form (arguments node)))))
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
  (->> (nodes db (d/q '[:find [?e ...] :where (or [?e :function/name] [?e :node/form-type :keyword]) (not [?e :node/quoted?])] db))
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
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/form-type :call] (not [?e :node/quoted?])] db))
    (mapcat (fn [node] ; for each function call
              (let [spec-args (::spec/keys (datafy (spec/args (:function/name node))))
                    spec-args (concat spec-args (repeat (count (arguments node)) (last spec-args)))]
                (mapv (fn [arg]
                        {:db/id     (:db/id arg)
                         :spec/name (nth spec-args (:node/position arg))})
                  (arguments node)))))
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

(defn get-root "Return the only node with no parent. The top-most expression node." [db]
  (sort-by :node/position
    (map (partial d/entity db) (d/q '[:find [?e ...]
                                      :where [?e]
                                      (not [_ :node/children ?e])  ; find nodes without parent
                                      (not [_ :node/props ?e])     ; which aren’t props
                                      (not [_ :node/arguments ?e]) ; nor args
                                      ]
                                 db))))

(defn compute-scopes-pass
  "An HFQL scope is a lexical scope. A point can refer to another point as long as
  this point is not declared under a cardinality many dependency. E.g.

  {(submissions \"\") [{:order/gender [:db/ident]} ; `ident` is accessible under the `submissions` scope
                       (props {:order/shirt-size [:db/ident]} {::hf/options (shirt-sizes ident)}]  ; ident can be refered here
   (genders) [:db/ident]}               ; this `ident` is not accessible to shirt-sizes because `genders` is cardinality many. No ambiguity.


  "[env db]
  (let [scopes (set (nodes db (d/q '[:find [?e ...] :where [?e :node/cardinality ::hf/many]] db)))] ; find all cardinality many functions
    (->> (nodes db (d/q '[:find [?e ...]
                          :where
                          [?e]
                          #_(not [_ :node/children ?e])] db)) ; get all points but the root
         (mapcat (fn [{:keys [db/id] :as start}]
                   (loop [prev start
                          node (parent start)] ; walk ancestors up to the root
                     (cond
                       (nil? node) (when (not= prev start)
                                     [{:db/id      id ; if this node does not have a cardinality
                                       :node/scope (:db/id prev)}]) ; many parent it is in the root scope.
                       (contains? scopes node) [{:db/id      id              ; otherwise it is in the scope of it’s closest
                                                 :node/scope (:db/id node)}] ; cardinality many parent.
                       :else                   (recur node (parent node))))))
         (d/db-with db))))

(defn compute-argument-scope-pass "Compute the scope of all fn arguments. A function arg is in the scope of it’s function."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (map (fn [{:keys [db/id] :as node}]
           {:db/id      id
            :node/scope (:db/id (parent node))}))
    (d/db-with db)))

(defn nodes-in-scope [db node]
  (if-not (:node/scope node) ; root
    (->> (nodes db (d/q '[:find [?e ...] :where [?e] (not [?e :node/scope])] db))
      (reduce (fn [r node] (into r (conj (set (:node/_scope node)) node) #_node)) #{}))
    (set (:node/_scope (:node/scope node)))))

(defn find-nodes-by-name
  "Look up for nodes named by `symbol` in the `point`’s scope, then in the parent scope, recursively.
  If there is more than one match, they are guaranteed to belong to the same
  scope. `point` cannot be a match (meaning a point cannot refer to itself)."
  ([db point symbol]
   (find-nodes-by-name db point point symbol))
  ([db starting-point current-point symbol]
   (if-let [matches (->> (disj (nodes-in-scope db current-point) starting-point) ; get all nodes but the current one, in the current scope
                      (filter #(= symbol (:node/name %)))
                      (seq))]
     (set matches)
     (when-let [scope (:node/scope current-point)]
       (recur db starting-point scope symbol))))) ; look up in parent scope

(defn resolve-lexical-references-pass
  "For all function arguments, resolve the node they might refer to in lexical
  scope. Record the dependency between the argument and the graph node, and
  rewrite the argument symbol to point to the graph node."
  [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (mapcat (fn [{:keys [node/form db/id] :as node}]
           (when (symbol? form)
             (when-some [matches (seq (find-nodes-by-name db (parent node) form))]
               (case (count matches)
                 0 nil
                 1 [{:db/id id, #_#_:node/dependencies (:db/id (first matches)), :node/reference (:db/id (first matches))}]
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
         r    ()]
    (cond (nil? node)           r
          (some? (parent node)) (recur (parent node) (conj r (parent node)))
          :else                 r)))

(defn find-join-target-point "Find the closest point we need to join to resolve a reference." [node]
  (loop [node-parents (ancestors node)
         ref-parents  (ancestors (:node/reference node))]
    (if (= (first node-parents) (first ref-parents))
      (recur (rest node-parents) (rest ref-parents))
      (first ref-parents))))

(defn reference-path "Return a path (can be passed to get-in), between two parent/child nodes." [parent child]
  (if (contains? (set (ancestors child)) parent)
    (conj (->> (ancestors child)
            (drop-while #(not= parent %))
            (drop 1)
            (mapv :node/symbolic-form))
      (:node/symbolic-form child))
    []))

(defn compute-reference-join-path-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/reference]] db))
    (mapcat (fn [node]
              (when-let [join-node (find-join-target-point node)]
                [{:db/id               (:db/id node)
                  :node/reference      (:db/id join-node)
                  :node/reference-path (if (= join-node (:node/reference node)) ; happens when a prop node refers to the node it’s attached to
                                         []
                                         (reference-path join-node (:node/reference node)))
                  :node/dependencies   (:db/id join-node)}
                 [:db/retract (:db/id node) :node/dependencies (:db/id (:node/reference node))]
                 {:db/id (:db/id join-node)}])))
    (d/db-with db)))

(defn compute-columns-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/children]] db))
    (map (fn [{:keys [db/id node/children] :as node}]
           {:db/id id, :node/columns (mapv :node/symbolic-form (sort-by :node/position children))}))
    (d/db-with db)))

(defn handle-free-inputs-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/type :argument]] db))
    (mapcat (fn [node]
              (when (= '. (:node/form node))
                [{:db/id            (:db/id node)
                  :node/free-input? true}])))
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

(def ^:const E (gensym "entity_"))

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
          :call  (if (and (= ::hf/many (:node/cardinality point)) account-for-cardinality)
                   (recur true (list* (:node/symbolic-form point) E path) (parent point))
                   (recur true (cons (:node/symbolic-form point) path) (parent point)))
          (if-let [form (:node/symbolic-form point)]
            (recur true (cons form path) (parent point))
            (recur true path (parent point))))))))

(defn compute-input-path-pass [env db]
  (->> (nodes db (d/q '[:find [?e ...] :where [?e :node/free-input? true]] db))
    (mapcat (fn [node] [{:db/id      (:db/id node)
                         :input/path (input-path node)}]))
    (d/db-with db)))

;; Passes to apply in order.
(def passes [#'resolve-functions-pass
             #'resolve-cardinalities-pass
             #'resolve-arguments-spec-pass
             #'compute-point-dependencies-pass
             #'compute-function-call-dependencies-pass
             #'compute-scopes-pass
             #'compute-argument-scope-pass
             #'compute-points-symbol-pass
             #'compute-arg-symbol-pass
             #'resolve-lexical-references-pass
             #'compute-reference-join-path-pass
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

(defn literal [x]
  `(hfql/literal
     ~(cond
        (vector? x) `vec
        (set? x)    `set
        (seq? x)    `identity
        (map? x)    `(partial apply hash-map)
        :else       `first)
     ~@x))

(defn parse-props "Parse a props map declared with (props <form> {k1 v1, …}). `parent-id` must be the node id the props are attached to."
  [gen-id parent-id props]
  (->> (map-indexed vector props) ; we want to emit-1 code in the same order as the hfql expr is read.
    (reduce (fn [r [index [k v]]] ; for each k v pair of the map, in seq order.
                 (case k
                   ::hf/options (into r (-> (parse gen-id parent-id v)
                                          (update 0 assoc :prop/key k, :node/position index, :node/_props parent-id)
                                          (update 0 dissoc :node/_children)))
                   ::hf/link    (let [quoted? (and (seq? v) (= 'quote (first v)))]
                                  (into r (cond-> (parse gen-id parent-id (literal (if quoted? (second v) v)))
                                            true (update 0 assoc :prop/key k, :node/position index, :node/_props parent-id)
                                            true (update 0 dissoc :node/_children)
                                            quoted? (update 0 assoc :node/quoted? true))))
                   #_(throw (ex-info "Linking with a symbol is not support yet. Use `(page arg)` syntax for now." {}))
                   (conj r {:db/id         (gen-id) ; create a node of type :prop with k and v
                            :node/type     :prop
                            :node/_props   parent-id
                            :prop/key      k
                            :prop/value    v
                            :node/position index})))
      [])))

(defn parse-arg "Parse a function argument into a tx. `parent-id` is supposed to be the function node’s id."
  [gen-id parent-id form]
  (->
    (cond
      ;; Props on args are allowed, for instance an argument could render as a select-option.
      (props? form) (let [[_ form props] form
                          arg            (parse-arg gen-id parent-id form)] ; parse the arg
                      (if (empty? props)
                        arg
                        (into arg (parse-props gen-id (-> arg first :db/id) props))  ; attach props a children of the arg
                        ))
      :else         (let [tx {:db/id           (gen-id)
                              :node/_arguments parent-id
                              :node/type       :argument}]
                      (if (nil? form) [tx] [(assoc tx :node/form form)])))
    (update 0 assoc :form/meta (or (meta form) {}))))

(defn parse
  "Parse an HFQL form into a datascript tx, representing a graph."
  ([form] (parse (partial swap! (atom 0) dec) nil form))
  ([gen-id parent-id form]
   (->
     (cond
       ;; Props are extra info attached to a form, like metadata. Some props are
       ;; interpreted, like ::hf/options.
       (props? form)   (let [[_ form props] form
                             parsed         (parse gen-id parent-id form)] ; parse the form
                         (if (empty? props) parsed
                             (into parsed (parse-props gen-id (-> parsed first :db/id) props)  ; parse and attach props as children of the form
                               )))
       ;; A vector represents a group. It usually renders as a form (card one) or a table (card many).
       (vector? form)  (into [] (comp
                                  (map (partial parse gen-id parent-id))  ; parse each member of the group
                                  (map-indexed (fn [idx tx] (update tx 0 assoc :node/position idx)))  ; record their position (we want colums and fields in the same order as the HFQL expression.
                                  cat) ; concat into a single tx
                         form)
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
                           )))
     (update 0 assoc :form/meta (or (meta form) {})))))

(defn to-graph [tx]
  (d/db-with @(d/create-conn {:db/id             {:db/unique :db.unique/identity}
                              :node/children     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} ; syntactical children in expr
                              :node/props        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
                              :node/arguments    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
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

(defn sort-by-rank "Sort a collection of render points by their rank (dependencies in scope). Used to order lexical bindings."
  [points] (sort (fn [x y]
                   (cond (< (:node/scope-rank x) (:node/scope-rank y)) -1 ; not using subtraction for readability
                         (> (:node/scope-rank x) (:node/scope-rank y)) 1
                         :else                (< (:db/id x) (:db/id y))) ; if same rank, sort by position in the HFQL expr
                   ) points))

(defn maybe-update "Like `update` but applies `f` only if `k` is present in the map."
  [m k f]
  (if (contains? m k) (update m k f) m))

(defn hf-options-symbol [point continuation]
  (let [opts (:node/symbol (props ::hf/options continuation))]
    (when-not (= opts (:node/symbol point))
      opts)))

(declare emit-1)

(defn scope-bindings "Return a toposorted list of lexical bindings in the scope of the given render point"
  [point]
  (mapcat (fn [point] [(:node/symbol point) (emit-1 point)]) (sort-by-rank (filter (fn [n] (= :argument (:node/type n))) (:node/_scope point)))))

(defn add-scope-bindings
  "If the given render point has dependencies in this scope, emit-1 them as lexical bindings."
  [point emitted-form]
  (if-some [bindings (seq (scope-bindings point))]
    `(let [~@bindings] ~emitted-form)
    emitted-form))

(declare emit-nodes emit-call)

(defn emit-props "Emit a map of {prop-key prop-value} for a given render point." [point]
  (let [props-map (->> (props point)
                    (map (fn [{:keys [prop/key prop/value] :as prop}]
                           [key (case key
                                  ::hf/options (add-scope-bindings prop `(p/fn [] ~(emit-call prop)))
                                  ::hf/link    (add-scope-bindings prop `(p/fn [] ~(emit-call prop)))
                                  ::hf/as      (list 'quote value)
                                  value)]))
                    (into {}))
        args      (when (= :call (:node/form-type point)) (arguments point))]
    (cond-> props-map
      true                            (merge {::hf/entity E
                                              :dbg/name   (list 'quote (:node/symbol point))})
      (empty? (:node/children point)) (assoc ::hf/type ::hf/leaf)
      (::hf/options props-map)        (assoc ::hf/continuation (when-some [continuation (some-> (:node/children point) seq (emit-nodes))]
                                                           `(p/fn [~E] ~continuation)))
      (:node/cardinality point)       (assoc ::hf/cardinality (:node/cardinality point))
      (:node/columns point)           (assoc ::hf/keys (:node/columns point))
      (:node/form-type point)         (assoc ::hf/attribute (:node/symbolic-form point))
      (:input/path point)             (assoc ::hf/path (:input/path point))
      (seq args)                      (assoc ::hf/arguments (mapv (fn [arg]
                                                                    (let [path (:input/path arg)]
                                                                      [(:spec/name arg)
                                                                       (merge
                                                                         {::hf/read     (:node/symbol arg)
                                                                          ::hf/readonly (not (:node/free-input? arg))
                                                                          ::hf/path     path}
                                                                         (when-let [options (props ::hf/options arg)]
                                                                           {::hf/options (add-scope-bindings options `(p/fn [] ~(emit-call options)))}))]))
                                                        args)))))

(def ^:dynamic *bindings*)

(defn convey-dynamic-env [sym]
  (if (seq *bindings*)
    `(p/partial-dynamic ~*bindings* ~sym)
    sym))

(defn emit-call [point]
  (let [f    (:function/name point)
        args (map (fn [node] `(new ~(:node/symbol node))) (arguments point))]
    (if (:node/quoted? point)
      `(list '~f ~@args)
      (cons (convey-dynamic-env f) args))))

(defn wrap-default [argument-node form]
  (if-let [Default (:prop/value (props ::hf/default argument-node))]
    `(new ~Default ~form)
    form))

(defn emit-argument [node]
  (if-let [ref (:node/reference node)]
    `(p/fn [] ~(wrap-default node `(get-in (hyperfiddle.hfql/JoinArg. ~(:node/symbol ref)) ~(:node/reference-path node))))
    (if (:node/free-input? node)
      `(p/fn [] ~(wrap-default node `(get-in hf/route ~(:input/path node))))
      `(p/fn [] ~(wrap-default node
                   (let [form (:node/form node)]
                     (if (= '% form)
                       E
                       form)))))))

(declare emit-nodes)

(defn self-ref? [point] (->> point :node/_reference (filter #(= point (parent (parent %)))) seq boolean))

(defn emit-1 "Emit clojure code for a render point and its dependencies." [point]
  (case (:node/type point)
    :render-point                       ; TODO drop :render-point type
    (case (:node/form-type point)
      (:keyword :symbol)
      (let [attribute (:node/form point)
            value     (case (:node/form-type point)
                        :keyword `(hf/*nav!* hf/db ~E ~attribute)
                        :symbol  `(~(convey-dynamic-env (:function/name point)) ~E))]
        (if-some [continuation (seq (:node/children point))]
          (let [card-one-continuation (gensym "continuation_")]
            `(let [~(:node/symbol point)
                   (p/fn []
                     (let [~card-one-continuation (p/fn [~E] ~(emit-nodes continuation))]
                       ~(case (:node/cardinality point)
                          ::hf/one  `(new ~card-one-continuation ~value)
                          ::hf/many `(p/for [e# ~value] (new ~card-one-continuation e#))
                          `(let [value# ~value]
                             (case (hf/*cardinality* hf/*schema* hf/db ~attribute)
                               ::hf/one  (new  ~card-one-continuation value#)
                               ::hf/many (p/for [e# value#] (new ~card-one-continuation e#)))))))]
               ~(assoc (emit-props point)
                  ::hf/Value (:node/symbol point))))
          ;; No continuation, so cardinality doesn’t matter, we produce a final value.
          `(let [~(:node/symbol point) (p/fn [] ~value)]
             ~(assoc (emit-props point) ::hf/Value (:node/symbol point)))))
      :call (add-scope-bindings point
              (let [value (emit-call point)]
                (assoc (emit-props point)
                  ::hf/Value
                  (if-some [continuation (seq (:node/children point))]
                    (let [continuation-sym (gensym "continuation_")]
                      `(p/fn [~@(when (= ::hf/options (:prop/key point))
                                  [(:node/symbol continuation)])
                              ]
                         ~(case (:node/cardinality point)
                            ::hf/one  `(let [~E ~value] ~(emit-nodes continuation)
                                            #_(new ~continuation-sym ~value))
                            ::hf/many `(p/for [~E ~value] ~(emit-nodes continuation)))))
                    ;; Some calls don’t have a continuation (e.g. Links)
                    `(p/fn [] ~value))))))
    :argument (emit-argument point)
    (assert false (str "emit-1 - not a renderable point " (:node/type point)))))

(defn emit-nodes [nodes]
  (if (or (> (count nodes) 1) (:node/position (first nodes)))
    (let [nodes  (sort-by :node/position nodes)
          keys   (mapv :node/symbolic-form nodes)
          values (->> (sort-by :node/position nodes)
                   (mapv (fn [node] (if (:node/_reference node) (:node/symbol node) (emit-1 node)))))
          shared (->> (filter :node/_reference nodes) (mapcat (fn [node] [(:node/symbol node) (emit-1 node)])))]
      `{::hf/type   ::hf/keys
        ::hf/keys   ~keys
        ::hf/values ~(if (seq shared)
                       `(let [~@shared] ~values)
                       values)})
    (emit-1 (first nodes))))

;;;;;;;;;;;
;; Macro ;;
;;;;;;;;;;;

(defn all-bindings [db]
  (->> (disj (nodes db (d/q '[:find [?e ...] :where [?e :node/scope]] db)) (get-root db))
    (group-by :node/scope)
    (map (fn [[node nodes]] [(:db/id node) (sort-by-rank nodes)]))
    (sort-by first >)
    (mapcat second)
    )
  )

(defn hfql* [env bindings form]
  (binding [*bindings* bindings]
    (let [db (->> (analyze form) (apply-passes passes (c/normalize-env env)))
          roots (get-root db)]
      `(let [~E hf/entity]
         ~(emit-nodes roots)))))

(defmacro hfql ([form] (hfql* &env [] form))
  ([bindings form] (hfql* &env bindings form)))


(comment

  (hfql {(wip.orders-datascript/orders "") [:db/id]}) 
  (hfql {:order/gender [:db/id :db/ident]}) 
  (graph '{:order/gender [:db/id :db/ident]})
  (hfql [:db/id :db/ident]) 
  (hfql [:db/id]) 
  (hfql :db/id) 


  )
