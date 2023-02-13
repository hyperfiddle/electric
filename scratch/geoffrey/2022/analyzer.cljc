(ns hyperfiddle.analyzer
  (:require [datascript.core :as d]))

(def SCHEMA {:node/body      {:db/cardinality :db.cardinality/one,  :db/valueType :db.type/ref}
             :call/target    {:db/cardinality :db.cardinality/one,  :db/valueType :db.type/ref}
             :call/arguments {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}
             :node/bindings  {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}
             :map/keys       {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}
             :map/values     {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}
             :node/meta      {:db/cardinality :db.cardinality/one,  :db/valueType :db.type/ref}
             :node/children  {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}
             })

(def ^:dynamic *gen-id*)

(defn parse* [form]
  (-> (cond (symbol? form)  {:node/type ::symbol}
            (keyword? form) {:node/type ::keyword}
            (map? form)     {:node/type  ::map,
                             :map/keys   (doall (map-indexed (fn [idx form] (assoc (parse* form) :node/position idx)) (keys form))),
                             :map/values (doall (map-indexed (fn [idx form] (assoc (parse* form) :node/position idx)) (vals form)))}
            (vector? form)  {:node/type     ::vector
                             :node/children (doall (map-indexed (fn [idx form] (assoc (parse* form) :node/position idx)) form))}
            (set? form)     {:node/type     ::set
                             :node/children (mapv parse* form)}
            (seq? form)     {:node/type      ::call
                             :call/target    (parse* (first form))
                             :call/arguments (doall (map-indexed (fn [idx form] (assoc (parse* form) :node/position idx)) (rest form)))}
            (var? form)     {:node/type ::var}
            :else           {:node/type ::const})
    (assoc :db/id (*gen-id*), :node/form form)))

(defn parse [form]
  (binding [*gen-id* (partial swap! (atom 0) dec)]
    (parse* form)))

(defn to-graph [tx]
  (d/db-with @(d/create-conn SCHEMA) tx))


(defn resolve-pass [env db])
(defn macroexpand-pass [env db])

(def PASSES [#'macroexpand-pass])

(defn emit* [node]
  (case (:node/type node)
    (::const ::keyword ::symbol) (:node/form node)
    ::map                        (into {} (zipmap
                                            (map emit* (sort-by :node/position (:map/keys node)))
                                            (map emit* (sort-by :node/position (:map/values node)))))
    ::vector                     (into [] (map emit* (sort-by :node/position (:node/children node))))
    ::set                        (into #{} (map emit* (:node/children node)))
    ::call                       (list* (emit* (:call/target node)) (mapv emit* (:call/arguments node)))
    ::var                        (:node/form node)))

(defn emit [db] (emit* (d/entity db 1)))

(comment

  (parse 'a)
  (-> [(parse '(#'clojure.core/identity (let [a 1]
                                          [{:a a, :b 2} #{:a :b :c}])))]
    (to-graph)
    (emit)
    (eval)
    )
  )
