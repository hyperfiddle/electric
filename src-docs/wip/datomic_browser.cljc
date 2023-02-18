(ns wip.datomic-browser
  "must have datomic on classpath, and must load 'test ns"
  #?(:cljs (:require-macros wip.datomic-browser))
  #?(:cljs (:import [goog.math Long])) ; only this require syntax passes shadow in this file, why?
  (:require clojure.edn
            [contrib.str :refer [any-matches?]]
            [contrib.data :refer [unqualify treelister]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:cljs contrib.datomic-cloud-contrib)
            [contrib.datomic-m #?(:clj :as :cljs :as-alias) d]
            [contrib.ednish :as ednish]
            [wip.explorer :as explorer :refer [Explorer]]
            [wip.gridsheet :as-alias gridsheet]
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.router :as router]
            #?(:cljs [hyperfiddle.router-html5 :as html5])
            [missionary.core :as m]))

; Todo needs cleanup
; - port to nested router
; - port to HFQL maybe

(e/def conn)
(e/def db)
(e/def schema) ; used by entity-tree-entry-children and FormatEntity in this file only

(e/defn RecentTx []
  (e/client (dom/h1 (dom/text "Recent Txs")))
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Format (e/fn [[e _ v tx op :as record] a]
                              (case a
                                :db/id (e/client (router/link [::tx tx] (dom/text tx)))
                                :db/txInstant (e/client (dom/text (pr-str v))) #_(e/client (.toLocaleDateString v))))]
    (Explorer.
      (treelister (new (->> (d/datoms> db {:index :aevt, :components [:db/txInstant]})
                                   (m/reductions conj ())
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 30
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em auto"})))

(e/defn Attributes []
  (e/client (dom/h1 (dom/text "Attributes")))
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (e/fn [row col]
                              (e/client
                                (let [v (col row)]
                                  (case col
                                    :db/ident (router/link [::attribute v] (dom/text v))
                                    :db/valueType (some-> v :db/ident name dom/text)
                                    :db/cardinality (some-> v :db/ident name dom/text)
                                    :db/unique (some-> v :db/ident name dom/text)
                                    (dom/text (str v))))))]
    (Explorer.
      (treelister (->> (dx/attributes> db explorer/cols)
                              (m/reductions conj [])
                              (m/relieve {})
                              new
                              (sort-by :db/ident)) ; sort by db/ident which isn't available
        (fn [_]) any-matches?)
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

(e/defn Format-entity [[k v :as row] col]
  (assert (some? schema))
  (case col
    ::k (cond
          (= :db/id k) (e/client (dom/text k)) ; :db/id is our schema extension, can't nav to it
          (contains? schema k) (e/client (router/link [::attribute k] (dom/text k)))
          () (e/client (dom/text (str k)))) ; str is needed for Long db/id, why?
    ::v (if-not (coll? v) ; don't render card :many intermediate row
          (let [[valueType cardinality]
                ((juxt (comp unqualify dx/identify :db/valueType)
                       (comp unqualify dx/identify :db/cardinality)) (k schema))]
            (cond
              (= :db/id k) (e/client (router/link [::entity v] (dom/text v)))
              (= :ref valueType) (e/client (router/link [::entity v] (dom/text v)))
              () (e/client (dom/text (pr-str v))))))))

(e/defn EntityDetail [e]
  (assert e)
  (e/client (dom/h1 (dom/text "Entity detail: " e))) ; treeview on the entity
  (binding [explorer/cols [::k ::v] explorer/Format Format-entity]
    (Explorer.
      ;; TODO inject sort
      (treelister (new (e/task->cp (d/pull db {:eid e :selector ['*] :compare compare})))
        (partial dx/entity-tree-entry-children schema)
        any-matches?)
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em auto"})))

(e/defn EntityHistory [e]
  (assert e)
  (e/client (dom/h1 (dom/text "Entity history: " (pr-str e))))
  (binding [explorer/cols [::e ::a ::op ::v ::tx-instant ::tx]
            explorer/Format (e/fn [[e aa v tx op :as row] a]
                              (when row ; when this view unmounts, somehow this fires as nil
                                (case a
                                  ::op (e/client (dom/text (name (case op true :db/add false :db/retract))))
                                  ::e (e/client (router/link [::entity e] (dom/text e)))
                                  ::a (if (some? aa)
                                        (let [ident (:db/ident (new (e/task->cp (d/pull db {:eid aa :selector [:db/ident]}))))]
                                          (e/client (dom/text (pr-str ident)))))
                                  ::v (e/client (some-> v pr-str dom/text))
                                  ::tx (e/client (router/link [::tx tx] (dom/text tx)))
                                  ::tx-instant (let [x (:db/txInstant (new (e/task->cp (d/pull db {:eid tx :selector [:db/txInstant]}))))]
                                                 (e/client (pr-str (dom/text x))))
                                  (str v))))]
    (Explorer.
      ; accumulate what we've seen so far, for pagination. Gets a running count. Bad?
      (treelister (new (->> (dx/entity-history-datoms> db e)
                                   (m/reductions conj []) ; track a running count as well?
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em 10em 3em auto auto 9em"})))

(e/defn AttributeDetail [a]
  (e/client (dom/h1 (dom/text "Attribute detail: " a)))
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (e/fn [[e _ v tx op :as x] k]
                              (e/client
                                (case k
                                  :e (router/link [::entity e] (dom/text e))
                                  :a (dom/text (pr-str a)) #_(let [aa (new (e/task->cp (dx/ident! db aa)))] aa)
                                  :v (some-> v pr-str dom/text) ; when a is ref, render link
                                  :tx (router/link [::tx tx] (dom/text tx)))))]
    (Explorer.
      (treelister (new (->> (d/datoms> db {:index :aevt, :components [a]})
                                   (m/reductions conj [])
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(e/defn TxDetail [e]
  (e/client (dom/h1 (dom/text "Tx detail: " e)))
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (e/fn [[e aa v tx op :as x] a]
                              (case a
                                :e (let [e (new (e/task->cp (dx/ident! db e)))] (e/client (router/link [::entity e] (dom/text e))))
                                :a (let [aa (new (e/task->cp (dx/ident! db aa)))] (e/client (router/link [::attribute aa] (dom/text aa))))
                                :v (pr-str v) ; when a is ref, render link
                                (str tx)))]
    (Explorer.
      (treelister (new (->> (d/tx-range> conn {:start e, :end (inc e)}) ; global
                                   (m/eduction (map :data) cat)
                                   (m/reductions conj [])
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(e/defn DbStats []
  (e/client (dom/h1 (dom/text "Db stats")))
  (binding [explorer/cols [::k ::v]
            explorer/Format (e/fn [[k v :as row] col]
                              (e/client
                                (case col
                                  ::k (dom/text (pr-str k))
                                  ::v (cond
                                        (= k :attrs) nil ; print children instead
                                        () (dom/text (pr-str v))))))] ; {:count 123}
    (Explorer.
      (treelister
        (new (e/task->cp (d/db-stats db)))
        (fn [[k v]] (condp = k :attrs (into (sorted-map) v) nil))
        any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "20em auto"})))

(comment
  {:datoms 800958,
   :attrs
   {:release/script {:count 11435},
    :label/type {:count 870}
    ... ...}})

(e/defn Page [[self state [local-page x]]]
  (dom/h1 (dom/text "Datomic browser"))
  (dom/link (dom/props {:rel :stylesheet, :href "user/gridsheet-optional.css"}))
  (dom/div (dom/props {:class "user-gridsheet-demo"})
    (dom/div (dom/text "Nav: ")
      (router/link [::summary] (dom/text "home")) (dom/text " ")
      (router/link [::db-stats] (dom/text "db-stats")) (dom/text " ")
      (router/link [::recent-tx] (dom/text "recent-tx")))
    (router/router 1 ; focus explorer state
      (e/server
        (case (or local-page ::summary)
          ::summary (Attributes.)
          ::attribute (AttributeDetail. x)
          ::tx (TxDetail. x)
          ::entity (do (EntityDetail. x) (EntityHistory. x))
          ::db-stats (DbStats.)
          ::recent-tx (RecentTx.)
          (e/client (dom/text "no matching route: " (pr-str local-page))))))))

(e/defn DatomicBrowser []
  (println (pr-str (type 1))) ; show we're on the server
  (e/server ; bug that this is needed; above line shows we're already here
  (binding [conn @(requiring-resolve 'user/datomic-conn)]
    (binding [db (d/db conn)]
      (binding [schema (new (dx/schema> db))]
        (e/client
          (binding [router/build-route (fn [[self state local-route] local-route']
                                         ; root local links through this entrypoint for DI
                                         `[DatomicBrowser ~state ~local-route'])]
            (Page. router/route))))))))
