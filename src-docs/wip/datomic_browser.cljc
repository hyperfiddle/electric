(ns wip.datomic-browser
  "must have datomic on classpath, and must load 'test ns"
  #?(:cljs (:require-macros wip.datomic-browser))
  #?(:cljs (:import [goog.math Long])) ; only this require syntax passes shadow in this file, why?
  (:require clojure.edn
            [contrib.str :refer [any-matches?]]
            [contrib.data :refer [index-by unqualify]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:cljs contrib.datomic-cloud-contrib)
            [contrib.datomic-m #?(:clj :as :cljs :as-alias) d]
            [contrib.ednish :as ednish]
            [wip.explorer :as explorer :refer [Explorer]]
            [wip.gridsheet :as-alias gridsheet]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.router :as router]
            #?(:cljs [hyperfiddle.router-html5 :as html5])
            [missionary.core :as m]))

; Todo needs cleanup
; - port to nested router
; - port to HFQL maybe

(p/def conn)
(p/def db)
(p/def schema) ; used by entity-tree-entry-children and FormatEntity in this file only

(p/defn RecentTx []
  (p/client (dom/h1 (dom/text "Recent Txs")))
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Format (p/fn [[e _ v tx op :as record] a]
                              (case a
                                :db/id (p/client (router/link [::tx tx] (dom/text tx)))
                                :db/txInstant (p/client (dom/text (pr-str v))) #_(p/client (.toLocaleDateString v))))]
    (Explorer.
      (explorer/tree-lister (new (->> (d/datoms> db {:index :aevt, :components [:db/txInstant]})
                                   (m/reductions conj ())
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 30
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em auto"})))

(p/defn Attributes []
  (p/client (dom/h1 (dom/text "Attributes")))
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (p/fn [row col]
                              (p/client
                                (let [v (col row)]
                                  (case col
                                    :db/ident (router/link [::attribute v] (dom/text v))
                                    :db/valueType (some-> v :db/ident name dom/text)
                                    :db/cardinality (some-> v :db/ident name dom/text)
                                    :db/unique (some-> v :db/ident name dom/text)
                                    (dom/text (str v))))))]
    (Explorer.
      (explorer/tree-lister (->> (dx/attributes> db explorer/cols)
                              (m/reductions conj [])
                              (m/relieve {})
                              new
                              (sort-by :db/ident)) ; sort by db/ident which isn't available
        (fn [_]) any-matches?)
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

(p/defn Format-entity [[k v :as row] col]
  (assert (some? schema))
  (case col
    ::k (cond
          (= :db/id k) (p/client (dom/text k)) ; :db/id is our schema extension, can't nav to it
          (contains? schema k) (p/client (router/link [::attribute k] (dom/text k)))
          () (p/client (dom/text (str k)))) ; str is needed for Long db/id, why?
    ::v (if-not (coll? v) ; don't render card :many intermediate row
          (let [[valueType cardinality]
                ((juxt (comp unqualify dx/identify :db/valueType)
                       (comp unqualify dx/identify :db/cardinality)) (k schema))]
            (cond
              (= :db/id k) (p/client (router/link [::entity v] (dom/text v)))
              (= :ref valueType) (p/client (router/link [::entity v] (dom/text v)))
              () (p/client (dom/text (pr-str v))))))))

#?(:clj
   (defn entity-tree-entry-children [schema [k v :as row]] ; row is either a map-entry or [0 {:db/id _}]
     ; This shorter expr works as well but is a bit "lucky" with types in that you cannot see
     ; the intermediate cardinality many traversal. Unclear what level of power is needed here
     ;(cond
     ;  (map? v) (into (sorted-map) v)
     ;  (sequential? v) (index-by identify v))

     ; instead, dispatch on static schema in controlled way to reveal the structure
     (cond
       (contains? schema k)
       (let [x ((juxt (comp unqualify dx/identify :db/valueType)
                      (comp unqualify dx/identify :db/cardinality)) (k schema))]
         (case x
           [:ref :one] (into (sorted-map) v) ; todo lift sort to the pull object
           [:ref :many] (index-by dx/identify v) ; can't sort, no sort key
           nil #_(println `unmatched x))) ; no children

       ; in card :many traversals k can be an index or datomic identifier, like
       ; [0 {:db/id 20512488927800905}]
       ; [20512488927800905 {:db/id 20512488927800905}]
       ; [:release.type/single {:db/id 35435060739965075, :db/ident :release.type/single}]
       (number? k) (into (sorted-map) v)

       () (assert false (str "unmatched tree entry, k: " k " v: " v)))))

#?(:clj
   (tests
     ; watch out, test schema needs to match
     (entity-tree-entry-children test/schema [:db/id 87960930235113]) := nil
     (entity-tree-entry-children test/schema [:abstractRelease/name "Pour l’amour..."]) := nil
     (entity-tree-entry-children test/schema [:abstractRelease/type #:db{:id 35435060739965075, :ident :release.type/single}])
     := #:db{:id 35435060739965075, :ident :release.type/single}
     (entity-tree-entry-children test/schema [:abstractRelease/artists [#:db{:id 20512488927800905}
                                                                        #:db{:id 68459991991856131}]])
     := {20512488927800905 #:db{:id 20512488927800905},
         68459991991856131 #:db{:id 68459991991856131}}

     (def tree (m/? (d/pull test/datomic-db {:eid test/pour-lamour :selector ['*]})))
     (->> tree (map (fn [row]
                      (entity-tree-entry-children test/schema row))))
     := [nil
         nil
         nil
         #:db{:id 35435060739965075, :ident :release.type/single}
         {20512488927800905 #:db{:id 20512488927800905},
          68459991991856131 #:db{:id 68459991991856131}}
         nil]
     nil))

(p/defn EntityDetail [e]
  (assert e)
  (p/client (dom/h1 (dom/text "Entity detail: " e))) ; treeview on the entity
  (binding [explorer/cols [::k ::v] explorer/Format Format-entity]
    (Explorer.
      ;; TODO inject sort
      (explorer/tree-lister (new (p/task->cp (d/pull db {:eid e :selector ['*] :compare compare})))
        (partial entity-tree-entry-children schema)
        any-matches?)
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em auto"})))

(p/defn EntityHistory [e]
  (assert e)
  (p/client (dom/h1 (dom/text "Entity history: " (pr-str e))))
  (binding [explorer/cols [::e ::a ::op ::v ::tx-instant ::tx]
            explorer/Format (p/fn [[e aa v tx op :as row] a]
                              (when row ; when this view unmounts, somehow this fires as nil
                                (case a
                                  ::op (p/client (dom/text (name (case op true :db/add false :db/retract))))
                                  ::e (p/client (router/link [::entity e] (dom/text e)))
                                  ::a (if (some? aa)
                                        (let [ident (:db/ident (new (p/task->cp (d/pull db {:eid aa :selector [:db/ident]}))))]
                                          (p/client (dom/text (pr-str ident)))))
                                  ::v (p/client (some-> v pr-str dom/text))
                                  ::tx (p/client (router/link [::tx tx] (dom/text tx)))
                                  ::tx-instant (let [x (:db/txInstant (new (p/task->cp (d/pull db {:eid tx :selector [:db/txInstant]}))))]
                                                 (p/client (pr-str (dom/text x))))
                                  (str v))))]
    (Explorer.
      ; accumulate what we've seen so far, for pagination. Gets a running count. Bad?
      (explorer/tree-lister (new (->> (dx/entity-history-datoms> db e)
                                   (m/reductions conj []) ; track a running count as well?
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em 10em 3em auto auto 9em"})))

(p/defn AttributeDetail [a]
  (p/client (dom/h1 (dom/text "Attribute detail: " a)))
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e _ v tx op :as x] k]
                              (p/client
                                (case k
                                  :e (router/link [::entity e] (dom/text e))
                                  :a (dom/text (pr-str a)) #_(let [aa (new (p/task->cp (dx/ident! db aa)))] aa)
                                  :v (some-> v pr-str dom/text) ; when a is ref, render link
                                  :tx (router/link [::tx tx] (dom/text tx)))))]
    (Explorer.
      (explorer/tree-lister (new (->> (d/datoms> db {:index :aevt, :components [a]})
                                   (m/reductions conj [])
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(p/defn TxDetail [e]
  (p/client (dom/h1 (dom/text "Tx detail: " e)))
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e aa v tx op :as x] a]
                              (case a
                                :e (let [e (new (p/task->cp (dx/ident! db e)))] (p/client (router/link [::entity e] (dom/text e))))
                                :a (let [aa (new (p/task->cp (dx/ident! db aa)))] (p/client (router/link [::attribute aa] (dom/text aa))))
                                :v (pr-str v) ; when a is ref, render link
                                (str tx)))]
    (Explorer.
      (explorer/tree-lister (new (->> (d/tx-range> conn {:start e, :end (inc e)}) ; global
                                   (m/eduction (map :data) cat)
                                   (m/reductions conj [])
                                   (m/relieve {})))
        (fn [_]) any-matches?)
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(p/defn DbStats []
  (p/client (dom/h1 (dom/text "Db stats")))
  (binding [explorer/cols [::k ::v]
            explorer/Format (p/fn [[k v :as row] col]
                              (p/client
                                (case col
                                  ::k (dom/text (pr-str k))
                                  ::v (cond
                                        (= k :attrs) nil ; print children instead
                                        () (dom/text (pr-str v))))))] ; {:count 123}
    (Explorer.
      (explorer/tree-lister
        (new (p/task->cp (d/db-stats db)))
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

(p/defn Page [[self state [local-page x]]]
  (dom/link (dom/props {:rel :stylesheet, :href "user/datomic-browser.css"}))
  (dom/h1 (dom/text "Datomic browser"))
  (dom/div (dom/props {:class "user-datomic-browser"})
    (dom/div (dom/text "Nav: ")
      (router/link [::summary] (dom/text "home")) (dom/text " ")
      (router/link [::db-stats] (dom/text "db-stats")) (dom/text " ")
      (router/link [::recent-tx] (dom/text "recent-tx")))
    (router/router 1 ; focus explorer state
      (p/server
        (case (or local-page ::summary)
          ::summary (Attributes.)
          ::attribute (AttributeDetail. x)
          ::tx (TxDetail. x)
          ::entity (do (EntityDetail. x) (EntityHistory. x))
          ::db-stats (DbStats.)
          ::recent-tx (RecentTx.)
          (p/client (dom/text "no matching route: " (pr-str local-page))))))))

(p/defn DatomicBrowser []
  (println (pr-str (type 1))) ; show we're on the server
  (p/server ; bug that this is needed; above line shows we're already here
  (binding [conn @(requiring-resolve 'user/datomic-conn)]
    (binding [db (d/db conn)]
      (binding [schema (new (dx/schema> db))]
        (p/client
          (binding [router/build-route (fn [[self state local-route] local-route']
                                         ; root local links through this entrypoint for DI
                                         `[DatomicBrowser ~state ~local-route'])]
            (Page. router/route))))))))
