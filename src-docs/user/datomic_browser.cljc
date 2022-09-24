(ns user.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [contrib.data :refer [unqualify index-by]]
            [missionary.core :as m]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! %]]
            #?(:clj [user.datomic-contrib :as dx
                     :refer [schema! ident! entity-history-datoms> attributes>]])
            [user.datomic-missionary #?(:clj :as :cljs :as-alias) d]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.datomic-browser)))

(p/def conn)
(p/def db)
(p/def schema) ; schema is available in all explorer renderers
(p/def Navigate!)

(p/defn Nav-link [x label]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e]
                                          (.preventDefault e)
                                          (println "nav-link clicked, route: " x)
                                          (Navigate!. x))} label)))

(p/defn RecentTx []
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Format (p/fn [[e _ v tx op :as record] a]
                              (case a
                                :db/id (Nav-link. [::tx tx] tx)
                                :db/txInstant (pr-str v) #_(p/client (.toLocaleDateString v))))]
    (Explorer.
      "Recent Txs"
      (new (->> (d/datoms> db {:index :aevt, :components [:db/txInstant]})
                (m/reductions conj ())
                (m/latest identity))) ; fixme buffer
      {::explorer/page-size 30
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em auto"})))

(p/defn Attributes []
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (p/fn [row col]
                              (let [v (col row)]
                                (case col
                                  :db/ident (Nav-link. [::attribute v] v)
                                  :db/valueType (some-> v :db/ident name)
                                  :db/cardinality (some-> v :db/ident name)
                                  :db/unique (some-> v :db/ident name)
                                  (str v))))]
    (Explorer.
      "Attributes"
      (->> (attributes> db explorer/cols)
           (m/reductions conj [])
           new
           (sort-by :db/ident)) ; sort by db/ident which isn't available
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

(comment
  (def cobblestone 536561674378709)
  (m/? (d/pull! user/db {:eid cobblestone :selector ['*]})))

(p/defn entity-tree-entry-children [[k v :as row]] ; row is either a map-entry or [0 {:db/id _}]
  ; This shorter expr works as well but is a bit "lucky" with types in that you cannot see
  ; the intermediate cardinality many traversal. Unclear what level of power is needed here
  ;(cond
  ;  (map? v) (into (sorted-map) v)
  ;  (sequential? v) (index-by dx/identify v))

  ; this controlled way dispatches on static schema to clarify the structure
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

    () (assert false (str "unmatched tree entry, k: " k " v: " v))))

(p/defn EntityDetail [e]
  (assert e)
  (binding [explorer/cols [::k ::v]
            explorer/Children entity-tree-entry-children
            explorer/Search? (p/fn [[k v :as row] s] (or (includes-str? k s)
                                                         (includes-str? (if-not (map? v) v) s)))
            explorer/Format (p/fn [[k v :as row] col]
                              (case col
                                ::k (cond
                                      (= :db/id k) k ; :db/id is our schema extension, can't nav to it
                                      (contains? schema k) (Nav-link. [::attribute k] k)
                                      () (str k)) ; str is needed for Long db/id, why?
                                ::v (if-not (coll? v) ; don't render card :many intermediate row
                                      (let [[valueType cardinality]
                                            ((juxt (comp unqualify dx/identify :db/valueType)
                                                   (comp unqualify dx/identify :db/cardinality)) (k schema))]
                                        (cond
                                          (= :db/id k) (Nav-link. [::entity v] v)
                                          (= :ref valueType) (Nav-link. [::entity v] v)
                                          () (pr-str v))))))]
    (Explorer.
      (str "Entity detail: " e) ; treeview on the entity
      (new (p/task->cp (d/pull! db {:eid e :selector ['*] ::d/compare compare}))) ; todo inject sort
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em auto"})))

(p/defn EntityHistory [e]
  (assert e)
  (binding [explorer/cols [::e ::a ::op ::v ::tx-instant ::tx]
            explorer/Format (p/fn [[e aa v tx op :as row] a]
                              (when row ; when this view unmounts, somehow this fires as nil
                                (case a
                                  ::op (name (case op true :db/add false :db/retract))
                                  ::e (Nav-link. [::entity e] e)
                                  ::a (if (some? aa)
                                        (:db/ident (new (p/task->cp (d/pull! db {:eid aa :selector [:db/ident]})))))
                                  ::v (some-> v pr-str)
                                  ::tx (Nav-link. [::tx tx] tx)
                                  ::tx-instant (pr-str (:db/txInstant (new (p/task->cp (d/pull! db {:eid tx :selector [:db/txInstant]})))))
                                  (str v))))]
    (Explorer.
      (str "Entity history: " (pr-str e))
      ; accumulate what we've seen so far, for pagination. Gets a running count. Bad?
      (new (->> (entity-history-datoms> db e)
                (m/reductions conj []) ; track a running count as well?
                (m/latest identity))) ; fixme buffer
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em 10em 3em auto auto 9em"})))

(p/defn AttributeDetail [a]
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e _ v tx op :as x] k]
                              (case k
                                :e (Nav-link. [::entity e] e)
                                :a (pr-str a) #_(let [aa (new (p/task->cp (ident! db aa)))] aa)
                                :v (some-> v pr-str)
                                :tx (Nav-link. [::tx tx] tx)))]
    (Explorer.
      (str "Attribute detail: " a)
      (new (->> (d/datoms> db {:index :aevt, :components [a]})
                (m/reductions conj [])
                (m/latest identity)))
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(p/defn TxDetail [e]
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e aa v tx op :as x] a]
                              (case a
                                :e (let [e (new (p/task->cp (ident! db e)))] (Nav-link. [::entity e] e))
                                :a (let [aa (new (p/task->cp (ident! db aa)))] (Nav-link. [::attribute aa] aa))
                                :v (pr-str v)
                                (str tx)))]
    (Explorer.
      (str "Tx detail: " e)
      (new (->> (d/tx-range> conn {:start e, :end (inc e)}) ; global
                (m/eduction (map :data) cat)
                (m/reductions conj []) ; track a running count as well
                (m/latest identity))) ; fixme buffer
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(p/defn DbStats []
  (binding [explorer/cols [::k ::v]
            explorer/Children (p/fn [[k v :as row]] (if (map? v) (into (sorted-map) v))) ; todo move sort into pull
            explorer/Search? (p/fn [[k v :as row] s] (or (includes-str? k s)
                                                         (includes-str? (if-not (map? v) v) s)))
            explorer/Format (p/fn [[k v :as row] col] (case col ::k k ::v v))]
    (Explorer.
      (str "Db Stats:")
      (new (p/task->cp (d/db-stats db))) ; todo inject sort
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "20em auto"})))

#?(:cljs (def !route (atom [::summary] #_[::entity 87960930235113])))

(p/defn App []
  (binding [conn @(requiring-resolve 'user/datomic-conn)]
    (binding [db (d/db conn)]
      (binding [schema (new (p/task->cp (schema! db)))]
        (p/client
          (binding [Navigate! (p/fn [x]
                                (println "Navigate!. route: " x)
                                (reset! !route x))]
            (dom/link {:rel :stylesheet, :href "user/datomic-browser.css"})
            (dom/h1 "Datomic browser")
            (dom/div {:class "user-datomic-browser"}
              (dom/pre (pr-str (p/watch !route)))
              (dom/div "Nav: "
                (Nav-link. [::summary] "home") " "
                (Nav-link. [::db-stats] "db-stats") " "
                (Nav-link. [::recent-tx] "recent-tx"))
              (p/server
                ; x transfers, don't use a ref in the route
                (let [[page x :as route] (p/client (p/watch !route))]
                  (case page
                    ::summary (do (Attributes.))
                    ::attribute (AttributeDetail. x)
                    ::tx (TxDetail. x)
                    ::entity (do (EntityDetail. x) (EntityHistory. x))
                    ::db-stats (DbStats.)
                    ::recent-tx (RecentTx.)
                    (str "no matching route: " (pr-str route))))))))))))