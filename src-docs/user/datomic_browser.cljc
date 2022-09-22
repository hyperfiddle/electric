(ns user.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [user.datomic-missionary #?(:clj :as :cljs :as-alias) d]
            [missionary.core :as m]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! %]]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.datomic-browser)))

(p/def conn)
(p/def db)

(comment
  (def cobblestone 536561674378709)
  "pulls are sorted at top layer"
  (take 3 (keys (m/? (d/pull! user/db {:eid cobblestone :selector '[*]}))))
  := [:db/id :label/country :label/gid] ; sorted!

  "pulls are sorted at intermedate layers"
  todo)

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

#?(:clj (defn attributes>
          ([db]
           (m/ap (->> (m/?> (d/qseq {:query '[:find ?e :where [?e :db/valueType _]]
                                   :args [db]}))
                      (map first))))
          ([db pull-pattern]
           (m/ap (->> (m/?> (d/qseq {:query '[:find (pull ?e pattern)
                                            :in $ pattern
                                            :where [?e :db/valueType _]]
                                   :args [db pull-pattern]}))
                      (map first))))))

(comment
  (time (m/? (m/reduce into [] (attributes> user/db [:db/ident]))))
  (time (m/? (m/reduce into [] (attributes> user/db))))
  (m/? (d/pull! user/db {:eid 50 :selector [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent]})))

(p/defn Attributes []
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (p/fn [a col]
                              (let [x (new (p/task->cp (d/pull! db {:eid a :selector explorer/cols})))
                                    v (col x)]
                                (case col
                                  :db/ident (Nav-link. [::attribute v] v)
                                  :db/valueType (some-> v :db/ident name)
                                  :db/cardinality (some-> v :db/ident name)
                                  :db/unique (some-> v :db/ident name)
                                  (str v))))]
    (Explorer.
      "Attributes"
      (new (->> (attributes> db) ; todo this query has task semantics, can it be optimized?
                (m/eduction cat)
                (m/reductions conj [])
                (m/latest identity)))
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

(comment
  (def cobblestone 536561674378709)
  (m/? (d/pull! user/db {:eid cobblestone :selector ['*]})))

(p/defn EntityDetail [e]
  (assert e)
  (binding [explorer/cols [::a ::v]
            explorer/Children (p/fn [[a v :as row]] (if (map? v) (into (sorted-map) v))) ; todo move sort into pull
            explorer/Search? (p/fn [[a v :as row] s] (or (includes-str? a s)
                                                         (includes-str? (if-not (map? v) v) s)))
            explorer/Format (p/fn [[a v :as row] col]
                              (case col
                                ::a (Nav-link. [::attribute a] a)
                                ::v (if (some? v) (if-not (map? v) (pr-str v)))))]
    (Explorer.
      (str "Entity detail: " e) ; treeview on the entity
      (new (p/task->cp (d/pull! db {:eid e :selector ['*] ::d/compare compare}))) ; todo inject sort
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em auto"})))

;#?(:clj (defn before? [^java.util.Date a ^java.util.Date b] (<= (.getTime a) (.getTime b))))
;(defn- xf-filter-before-date [before] #?(:clj (filter (fn [[tx e a v added?]] (before? t before)))))

(defn sort-datoms-by-time
  [[tx  e  a  v  added]
   [tx' e' a' v' added']]
  ; tx are monotically increasing right?
  ; Draw :add as more recent than :retract for the same attribute
  (compare [tx' a added']
           [tx a' added]))

#?(:clj
   (defn entity-history-datoms> [db ?e ?a]
     (->> (m/ap
            (let [history (d/history db)
                  ; (sequence #_(comp (xf-filter-before-date before)))
                  >fwd-xs (d/datoms> history {:index :eavt :components [?e ?a]})
                  >rev-xs (d/datoms> history {:index :vaet :components [?e ?a]})]
              (m/amb= (m/?> >fwd-xs) (m/?> >rev-xs)))))))

(comment
  (time (m/? (m/reduce conj [] (entity-history-datoms> user/db 74766790739005 nil))))
  (time (count (m/? (m/reduce conj [] (entity-history-datoms> user/db nil nil)))))
  (def it ((entity-history-datoms> user/db 74766790739005 nil)
           #(println ::notify) #(println ::terminate)))
  @it
  (it))

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
      (new (->> (entity-history-datoms> db e nil)
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

#?(:clj (defn ident! [db ?e]
          {:pre [db]}
          ; future work - cache idents?
          (m/sp
            (if ?e
              (let [[[k]] (m/? (d/q! {:query '[:find ?k :in $ ?e :where [?e :db/ident ?k]]
                                       :args [db ?e]}))]
                (or k ?e))))))

(comment
  (m/? (ident! user/db 17)) := :db.excise/beforeT
  (m/? (ident! user/db nil)) := nil)

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

#?(:cljs (def !route (atom [::summary] #_[::entity 536561674378709])))

(p/defn App []
  (binding [conn @(requiring-resolve 'user/datomic-conn)]
    (binding [db (d/db conn)]
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
                  (str "no matching route: " (pr-str route)))))))))))