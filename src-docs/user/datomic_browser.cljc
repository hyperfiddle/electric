(ns user.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [contrib.data :refer [unqualify index-by]]
            [contrib.datomic-contrib :as dx
             #?@(:clj (:refer [schema! ident! entity-history-datoms> attributes>]))]
            [contrib.datomic-missionary #?(:clj :as :cljs :as-alias) d]
            [missionary.core :as m]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! %]]
            #?(:cljs [hyperfiddle.router :as router])
            [contrib.ednish :as ednish]
            [user.util :refer [includes-str? pprint-str]]
            [clojure.edn :as edn])
  #?(:cljs (:require-macros user.datomic-browser))
  #?(:cljs (:import [goog.math Long]))) ; only this require syntax passes shadow in this file, why?

(p/def conn)
(p/def db)
(p/def schema) ; schema is available in all explorer renderers
(p/def !path (p/client (m/mbx)))

#?(:cljs (def read-edn-str (partial clojure.edn/read-string {:readers {'goog.math/Long goog.math.Long/fromString}})))

#?(:cljs (defn decode-path [path] {:pre [(string? path) (some? read-edn-str)]}
           (if-not (= path "/")
             (-> path (subs 1) ednish/decode read-edn-str)
             [::summary])))

#?(:cljs (defn encode-path [route] (->> route pr-str ednish/encode (str "/"))))

(p/defn Nav-link [route label]
  (p/client
    (let [path (encode-path route)]
      (ui/element dom/a {::dom/href path ; middle click
                         ::ui/click-event (p/fn [e]
                                            (.preventDefault e)
                                            (println "nav-link clicked, route: " route)
                                            (router/pushState! !path path))} label))))

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

(p/defn EntityDetail [e]
  (assert e)
  (binding [explorer/cols [::k ::v]
            explorer/Children (p/fn [m] (dx/entity-tree-entry-children schema m))
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
                (m/reductions conj [])))
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
                (m/reductions conj []))) ; track a running count as well; fixme buffer
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

(p/defn App []
  (binding [conn @(requiring-resolve 'user/datomic-conn)]
    (binding [db (d/db conn)]
      (binding [schema (new (p/task->cp (schema! db)))]
        (p/client
          (let [[page x :as route] (decode-path (new (router/path> !path)))]
            (dom/link {:rel :stylesheet, :href "user/datomic-browser.css"})
            (dom/h1 "Datomic browser")
            (dom/div {:class "user-datomic-browser"}
              (dom/pre (pr-str route))
              (dom/div "Nav: "
                (Nav-link. [::summary] "home") " "
                (Nav-link. [::db-stats] "db-stats") " "
                (Nav-link. [::recent-tx] "recent-tx"))
              (p/server
                (case page
                  ::summary (Attributes.)
                  ::attribute (AttributeDetail. x)
                  ::tx (TxDetail. x)
                  ::entity (do (EntityDetail. x) (EntityHistory. x))
                  ::db-stats (DbStats.)
                  ::recent-tx (RecentTx.)
                  (str "no matching route: " (pr-str page)))))))))))
