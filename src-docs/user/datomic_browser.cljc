(ns user.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj datomic.client.api)
            #?(:clj [datomic.client.api.async :as d])
            [missionary.core :as m]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! %]]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.datomic-browser)))

#?(:clj (def datomic-client (datomic.client.api/client {:server-type :dev-local :system "datomic-samples"})))
#?(:clj (def datomic-conn (datomic.client.api/connect datomic-client {:db-name "mbrainz-subset"})))
#?(:clj (def db (d/db datomic-conn)))

#?(:clj (defn pull! [db e selector] (p/chan-read (d/pull db {:eid e :selector selector}))))

(comment (m/? (pull! db 13194139533314 '[*])))

(p/def Navigate!)

(p/defn Nav-link [x label]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e] (.preventDefault e) (Navigate!. x))} label)))

;#?(:clj
;   (defn transactions! [db pull-pattern]
;     (m/sp (->> (m/? (p/chan->task
;                       (d/q {:query '[:find (pull ?tx pattern)
;                                      :in $ pattern
;                                      :where [?tx :db/txInstant]]
;                             :args [db pull-pattern]})))
;                (map first)
;                (sort-by :db/txInstant)
;                reverse))))

;#?(:clj (defn transactions'! [db]))

;#?(:clj (defn transactions>
;          ; tx-range has pagination, as does datoms
;          ([conn]
;           (->> (p/chan->ap (d/tx-range conn {:start nil :end nil}))
;                (m/eduction (map :data))))
;          #_([db pull-pattern])))
;
;#?(:clj (defn transactions< [db]))

(comment
  #_(take 3 (m/? (transactions! db [:db/id :db/txInstant])))
  #_(take 5 (drop 20 (m/? (m/reduce conj [] (transactions> datomic-conn))))))

#?(:clj (defn attr-datoms! [db a] (p/chan->task (d/datoms db {:index :aevt, :components [a]}))))
#?(:clj (defn attr-datoms> [db a] (->> (p/chan->ap (d/datoms db {:index :aevt, :components [a]}))
                                       (m/eduction (mapcat identity)))))
#?(:clj (defn attr-datoms< [db a] (->> (attr-datoms> db a)
                                       (m/reductions conj [])
                                       (m/latest identity)))) ; FIXME BUFFER

(comment
  (m/? (attr-datoms! db :db/ident))
  (take 3 (m/? (m/reduce conj [] (attr-datoms> db :db/ident))))
  (take 3 (m/? (m/reduce conj [] (attr-datoms> db :db/txInstant)))))

(p/defn RecentTransactions []
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Format (p/fn [[e _ v tx op] a]
                              (let [m (new (p/task->cp (pull! db e explorer/cols)))
                                    v (a m)]
                                (case a
                                  :db/id (Nav-link. [::tx v] v)
                                  :db/txInstant (pr-str v) #_(p/client (.toLocaleDateString v))
                                  (pr-str v))))]
    (Explorer.
      "Recent Txs"
      (new (attr-datoms< db :db/txInstant))
      {::explorer/page-size 10
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em auto"})))

;#?(:clj (defn attributes! [db pull-pattern]
;          (m/sp (->> (m/? (p/chan->task
;                            (d/q {:query '[:find (pull ?e pattern)
;                                           :in $ pattern
;                                           :where [?e :db/valueType _]]
;                                  :args [db pull-pattern]})))
;                     (map first)
;                     (sort-by :db/ident)))))
;
;(comment (time (take 3 (m/? (attributes! db [:db/ident])))))

#?(:clj (defn attributes>
          ([db]
           (m/ap (->> (m/?> (p/chan->ap
                              (d/q {:query '[:find ?e :where [?e :db/valueType _]]
                                    :args [db]})))
                      (map first))))
          ([db pull-pattern]
           (m/ap (->> (m/?> (p/chan->ap
                              (d/q {:query '[:find (pull ?e pattern)
                                             :in $ pattern
                                             :where [?e :db/valueType _]]
                                    :args [db pull-pattern]})))
                      (map first)
                      #_(sort-by :db/ident))))))

(comment
  (time (m/? (m/reduce into [] (attributes> db [:db/ident]))))
  (time (m/? (m/reduce into [] (attributes> db)))))

#?(:clj (defn attributes<
          ([db]
           (->> (attributes> db)
                (m/eduction (mapcat identity)) ; ?
                (m/reductions conj [])
                (m/latest identity)))
          ([db pull-pattern]
           (->> (attributes> db pull-pattern)
                (m/eduction (mapcat identity)) ; ?
                (m/reductions conj [])
                (m/latest identity)))))

(comment
  (time (m/? (m/reduce into [] (attributes< db [:db/ident]))))
  (time (m/? (m/reduce into [] (attributes< db)))))

(p/defn Attributes []
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (p/fn [e a _]
                              (let [x (new (p/task->cp (pull! db e explorer/cols)))
                                    v (a x)]
                                (case a
                                  :db/ident (Nav-link. [::attribute v] v)
                                  :db/valueType (some-> v :db/ident name)
                                  :db/cardinality (some-> v :db/ident name)
                                  :db/unique (some-> v :db/ident name)
                                  (str v))))]
    (Explorer.
      "Attributes"
      (new (attributes< db)) ; why bother? d/q has task semantics right?
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

;#?(:clj
;   (defn render-datoms [db datoms]
;     (m/sp (let [ref-attr? (->> (m/? (p/chan->task (d/q {:query '[:find ?e :where [?e :db/valueType :db.type/ref]]
;                                                         :args [db]})))
;                                (map first) set)
;                 datoms-id? (set (concat (map #(nth % 0) datoms)
;                                         (map #(nth % 1) datoms)
;                                         (->> datoms
;                                              (filter #(ref-attr? (nth % 1)))
;                                              (map #(nth % 2)))))
;                 id->ident (into {} (m/? (p/chan->task (d/q {:query '[:find ?e ?v
;                                                                      :in $ ?datoms-id?
;                                                                      :where
;                                                                      [?e :db/ident ?v]
;                                                                      [(contains? ?datoms-id? ?e)]]
;                                                             :args [db datoms-id?]}))))]
;             (->> datoms (map (fn [[e a v t]]
;                                {:e (get id->ident e e)
;                                 :a (get id->ident a a)
;                                 :v (get id->ident v v)
;                                 :tx t})))))))

;#?(:clj (defn ident> [db e]))

#?(:clj (defn ident! [db ?e]
          {:pre [db]}
          (m/sp
            (if ?e
              (let [[[k]] (m/? (p/chan->task (d/q {:query '[:find ?k :in $ ?e :where [?e :db/ident ?k]]
                                                   :args [db ?e]})))]
                (or k ?e))))))

(comment
  (m/? (ident! db 17)) := :db.excise/beforeT
  (m/? (ident! db nil)) := nil)

; ap - query an ident
; cp - reduce them into a map, as a cache
; return an async ident, which maybe loads from query

;#?(:clj
;   (defn render-datom> [db [e a v t]]
;     ; doesn't need to be async - extract the queries
;     (m/ap
;       (let [ref-attr? (->> (m/? (p/chan->task (d/q {:query '[:find ?e :where [?e :db/valueType :db.type/ref]]
;                                                     :args [db]})))
;                            (map first) set)
;             datoms-id? (->> [e a (if (ref-attr? a) v)] (remove nil?) set)
;             id->ident (into {} (m/? (p/chan->task (d/q {:query '[:find ?e ?v
;                                                                  :in $ ?datoms-id?
;                                                                  :where
;                                                                  [?e :db/ident ?v]
;                                                                  [(contains? ?datoms-id? ?e)]]
;                                                         :args [db datoms-id?]}))))]
;         {:e (get id->ident e e)
;          :a (get id->ident a a)
;          :v (get id->ident v v)
;          :tx t}))))
;
;(comment
;  (def it ((m/ap (m/?> (render-datom> db (m/?> (tx-datoms> datomic-conn 13194139534022)))))
;           #(println ::notify) #(println ::terminate)))
;  @it
;  (it))

#?(:clj (defn tx-datoms> [conn txid]
          (->> (p/chan->ap (d/tx-range conn {:start txid, :end (inc txid)}))
               (m/eduction (comp (map :data) (mapcat identity))))))
(comment (take 3 (m/? (m/reduce conj [] (tx-datoms> datomic-conn 13194139534022)))))

#?(:clj
   (defn tx-datoms [conn txid]
     ; https://docs.datomic.com/client-api/datomic.client.api.async.html#var-tx-range
     (->> (tx-datoms> conn txid)
          (m/eduction (take 1)) ; terminate flow after 1 tx (? why)
          (m/eduction (map :data))
          (m/reduce into []))))

(comment
  (take 3 (m/? (tx-datoms datomic-conn 13194139534022)))
  (time (take 3 (m/? (m/sp (m/? (render-datoms db (m/? (tx-datoms datomic-conn 13194139534022))))))))

  (time (take 3 (m/? (tx-datoms datomic-conn 13194139534018))))
  (time (take 3 (m/? (m/sp (m/? (render-datoms db (m/? (tx-datoms datomic-conn 13194139534018)))))))))

#?(:clj (defn entity-datoms! [db e] (p/chan->task (d/datoms db {:index :eavt, :components [e]}))))
#?(:clj (defn entity-datoms> [db e] (->> (p/chan->ap (d/datoms db {:index :eavt, :components [e]}))
                                         (m/eduction (mapcat identity)))))
#?(:clj (defn entity-datoms< [db a] (->> (entity-datoms> db a)
                                         (m/reductions conj [])
                                         (m/latest identity)))) ; FIXME BUFFER

(comment
  (m/? (entity-datoms! db 1))
  (m/? (entity-datoms! db :db/ident))
  (take 3 (m/? (m/reduce conj [] (entity-datoms> db :db/ident))))
  #_(m/? (m/sp (m/? (render-datoms db (m/? (entity-datoms! db :db/ident)))))))

(p/defn EntityDetail [e]
  (assert e)
  (binding [explorer/cols [:a :v :tx]
            explorer/Format (p/fn [[e aa v tx op] a]
                              (case a
                                :a (let [aa (new (p/task->cp (ident! db aa)))]
                                     (Nav-link. [::attribute aa] aa))
                                :v (some-> v pr-str)
                                :tx (Nav-link. [::tx tx] tx)))]
    (Explorer.
      (str "Entity detail: " e)
      (new (entity-datoms< db e)) ; walk the entity graph rather
      {::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em calc(100% - 15em - 9em) 9em"})))

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
                  >fwd-xs (p/chan->ap (d/datoms history {:index :eavt :components [?e ?a]}))
                  >rev-xs (p/chan->ap (d/datoms history {:index :vaet :components [?e ?a]}))]
              ; returns chunks, not individual datoms
              (m/amb= (m/?> >fwd-xs) (m/?> >rev-xs))))
          (m/eduction (mapcat identity)))))

(comment
  (time (m/? (m/reduce conj [] (entity-history-datoms> db 74766790739005 nil))))
  (time (count (m/? (m/reduce conj [] (entity-history-datoms> db nil nil)))))
  (def it ((entity-history-datoms> db 74766790739005 nil)
           #(println ::notify) #(println ::terminate)))
  @it
  (it))

#?(:clj
   (defn entity-history-datoms< [db ?e ?a]
     ; accumulate what we've seen so far, for pagination. Gets a running count. Bad?
     (->> (entity-history-datoms> db ?e ?a)
          (m/reductions conj []) ; track a running count as well
          (m/latest identity)))) ; fixme buffer

(p/defn EntityHistory [e]
  (assert e)
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e aa v tx op] a]
                              (case a
                                :e (Nav-link. [::entity e] e)
                                :a (some-> aa pr-str)
                                :v (some-> v pr-str)
                                :tx (Nav-link. [::tx tx] tx)
                                (str v)))]
    (Explorer.
      (str "Entity history: " (pr-str e))
      (new (entity-history-datoms< db e nil))
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(comment
  ;(attr-datoms< db :label/name)
  )

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
      (new (attr-datoms< db a))
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

#?(:clj (defn tx-datoms< [!conn db]
          (->> (tx-datoms> !conn db)
               (m/reductions conj []) ; track a running count as well
               (m/latest identity)))) ; fixme don't buffer

(p/defn TxDetail [e]
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [[e aa v tx op :as x] a]
                              (case a
                                :e (let [e (new (p/task->cp (ident! db e)))] (Nav-link. [::entity e] e))
                                :a (let [aa (new (p/task->cp (ident! db aa)))] (Nav-link. [::attribute aa] aa))
                                :v (pr-str v)
                                ; these are mostly not refs, ints are simple values
                                #_(if v (let [v' (new (p/task->cp (ident! db v)))]
                                           (str (pr-str v)
                                                (if (not= v v') (str " (" (pr-str v') ")")))))
                                (str tx)))]
    (Explorer.
      (str "Tx detail: " e)
      (new (tx-datoms< datomic-conn e))               ; global
      {::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

#?(:cljs (def !route (atom [::summary])))

(p/defn App []
  (p/client
    (binding [Navigate! (p/fn [x]
                          (println "Navigate!. route: " x)
                          (reset! !route x))]
      (dom/link {:rel :stylesheet, :href "user/datomic-browser.css"})
      (dom/h1 "Datomic browser")
      (dom/div {:class "user-datomic-browser"}
        (dom/pre (pr-str (p/watch !route)))
        (dom/div "Nav: "
          (Nav-link. [::summary] "home"))
        (p/server
          ; x transfers, don't use a ref
          (let [[page x :as route] (p/client (p/watch !route))]
            (case page
              ::summary (do (RecentTransactions.) (Attributes.))
              ::attribute (AttributeDetail. x)
              ::tx (TxDetail. x)
              ::entity (do #_(EntityDetail. x) (EntityHistory. x))
              (str "no matching route: " (pr-str route)))))))))