(ns contrib.datomic-cloud-contrib
  (:require [contrib.data :refer [index-by unqualify]]
            contrib.datomic-common-contrib
            [contrib.datomic-cloud-m :as d]
            [hyperfiddle.rcf :refer [tests % tap]]
            [missionary.core :as m]))

(defn tempid? [x] (or (string? x) (neg? x)))

(tests
  (tempid? -1) := true
  (tempid? 1) := false
  (tempid? 0) := false
  (tempid? "a") := true)

(def identify contrib.datomic-common-contrib/identify)
(def identify contrib.datomic-common-contrib/identities)
(def reverse-attr contrib.datomic-common-contrib/reverse-attr)
(def entity-tree-entry-children contrib.datomic-common-contrib/entity-tree-entry-children)

(defn attributes>
  ([db] (attributes> db [:db/ident]))
  ([db pull-pattern]
   (->> (d/qseq {:query '[:find (pull ?e pattern)
                          :in $ pattern
                          :where [?e :db/valueType _]]
                 :args [db pull-pattern]})
        (m/eduction (map first)))))

(comment
  (time (m/? (m/reduce conj [] (attributes> user/db [:db/ident]))))
  (time (m/? (m/reduce conj [] (attributes> user/db))))
  (m/? (d/pull! user/db {:eid 50 :selector [:db/ident :db/valueType :db/cardinality]})))

;#?(:clj (defn schema< [db]
;          (->> (attributes> db [:db/ident :db/valueType :db/cardinality])
;               (m/reductions conj [])
;               (m/relieve {})
;               (m/latest (partial index-by :db/ident)))))

(defn schema! [db] ; todo stream
  (m/sp
    (let [>as (attributes> db [:db/ident
                               {:db/valueType [:db/ident]}
                               {:db/cardinality [:db/ident]}])]
      (->> (m/? (m/reduce conj [] >as))

           ; :db/id is a renderable attr in the semantic UI, schema metadata describes how
           (cons {:db/ident :db/id
                  :db/cardinality {:db/ident :db.cardinality/one}
                  :db/valueType {:db/ident :db.type/long}
                  #_#_:db/unique :db.unique/identity})

           ; todo - streaming group-by should be faster – shouldn't need to wait for all attrs to load
           ; todo - only load the attrs that the renderers actually needs
           (index-by :db/ident)))))

(comment
  (:db/ident (m/? (schema! user/db)))
  (:db/id (m/? (schema! user/db)))
  (m/? (schema! user/db)))

;#?(:clj (defn before? [^java.util.Date a ^java.util.Date b] (<= (.getTime a) (.getTime b))))
;(defn- xf-filter-before-date [before] #?(:clj (filter (fn [[tx e a v added?]] (before? t before)))))

(defn sort-datoms-by-time
  [[tx  e  a  v  added]
   [tx' e' a' v' added']]
  ; tx are monotically increasing right?
  ; Draw :add as more recent than :retract for the same attribute
  (compare [tx' a added']
           [tx a' added]))

(defn entity-history-datoms>
  ([db e] (entity-history-datoms> db e nil))
  ([db ?e ?a]
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

(defn ident! [db ?e]
  {:pre [db]}
  ; future work - cache idents?
  (m/sp
    (if ?e
      (let [[[k]] (m/? (d/q! {:query '[:find ?k :in $ ?e :where [?e :db/ident ?k]]
                              :args [db ?e]}))]
        (or k ?e)))))

(comment
  (m/? (ident! user/db 17)) := :db.excise/beforeT
  (m/? (ident! user/db nil)) := nil)

;(extend-protocol Datafiable
;  datomic.query.EntityMap
;  (datafy [o] (into {} o)))
;
;(extend-protocol Navigable
;  datomic.query.EntityMap
;  (nav [coll k v]
;    (clojure.datafy/datafy v)))

;(defn transactions> [conn]
;  (->> (p/chan->ap (d/tx-range conn {:start nil :end nil}))
;       (m/eduction (map :data))))

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

; #?(:clj (defn attributes<
;          ([db]
;           (->> (attributes> db)
;                (m/eduction cat) ; ?
;                (m/reductions conj [])
;                (m/latest identity)))
;          ([db pull-pattern]
;           (->> (attributes> db pull-pattern)
;                (m/eduction cat) ; ?
;                (m/reductions conj [])
;                (m/latest identity)))))
; (comment
;  (time (m/? (m/reduce into [] (attributes> db [:db/ident]))))
;  (time (m/? (m/reduce into [] (attributes> db)))))

;#?(:clj (defn entity-datoms! [db e] (p/chan->task (d/datoms db {:index :eavt, :components [e]}))))
;#?(:clj (defn entity-datoms> [db e] (->> (p/chan->ap (d/datoms db {:index :eavt, :components [e]}))
;                                         (m/eduction (mapcat identity)))))
;#?(:clj (defn entity-datoms< [db a] (->> (entity-datoms> db a)
;                                         (m/reductions conj [])
;                                         (m/latest identity)))) ; FIXME BUFFER
;
;(comment
;  (m/? (entity-datoms! db 1))
;  (m/? (entity-datoms! db :db/ident))
;  (take 3 (m/? (m/reduce conj [] (entity-datoms> db :db/ident)))))