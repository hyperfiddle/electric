(ns dustin.y2022.forms.circuit-explainer
  (:require #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom] ; no sugar + automatic merge-tx
            [hyperfiddle.photon-ui2 :as ui]) ; looped controls
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.forms.circuit-explainer)))


; db = App(db)
(defn App [db] (d/with db [:db/add 1 :person/name "alice"]))

; db is a log, so :
; log = App(log)
; log is time / global ordering, so :
; t = App(t)

; t = Σ [t=0..T] App(t) Δt
; db = Σ [t=0..T] App(db)Δdb
; t = Σ [t=0..T] App(t)Δt

; db = Σ [t=0..T] App(db)Δdb    -- isolate the contribution during Δdb
; t = Σ [t=0..T] ΔApp(t)

; App :: Stream Db -> Stream Db
; [db, log] = App (db, log)

(def cobbblestone 536561674378709)

(p/defn App [db]
  (let [record (d/pull db [:label/name :db/id] cobbblestone)]
    (dom/dl
      (dom/dt (dom/text "name"))
      (dom/dd (let [v (ui/input (:label/name record))]
                (p/server (d/with db [:db/add cobbblestone :label/name v])))))))

(def db-conn @(requiring-resolve 'test/datomic-conn))
(def db0 (d/with-db db-conn))

(p/run (p/with-cycle [db db0]
         (App. db)))

(p/def Transact!) ; server

;#?(:clj (defn merge-tx [schema & txs] (reduce (partial hf/into-tx schema) txs)))
(defn merge-tx [& txs] (apply concat txs)) ; colorless

(defn remove-noop-txs [db tx] [])
(p/def Transact!)
(p/defn App [db]
  (let [record (d/pull db [:db/id :label/name :label/sortName] cobbblestone)] ; pending
    (p/client
      ;(merge-tx)
      (dom/div (dom/text (name hf/loading) " " (hf/Load-timer.) "ms"))
      (dom/dl
        (dom/dt (dom/text "name"))

        ; when-some ...

        (dom/dd (let [v-old (:label/name record) ; "alice"
                      v (ui/input v-old
                                  (dom/props {:style {:background-color (if local-loading "yellow")}}))]
                  ;(p/Unglitch. v)
                  (Transact!. [[:db/add (:db/id record) :label/name v] ; on first render, emit no-op tx
                               [:db/cas ...]])))

        (p/with-cycle [local-loading false]
          (try
            (dom/dt (dom/text "sortName"))
            (dom/dd (let [v (ui/input (:label/sortName record)
                                      (dom/props {:style {:background-color (if local-loading "yellow")}}))]
                      ;(p/Unglitch. v)
                      (Transact!. [[:db/add (:db/id record) :label/sortName v]])))
            ::hf/idle (catch Pending e ::hf/loading)))
        )
      (dom/pre (dom/text (pprint-str record))))))

(defn with [db tx] (:db-after (d/with db tx)))

(defn ΔApp [db] [:db/add 1 :person/name "alice"])
(p/run
  (p/with-cycle [loading ::hf/idle]
    (binding [hf/loading loading]
      (dom/div (dom/text (name hf/loading) " " (hf/Load-timer.) "ms")) ; page level loading state
      (try
        (p/server
          (let [!db @(requiring-resolve 'test/datomic-conn)]
            (let [!stage (atom []) stage (p/watch !stage)
                  !t (atom (global-datomic-clock !db))
                  db (with (d/with-db (p/watch !t)) stage)] ; rebase
              (binding [Transact! (p/fn [tx]
                                    (p/wrap
                                      (when-some [tx (seq (remove-noop-txs db tx))] ; stabilized
                                        (swap! !t with tx) ; local commit - advance branch time
                                        (swap! !stage concat tx))))]
                (ΔApp db)))))
        ::hf/idle (catch Pending e ::hf/loading)))))


; can we map the semantics of with-cycle to the database?
;   instead of reset! it's compare-and-set ... which is swap!
;   if

; [x] distribution
; [x] latency, pending
; [x] form with one field
; [x] dom semantics - automatic tx merging
; [x] form with two fields, concurrent edits
;    [x] should we fix the no-op stage "noise"? and how?
; [x] add remote database (async)
; [x] multiplayer app, concurrent transactions from different users
; [ ] failure, conflict resolution
; [ ] impulse

; How does `delay` relate to this?
