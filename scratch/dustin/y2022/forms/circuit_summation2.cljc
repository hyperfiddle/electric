(ns dustin.y2022.forms.circuit-summation2
  (:require [contrib.clojurex :refer [bindx]]
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            #?(:clj [hyperfiddle.txn :refer [minimal-tx]]))
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.forms.circuit-summation2)))

; The problem is distributed state loops
; it's the network. the network is discrete

; db = App(db)
; f = fix f
; (iterate App db)


; db = Σ [db=0..T] App(db)Δdb
; db = Σ [s=0..T] App(db)Δdb
; note that db is the clock, so we can substitute:

;
; db(t) = Σ [t=0..T] App(t)Δt
; where App(t)Δt = ΔApp(t),
; such that ΔApp(db) is a function of database that returns a small transactional change to the database
; db = Σ [s=0..T] ΔApp(db)
; db = Σ [s=0..T] App(db)Δdb


; Integrate db0,,dbT App(db)

(p/def Transact!)

(def cobbblestone 0)
(def label-form-spec [:db/id :label/gid :label/name :label/sortName :label/startYear])

(p/defn App [e]
  (let [record (d/pull hf/db label-form-spec)]
    (dom/dl
      (dom/dt (dom/text "name"))
      (dom/dd (let [v (ui/input (:label/name record) #_hf/loading)]
                (p/server (Transact! [[:db/add e :label/name v]])))) ; throws pending, even if no-op
      ; should we silence the pending?
      ; first the field throws pending on load
      ; then the db short circuits
      ; then we edit, throw pending, new db, queries throw pending, short circuit here

      (dom/dt (dom/text "sortName"))
      (dom/dd (let [v (ui/input (:label/sortName record) #_hf/loading)]
                (p/server (Transact! [[:db/add e :label/sortName v]])))))))

;(defmacro summation [rf [name x0] & body]
;  `(let [!t# (atom ~x0) ~name (p/watch !t#)]
;     (binding [Transact! (p/fn [Δt]
;                           (p/wrap (async-swap! !t# ~rf Δt)))] ; advance time, busy now true
;       (do ~@body))))

(p/defn Demo []
  (bindx [hf/db (d/with-db @(requiring-resolve 'test/datomic-conn))]
    (p/client
      (dom/h1 (dom/text (str `Demo)))
      (p/with-cycle [loading ::hf/loading]
        (binding [hf/loading loading]
          (dom/div (dom/text (name loading) " " (hf/Load-timer.) "ms"))
          (try
            (p/server
              ; don't lose the stage on rebase!
              (let [!t (atom hf/db)]
                (binding [hf/db (p/watch !t)
                          hf/with (fn [db tx] ; blocks, call with p/wrap. Or use async variant
                                    (when-some [Δt (seq (minimal-tx db Δt))] ; stabilize initial loop, requires query
                                      (:db-after (d/with db {:tx-data tx}))))
                          Transact! (p/fn [Δt]
                                      ; when finished, local busy state false, page busy state true
                                      ; call site can let the Pending exception through if the difference isn't meaningful
                                      (p/wrap (async-swap! !t (partial hf/with hf/db) Δt)))] ; advance time
                  (App. cobbblestone))))
            ::hf/idle (catch Pending e ::hf/loading))))
      nil)))

; What about optimistic updates? Don't wait for global order
; controls emit separate txs, the popover must intercept to make them atomic
; implement the tx listener