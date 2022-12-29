(ns user.demo-stage-ui2
  #?(:cljs (:require-macros user.demo-stage-ui2))
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]]
            #?(:clj [hyperfiddle.txn :refer [minimal-tx]]))
  (:import [hyperfiddle.photon Pending]))

; Requirements:
; top level page queries database
; button: rename district
; button triggers popover
;   form in popover depends on dbval (query)
;   ui/input is pure FRP
;   flow of change used to build tx, or emit eav. need both
;   form that emits tx once valid to local stage
;   commit and discard
; reflow parent queries
; entrypoint bindings visible

(def cobblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(comment
  (d/pull test/datomic-db ['*] cobblestone)
  (d/pull test/datomic-db label-form-spec cobblestone))

(p/defn ValueLog [v]
  (let [!log (atom ()), log (p/watch !log)]
    (swap! !log conj v)
    (dom/div (dom/text (str (take 5 log))))
    v))

;(defn valid-label? [txn]) -- what is the type? when does validation happen? after hydrate

(p/defn Form [e]
  (let [record (d/pull hf/db label-form-spec e)]
    (p/client
      ;(p/Unglitch. record)
      (dom/div (dom/text (name hf/loading) " " (hf/Load-timer.) "ms"))
      (dom/dl

        (dom/dt (dom/text "id"))
        (dom/dd (let [_ (ui/input (:db/id record) (dom/props {::dom/disabled true}))]))

        (dom/dt "gid")
        (dom/dd (let [_ (ui/uuid (:label/gid record) {::dom/disabled true})]))

        (dom/dt (dom/text "name"))
        (dom/dd (let [v (ValueLog. (ui/input (:label/name record)))]
                  (p/Unglitch. v)
                  (p/server (hf/Transact!. [[:db/add e :label/name v]]))))

        (dom/dt (dom/text "sortName"))
        (dom/dd (let [v (ValueLog. (ui/input (:label/sortName record)))]
                  (p/Unglitch. v)
                  (p/server (hf/Transact!. [[:db/add e :label/sortName v]]))))

        (dom/dt "startYear")
        (dom/dd (let [v (ValueLog. (ui/long (:label/startYear record)))]
                  (p/Unglitch. v)
                  (p/server (hf/Transact!. [[:db/add e :label/startYear v]]))))

        ;{:label/type [:db/ident]}

        ;(dom/dt "select test")
        ;(dom/dd (let [v (ui/select [{:text "" :value nil}
        ;                            {:text "a" :value :a}
        ;                            {:text "b" :value :b}
        ;                            {:text "c" :value :c}]
        ;                           :b)]
        ;          v))

        ;(dom/dt "country") -- bug - it's writing US to FR on initial render
        ;(dom/dd (let [v (ui/select [{:text "" :value nil}
        ;                            {:text "US" :value :country/US}
        ;                            {:text "FR" :value :country/FR}]
        ;                           (:db/ident (:label/country record)))]
        ;          (p/server (swap!-nil !txn into-tx [[:db/add e :label/country v]]))))

        )
      (dom/pre (dom/text (pprint-str record))))))

(p/defn App [e]
  (Form. e)
  #_(Form. e))

#?(:clj (defn with! [db tx]
          (if-some [tx (seq (minimal-tx db tx))] ; stabilize first loop (optional)
            ;(hf/into-tx hf/schema tx tx')
            (:db-after (d/with db {:tx-data tx}))
            db)))

(p/defn Demo []
  (p/client
    (dom/h1 (dom/text (str `Demo)))
    (p/with-cycle [loading ::hf/idle]
      (binding [hf/loading loading]
        (try
          (p/server
            (let [!db @(requiring-resolve 'test/datomic-conn) ; todo datomic-tx-listener. don't lose stage on rebase!
                  !t (atom (d/with-db !db))]
              (bindx [hf/db (p/watch !t)
                      hf/schema (new (dx/schema> hf/db))
                      hf/Transact! (p/fn [tx] (p/wrap (swap! !t with! tx)) nil)]
                (App. cobblestone))))
          ::hf/idle (catch Pending e ::hf/loading))))
    nil))

;(p/client (ui/edn-editor stage {::dom/disabled true}))