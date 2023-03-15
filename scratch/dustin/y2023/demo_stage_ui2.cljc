(ns user.demo-stage-ui2
  #?(:cljs (:require-macros user.demo-stage-ui2))
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.css :refer [css-slugify]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [clojure.tools.logging :as log]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            hyperfiddle.popover-ui2b
            [hyperfiddle.popover-ui2 :refer [Popover]]
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
      (p/Unglitch. record)
      (dom/dl

        (dom/dt (dom/text "id"))
        (dom/dd (let [_ (ui/input (:db/id record) {::dom/disabled true})]))

        (dom/dt "gid")
        (dom/dd (let [_ (ui/uuid (:label/gid record) {::dom/disabled true})]))

        (when (p/with-cycle [local-busy false]
                (try
                  (dom/dt (dom/text "name"))
                  (dom/dd (let [v (ui/input (:label/name record)
                                            (dom/props {:style {:background-color (if local-busy "yellow")}}))]
                            (p/Unglitch. v)
                            (p/server (hf/Transact!. [[:db/add e :label/name v]]))))
                  false (catch Pending _ true)))
          (throw (Pending.)))

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

(p/defn Page []
  (p/client (dom/div (name hf/loading) " " (str (hf/Load-timer.)) "ms"))
  (Form. cobblestone)
  (when-let [request (p/client (hyperfiddle.popover-ui2b/Popover.
                                 "open" (p/fn [] (p/server (Form. cobbblestone)))))]
    (println 'Page-request-from-popover request)))

(p/defn Demo []
  (p/client (dom/h1 (dom/text (str `Demo))))
  (p/server
    (let [conn @(requiring-resolve 'test/datomic-conn)
          secure-db (d/with-db conn)] ; todo datomic-tx-listener
      (binding [hf/schema (new (dx/schema> secure-db))
                hf/with (fn [db tx] ; inject datomic
                          (try (:db-after (d/with db {:tx-data tx}))
                               (catch Exception e (println "...failure, e: " e))))
                ;hf/Transact! (p/fn [tx] #_(:db/after (d/transact conn {:tx-data tx})))
                hf/db secure-db]
        (hf/branch
          (Page.)
          (p/client
            (dom/hr)
            (dom/element "style" (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }"))
            (ui/edn-editor (p/server hf/stage) {::dom/disabled true ::dom/class (css-slugify `stage)})))))))