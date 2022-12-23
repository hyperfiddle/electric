(ns user.demo-stage2
  "ui3/input - don't spam datomic"
  #?(:cljs (:require-macros user.demo-stage2))
  (:require [contrib.css :refer [css-slugify]]
            [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui3 :as ui3]
            hyperfiddle.popover2
            [hyperfiddle.popover :refer [Popover]]
            #?(:clj [hyperfiddle.txn :refer [minimal-tx]]))
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled]))

(def cobblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(comment (d/pull test/datomic-db ['*] cobblestone))

(p/defn Form [e]
  (let [record (d/pull hf/db label-form-spec e)]
    (p/client
      ;(p/Unglitch. record)
      (dom/dl

        (dom/dt (dom/text "id"))
        (dom/dd (ui3/input! (:db/id record) nil (dom/props {::dom/disabled true})))

        (dom/dt "gid")
        (dom/dd (ui3/uuid (:label/gid record) false (dom/props {::dom/disabled true})))

        ; right
        (dom/dt (dom/text "name"))
        (dom/dd (ui3/input! (:label/name record)
                            (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/name v]])))))

        ; wrong, query is caught as pending
        (dom/dt (dom/text "name"))
        (dom/dd (when (p/with-cycle [syncing nil]
                        (try
                          (ui3/input! (:label/name record)
                                      (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/name v]])))
                                      (dom/props {:style {:background-color (if syncing "yellow")}}))
                          (catch Pending _ true)))
                  (throw (Pending.))))

        ; wrong, query is caught as pending
        (dom/dt (dom/text "name"))
        (dom/dd (p/with-cycle2 [syncing nil]
                  (ui3/input! (:label/name record)
                              (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/name v]])))
                              (dom/props {:style {:background-color (if (= syncing ::p/pending) "yellow")}}))))


        ; right
        (dom/dt (dom/text "sortName"))
        (dom/dd (when (p/with-cycle [syncing false]
                        (if-some [v (ui3/input (:label/sortName record) syncing
                                               (dom/props {:style {:background-color (if syncing "yellow")}}))]
                          (try
                            (p/server (hf/Transact!. [[:db/add e :label/sortName v]]))
                            false (catch Pending _ true))
                          syncing))
                  (throw (Pending.))))

        ;(dom/dt (dom/text "startYear"))
        ;(dom/dd (when (p/with-cycle [syncing false]
        ;                (if-some [v (ui3/long (:label/startYear record) syncing
        ;                                       (dom/props {:style {:background-color (if syncing "yellow")}}))]
        ;                  (try
        ;                    (p/server (hf/Transact!. [[:db/add e :label/startYear v]]))
        ;                    false (catch Pending _ true))
        ;                  syncing))
        ;          (throw (Pending.))))
        )

      (dom/pre (dom/text (pprint-str record))))))

(p/defn Page []
  #_(p/client (dom/div (if hf/loading "loading" "idle") " " (str (hf/Load-timer.)) "ms"))
  (Form. cobblestone)
  (Form. cobblestone))

(p/defn Demo []
  (p/client (dom/h1 (dom/text (str `Demo))))
  (p/server
    (let [conn @(requiring-resolve 'test/datomic-conn)
          secure-db (d/with-db conn)] ; todo datomic-tx-listener
      (binding [hf/schema (new (dx/schema> secure-db))
                hf/with (fn [db tx] ; inject datomic
                          (try (:db-after (d/with db {:tx-data tx}))
                               (catch Exception e (println "...failure, e: " e))))
                hf/db secure-db]
        (hf/branch
          (Page.)
          (p/client
            (dom/hr)
            (dom/element "style" (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }"))
            (ui3/edn-editor (p/server hf/stage) false (dom/props {::dom/disabled true ::dom/class (css-slugify `stage)}))))))))
