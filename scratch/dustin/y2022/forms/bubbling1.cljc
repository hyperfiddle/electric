(ns bubbling
  (:require [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            [contrib.clojurex :refer [bindx]]
            #?(:clj [contrib.datomic-contrib :as dx])
            [contrib.str :refer [pprint-str]]
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  #?(:cljs (:require-macros bubbling)))


(def cobbblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(comment
  (d/pull test/datomic-db ['*] cobbblestone)
  (d/pull test/datomic-db label-form-spec cobbblestone))

(p/def into-tx) ; merge-command. server

(p/def !stage) ; server

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec cobbblestone)]
      (p/client
        (dom/mappend
          (dom/h1 "Label editor for: " (p/server (:label/name record)))
          (dom/dl
            (println
              (dom/mappend
                (dom/dt "id")
                #_(dom/dd (input2 record :db/id {::dom/disabled true})) ; Impure CT
                (dom/dd {} [::ui/value (ui/input (:db/id record) {::dom/disabled true})]))) ; Pure CT
            (dom/dt "name")
            (dom/dd {} [::ui/value (ui/input (:label/name record) {})]))
          (dom/pre (pprint-str record)))))))

(p/defn Page [e]
  (p/server (LabelForm. e))
  #_(p/client (Popover. "change name" (p/partial 1 LabelForm e)))) ; todo fork stage differently

(p/defn Demo []
  (let [secure-db (d/with-db @(requiring-resolve 'test/datomic-conn))]
    (bindx [hf/schema (new (dx/schema> secure-db))
            into-tx (partial hf/into-tx hf/schema)
            hf/with (fn [db tx] (d/with db {:tx-data tx}))] ; todo required by popover TODO
      (p/client
        (p/with-cycle [loading ::hf/loading]
          (binding [hf/loading loading]
            (dom/div (name loading) " " (str (hf/Load-timer.)) "ms")
            (try
              (p/server
                (bindx [!stage (atom [])]
                  (let [stage (p/watch !stage)]
                    (bindx [hf/db (:db-after (hf/with secure-db stage))] ; task can fail
                      (let [tx (Page. cobbblestone)
                            stage' (hf/into-tx hf/schema (p/Unglitch. stage)
                                               (p/client (ui/edn-editor stage {::dom/disabled true})))]
                        (println 'tx tx)
                        (hf/into-tx hf/schema stage' tx))))))
              ::hf/idle
              (catch Pending e ::hf/loading))))
        nil))))