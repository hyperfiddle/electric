(ns wip.form-impure-ct
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom] ; compatible with current dom
            [hyperfiddle.photon-ui2 :as ui] ; controls loop
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros wip.form-impure-ct)))

(def cobbblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(p/def stage!) ; server

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec e)]
      (p/client
        (dom/dl

          (dom/dt (dom/text "id"))
          (dom/dd (let [v (ui/input (:db/id record) {::dom/disabled true} #_hf/loading)]))

          (dom/dt (dom/text "name"))
          (dom/dd (let [v (ui/input (:label/name record) #_hf/loading)]
                    (p/server (stage! [[:db/add e :label/name v]
                                       #_[:db/cas ...]]))))

          (dom/dt (dom/text "sortName"))
          (dom/dd (let [v (ui/input (:label/sortName record) #_hf/loading)]
                    (p/server (stage! [[:db/add e :label/sortName v]]))))

          (dom/dt (dom/text "gid"))
          (dom/dd (let [v (ui/uuid (:label/gid record) #_hf/loading)]
                    (p/server (stage! [[:db/add e :label/gid v]]))))

          (dom/dt (dom/text "startYear"))
          (dom/dd (let [v (ui/long (:label/startYear record) #_hf/loading)]
                    (p/server (stage! [[:db/add e :label/startYear v]
                                       #_[:db/cas ...]]))))
          )
        (dom/pre (pprint-str record))))))

(p/defn App [e]
  (p/client
    (p/server (LabelForm. e))
    (Popover. "change name" (p/partial 1 LabelForm e)))) ; CT impulse, must feedback busy when handled

(p/defn Stage [stage]
  (stage! (p/client
            (dom/element "style" ".hyperfiddle-stage { display: block; width: 100%; }")
            (ui/edn-editor stage {::dom/disabled true
                                  ::dom/class "hyperfiddle-stage"}))))

(p/defn Page [e]
  (let [!stage (atom []) stage (p/watch !stage)]
    (binding [hf/db (p/wrap (:db-after (hf/with hf/db stage)))
              stage! (fn [tx] (swap! !stage (partial hf/into-tx hf/schema) tx) nil)]
      (App. e)
      (Stage. stage)))) ; (p/Unglitch. stage)

(p/defn Demo []
  (bindx [hf/db (d/with-db @(requiring-resolve 'test/datomic-conn))
          hf/schema (new (dx/schema> hf/db))
          hf/with (fn [db tx] (d/with db {:tx-data tx}))]
    (p/client
      (dom/h1 (dom/text (str `Demo)))
      (p/with-cycle [loading ::hf/loading]
        (binding [hf/loading loading]
          (dom/div (dom/text (name loading) " " (hf/Load-timer.) "ms"))
          (try
            (p/server (Page. cobbblestone))
            ::hf/idle (catch Pending e ::hf/loading))))
      nil)))