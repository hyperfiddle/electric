(ns wip.form-pure-ct
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            #?(:clj [hyperfiddle.txn :refer [minimal-tx]])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom] ; Leo-dom. Todo add merge-tx sugar?
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros wip.form-pure-ct)))

(def cobbblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

; colorless!
(p/defn Merge-tx2 [a b] (p/server (hf/into-tx hf/schema a b)))
(p/defn Merge-tx3 [a b c] (p/server (reduce (partial hf/into-tx hf/schema) [a b c])))
(p/defn Merge-tx4 [a b c d] (p/server (reduce (partial hf/into-tx hf/schema) [a b c d])))
(p/defn Merge-tx5 [a b c d e] (p/server (reduce (partial hf/into-tx hf/schema) [a b c d e])))
(p/defn Merge-tx10 [a b c d e f g h i j] (p/server (reduce (partial hf/into-tx hf/schema) [a b c d e f g h i j])))

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec e)]
      (p/client
        (Merge-tx2.
          (dom/dl
            (Merge-tx10.
              (dom/dt (dom/text "id"))
              (dom/dd (let [v (ui/input (:db/id record) (dom/props {::dom/disabled true}) #_hf/loading)]))

              (dom/dt (dom/text "name"))
              (dom/dd (let [v (ui/input (:label/name record) #_hf/loading)]
                        [[:db/add e :label/name v]
                         #_[:db/cas ...]]))

              (dom/dt (dom/text "sortName"))
              (dom/dd (let [v (ui/input (:label/sortName record) #_hf/loading)]
                        [[:db/add e :label/sortName v]]))

              (dom/dt (dom/text "gid"))
              (dom/dd (let [v (ui/uuid (:label/gid record) #_hf/loading)]
                        [[:db/add e :label/gid v]]))

              (dom/dt (dom/text "startYear"))
              (dom/dd (let [v (ui/long (:label/startYear record) #_hf/loading)]
                        [[:db/add e :label/startYear v]]))
              ))
          (dom/pre (dom/text (pprint-str record))))))))

(p/defn App [e]
  (p/client
    (Merge-tx2.
      (p/server (LabelForm. e))
      (Popover. "change name" (p/partial 1 LabelForm e))))) ; CT impulse, must feedback busy when handled

(p/defn Stage [stage]
  (p/client
    (dom/element "style" (dom/text ".hyperfiddle-stage { display: block; width: 100%; }"))
    (ui/edn-editor stage {::dom/disabled true
                          ::dom/class "hyperfiddle-stage"})))

(p/defn Page [e]
  (p/with-cycle [stage []]
    (binding [hf/db (p/wrap (:db-after (hf/with hf/db stage)))]
      (let [tx (App. e)
            _ (Stage. stage)]
        (minimal-tx hf/db tx))))
  nil)

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