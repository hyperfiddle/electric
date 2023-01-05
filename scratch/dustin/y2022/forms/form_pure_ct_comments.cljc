(ns wip.form_pure_ct-comments
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
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

(p/def Merge-tx) ; server

(comment
  (p/run (p/server
           (Merge-tx.
             (p/client {:db/id (ui/input ::ui/value (:db/id record) ::ui/busy hf/loading)})
             (p/client {:db/id (ui/input ::ui/value (:label/name record) ::ui/busy hf/loading)})))))

(p/defn LabelForm [e]
  ; don't validate, must close the circuit no matter what
  (p/server
    (let [record (d/pull hf/db label-form-spec e)]
      (p/client
        (Merge-tx. ; TODO it must handle routing commands as well!
          (dom/h1 (dom/text "Change name for label: " (p/server (:label/name record))))
          (dom/dl
            (Merge-tx.
              (dom/dt (dom/text "id"))
              (dom/dd (as-> (ui/input ::ui/value (:db/id record)
                                      ::dom/disabled true
                                      ::ui/busy hf/loading) v
                            ; not even a real txn, still must close the circuit?
                            ; it will get cancelled out by merge-tx anyway
                            [#_[:db/add e :db/id v]]))

              (dom/dt (dom/text "id"))
              (dom/dd (as-> (ui/input ::ui/value (:label/name record)
                                      ::dom/disabled true
                                      ::ui/busy hf/loading) v
                            [[:db/add e :label/name v]
                             #_[:db/cas ...]])))

            #_ ; map command lang isn't powerful enough, how do you route? how do you :db/cas?
                    {:db/id (do (dom/dt (dom/text "id"))
                                (dom/dd (ui/input ::ui/value (:label/name record)
                                                  ::dom/disabled true
                                                  ::ui/busy hf/loading)))

                     :label/name (do (dom/dt (dom/text "name"))
                                     (dom/dd (ui/input ::ui/value (:label/name record)
                                                       ::ui/busy hf/loading)))})

          (dom/pre (pprint-str record)))))))

(p/defn App [e]
  (p/client
    (Merge-tx.
      (dom/h1 (dom/text "Label page for: " e))
      (p/server (LabelForm. e))
      (Popover. "change name" (p/partial 1 LabelForm e))))) ; CT impulse, must feedback busy when handled

(p/defn Stage [stage]
  (p/client
    (dom/element "style" ".hyperfiddle-stage { display: block; width: 100%; }")
    (ui/edn-editor stage {::dom/disabled true
                          ::dom/class "hyperfiddle-stage"})))

(p/defn Page [e]
  (p/with-cycle [stage []] ; loop a txn, all no-op txns must stabilize to [] !!
    ; Otherwise, we'll always have a "dirty" stage for every "live" field
    (binding [hf/db (p/wrap (:db-after (hf/with hf/db stage)))]
      (let [tx (App. e)
            stage' (Merge-tx. (p/Unglitch. stage)
                              (Stage. stage))]
        (dx/minimal-diff hf/db (Merge-tx. stage' tx))))) ; will stabilize with [], or
  ; we get a really dirty stage with no-op edits for every form on the page
  nil)

(p/defn Demo []
  (bindx [hf/db (d/with-db @(requiring-resolve 'test/datomic-conn))
          hf/schema (new (dx/schema> hf/db))
          Merge-tx (p/fn [& txs] (p/server (reduce (partial hf/into-tx hf/schema) txs))) ; colorless!
          hf/with (fn [db tx] (d/with db {:tx-data tx}))]
    (p/client
      (p/with-cycle [loading ::hf/loading]
        (binding [hf/loading loading]
          (dom/div (dom/text (name loading) " " (hf/Load-timer.) "ms"))
          (try
            (p/server (Page. cobbblestone))
            ::hf/idle (catch Pending e ::hf/loading))))
      nil)))

; Todo: routing, hyperlink
; check busy/loading states in controls
; validation
; what if the txn fails? the whole page crashes, need to retain prior value and revert,
;    what happens to the form local state?
; how does the popover change in this model?
; confirm the popover latches properly?
; Insight: d/transact is not a with-cycle, its a swap! cycle
;   and the datomic txn algebra also is