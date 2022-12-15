(ns wip.form-impure-ct-comments
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

;(ns-unalias *ns* 'ui)

(def cobbblestone 536561674378709)

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(p/def stage!) ; server

(comment
  (p/run (p/server
           (Merge-tx.
             (p/client {:db/id (ui/input (:db/id record) #_hf/loading)})
             (p/client {:db/id (ui/input (:label/name record) #_hf/loading)})))))

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec e)]
      (p/client
        ; TODO routing
        ; TODO Validation?
        (dom/h1 (dom/text "Change name for label: " (p/server (:label/name record))))
        (dom/pre (pprint-str record))
        (dom/dl

          (dom/dt (dom/text "id"))
          (dom/dd (do (ui/input (:db/id record) {::dom/disabled true} #_hf/loading) ; TODO busy
                      nil)) ; todo no need to loop a disabled control, it's just a terminal text effect

          ; are the inputs discrete or continuous? where is the conversion?
          ; (when-some [e ...] (swap! ...)) is impulse

          #_#_
                  (dom/dt (dom/text "name"))
                  (dom/dd (let [v' (ui/input (:label/name record) #_hf/loading)]
                            ; do i know if it's dirty?
                            ; the value in the pre-db is different than the value now?
                            ; dirty means touched but not committed
                            ; should it return the clean db val or an altered db val?
                            (p/server (stage! [[:db/add e :label/name v']
                                               #_[:db/cas ...]]))))

          (dom/dt (dom/text "name"))
          (dom/dd (let [v (:label/name record)
                        v' (ui/input v #_hf/loading)]
                    (case (= v' v) ; next time, they'll be equal

                      ; This does not prevent the bug of backspacing to a redundant tx
                      ; therefore still need dx/minimal-diff. therefore diffing here is pointless.
                      ; though possibly convenient because the redundant stmt bug is not that bad.

                      false (p/server (stage! [[:db/add e :label/name v']])) ; recur
                      true nil) ; base case is stable
                    ; Mark dirty somehow - need algebraic effect for that
                    ; to know if this local command is uncommmitted

                    ; this requires tx algebra as the commands are discrete and need to flatten
                    ; a pure CT solution needs no diffing?
                    ; no, the readonly rendering of the stage still requires the command algebra?

                    ; at the point of the commit (popover close) is the point where we make discrete?
                    )))))))

(p/defn App [e]
  (p/client
    (dom/h1 (dom/text "Label page for: " e))
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
      (p/with-cycle [loading ::hf/loading]
        (binding [hf/loading loading]
          (dom/div (dom/text (name loading) " " (hf/Load-timer.) "ms"))
          (try
            (p/server (Page. cobbblestone))
            ::hf/idle (catch Pending e ::hf/loading))))
      nil)))