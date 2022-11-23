(ns user.demo-stage
  (:require #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  #?(:cljs (:require-macros user.demo-stage)))

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

#?(:clj (defn query-label-name [db e] (:label/name (d/pull db [:label/name] e))))

(p/defn LabelForm [e]
  (dom/h1 "Change name for label: " (p/server (query-label-name hf/db e)))
  (dom/label "label id")
  (dom/Input. (p/server (pr-str (:label/gid (d/pull hf/db [:label/gid] e)))) #_{:disabled true})
  (let [nom' (p/with-cycle [nom (p/server (query-label-name hf/db e))]
                        (dom/label "label name")
                        (dom/Input. nom))]
    (when (contrib.str/blank->nil nom') ; todo validate spec
      [[:db/add e :label/name nom']])))

(p/defn Page [e]
  (p/client
    (dom/h1 "Label page for: " (p/server (query-label-name hf/db e)))
    (Popover. "change name" (p/partial 1 LabelForm e) false)))

(p/defn Demo []
  (let [secure-db (d/with-db @(requiring-resolve 'test/datomic-conn))
        schema (new (dx/schema> secure-db))]
    (let [!stage (atom []) stage (p/watch !stage)]
      (binding [hf/with (fn [db tx] (d/with db {:tx-data tx}))] ; todo required by popover TODO
        (binding [hf/db (:db-after (hf/with secure-db stage))]

          (when-some [tx (Page. 536561674378709)]
            (swap! !stage (partial hf/into-tx schema) tx))

          (when-some [stage' (ui/Edn-editor. stage)]
            (reset! !stage stage')))))))



;; TODO next demo
;; hfql in popover
;; hfql widgets return eav triples
;; hfql collects these and returns as stage (can route through atom if needed to avoid changing hfql)
;
;(p/defn Page [district]
;  ;(dom/h1 "District page for " (p/server (get-district-name hf/db district)))
;  (hfql {district
;         [get-district-name
;          {(props district {:render (do (dom/h1 "Change District name for " (p/server (get-district-name rosie/db district)))
;                                        (hf/V.))
;                            :popover true
;                            :popover-label (str "Change District name for " get-district-name)
;                            :popover-tx nil})
;           [:district/id
;            :district/name]}]}))