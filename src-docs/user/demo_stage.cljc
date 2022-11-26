(ns user.demo-stage
  (:require #?(:clj [contrib.datomic-contrib :as dx])
            [contrib.str :refer [empty->nil]]
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

  (p/server
    (hf/into-tx hf/schema

      (p/client
        (dom/label "label id")
        (ui/input (p/server (pr-str (:label/gid (d/pull hf/db [:label/gid] e)))) {::dom/disabled true})
        nil) ; todo disabled input should emit nil

      (p/client
        (dom/label "label name")
        (when-some [[nom'] (p/with-cycle [[nom' nom] [nil (p/server (query-label-name hf/db e))]]
                             [(ui/input nom (= nom nom')) nom])] ; todo validate
            [[:db/add e :label/name nom']])))))

(p/defn Page [e]
  (p/client
    (dom/h1 "Label page for: " (p/server (query-label-name hf/db e)))
    (p/server
      (hf/into-tx hf/schema
        (p/client (Popover. "change name" (p/partial 1 LabelForm e)))
        (p/client (Popover. "change name" (p/partial 1 LabelForm e)))))))

(p/defn Demo []
  (let [secure-db (d/with-db @(requiring-resolve 'test/datomic-conn))]
    (binding [hf/schema (new (dx/schema> secure-db))
              hf/with (fn [db tx] (d/with db {:tx-data tx}))] ; todo required by popover TODO
      (let [!stage (atom []) stage (p/watch !stage)]
        (p/client
          (p/with-cycle [loading ::hf/loading]
            (binding [hf/loading loading] ; todo distributed glitch
              (dom/div (name loading) " " (str (hf/Load-timer.)) "ms")
              (try
                (p/server
                  (binding [hf/db (:db-after (hf/with secure-db stage))] ; task can fail
                    (when-some [tx (Page. 536561674378709)]
                      (swap! !stage #(hf/into-tx hf/schema %1 %2) tx))
                    (when-some [stage' (p/client (ui/edn-editor stage {::dom/disabled true}))]
                      (reset! !stage stage'))))
                ::hf/idle
                (catch Pending e ::hf/loading))))
          nil)))))

;(let [stage (reduce into-tx [(Popover. "change name" (p/partial 1 LabelForm e))
;                             (Popover. "change name" (p/partial 1 LabelForm e))
;                             @c])])

(comment
  "Sketch of EAV forms"

  (defmacro form [e & body]
    (binding [hf/e e]
      (let [[tx1# tx2#] (do ~@body)]
        (p/server (hf/into-tx hf/schema tx1# tx2#)))))

  (defmacro field [a v & [props]]
    `(when-some [[v'#] (p/with-cycle [[v'# v#] [nil ~v]]
                         [(ui/input v# ~props (= v# v'#))
                          v#])]
       ; todo validate
       [[:db/add ~hf/e ~a v'#]]))

  (p/defn LabelForm [e]
    (dom/h1 "Change name for label: " (p/server (query-label-name hf/db e)))
    (form e
      (field :label/gid (p/server (pr-str (:label/gid (d/pull hf/db [:label/gid] e)))) {::dom/disabled true})
      (field :label/name (p/server (query-label-name hf/db e)) {}))))


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