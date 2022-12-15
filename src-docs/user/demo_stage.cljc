(ns user.demo-stage
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
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

(defn reset!-nil [& args] (apply reset! args) nil)
(defn swap!-nil [& args] (apply swap! args) nil)

(def cobbblestone 536561674378709)

(comment
  (d/pull test/datomic-db ['*] cobbblestone)
  (d/pull test/datomic-db [:db/id
                           :label/gid
                           :label/name
                           :label/sortName
                           {:label/type [:db/ident]}
                           {:label/country [:db/ident]}
                           :label/startYear] cobbblestone)
  )

#?(:clj (defn query-label-name [db e] (:label/name (d/pull db [:label/name] e))))

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

;(defn valid-label? [txn]) -- what is the type? when does validation happen? after hydrate
(p/def into-tx) ; merge-command. server

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec cobbblestone)
          !txn (atom [])] ; alternately, bubble it out of the dom
      (p/client
        (dom/h1 "Change name for label: " (p/server (query-label-name hf/db e)))
        (dom/dd
          (dom/dt "id")
          (dom/dd (do (ui/input (:db/id record) {::dom/disabled true}) nil))

          (dom/dt "gid")
          (dom/dd (do (ui/uuid (:label/gid record) {::dom/disabled true}) nil))

          (dom/dt "name")
          (dom/dd (let [v (ui/uuid (:label/name record))]
                    (p/server (swap!-nil !txn into-tx [[:db/add e :label/name v]]))))

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

          (dom/dt "startYear")
          (dom/dd (let [v (ui/long (:label/startYear record))]
                    (p/server (swap!-nil !txn into-tx [[:db/add e :label/startYear v]])))))
        (dom/pre (pprint-str record)))

      #_(when (valid? record) txn)
      (p/watch !txn))))

(p/defn Page [e]
  (p/client
    (dom/h1 "Label page for: " (p/server (query-label-name hf/db e)))
    (p/server
      (hf/into-tx hf/schema
        (LabelForm. e)
        #_(p/client (Popover. "change name" (p/partial 1 LabelForm e)))
        (p/client (Popover. "change name" (p/partial 1 LabelForm e)))))))

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
                (p/with-cycle [stage []]
                  (binding [hf/db (:db-after (hf/with secure-db stage))] ; task can fail
                    (let [tx (Page. cobbblestone)
                          stage' (hf/into-tx hf/schema (p/Unglitch. stage)
                                   (p/client (ui/edn-editor stage {::dom/disabled true})))]
                      (hf/into-tx hf/schema stage' tx)))))
              ::hf/idle
              (catch Pending e ::hf/loading))))
        nil))))


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