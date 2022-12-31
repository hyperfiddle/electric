(ns user.demo-stage-ui3
  "ui3/input - don't spam datomic"
  #?(:cljs (:require-macros user.demo-stage-ui3))
  (:require [contrib.css :refer [css-slugify]]
            [contrib.clojurex :refer [bindx]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui3 :as ui]
            [hyperfiddle.photon-ui3 :as ui3]
            [hyperfiddle.popover-ui2 :refer [Popover popover-staged popover staged]])
  (:import [hyperfiddle.photon Pending]))

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
        (dom/dd (ui3/uuid! (:label/gid record) nil (dom/props {::dom/disabled true})))

        (dom/dt (dom/text "name"))
        (dom/dd (ui3/input! (:label/name record)
                            (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/name v]])))))

        (dom/dt (dom/text "sortName"))
        (dom/dd (ui3/input! (:label/sortName record)
                            (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/sortName v]])))))

        (dom/dt (dom/text "startYear"))
        (dom/dd (ui3/long! (:label/startYear record)
                            (p/fn [v] (p/server (hf/Transact!. [[:db/add e :label/startYear v]])))))
        )

      (dom/pre (dom/text (pprint-str record))))))

(p/defn Page []
  #_(p/client (dom/div (if hf/loading "loading" "idle") " " (str (hf/Load-timer.)) "ms"))
  (Form. cobblestone)
  (Form. cobblestone)
  (p/client (Popover. "open" (p/fn [] (p/server (Form. cobblestone))))))

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
            (dom/element "style" (str "." (css-slugify `staged) " { display: block; width: 100%; height: 10em; }"))
            (ui3/edn-editor (p/server hf/stage) false (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)}))))))))
