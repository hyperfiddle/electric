(ns wip.demo-stage-ui4
  (:require [contrib.css :refer [css-slugify]]
            [contrib.str :refer [pprint-str]]
            #?(:clj [contrib.datomic-contrib :as dx])
            #?(:clj [datomic.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.popover :refer [Popover]]
            #?(:clj [contrib.test.datomic-peer-mbrainz :as test])))

(def label-form-spec [:db/id
                      :label/gid
                      :label/name
                      :label/sortName
                      {:label/type [:db/ident]}
                      {:label/country [:db/ident]}
                      :label/startYear])

(comment (d/pull test/db ['*] test/cobblestone))

#?(:clj (defn type-options [db & [needle]]
          (->> (d/q '[:find (pull ?e [:db/ident]) :in $ ?needle :where
                      [?e :db/ident ?ident]
                      [(namespace ?ident) ?x-ns] [(= ?x-ns "label.type")]
                      [(name ?ident) ?x-label]
                      [(contrib.str/includes-str? ?x-label ?needle)]]
                    db (or needle ""))
               (map first))))

(comment
  (type-options test/db "")
  (type-options test/db "prod")
  (type-options test/db "bootleg")
  (type-options test/db nil))


(e/defn Form [e]
  (let [record (d/pull hf/db label-form-spec e)]
    (e/client
      (dom/dl

        (dom/dt (dom/text "id"))
        (dom/dd (ui/input (:db/id record) nil (dom/props {::dom/disabled true})))

        (dom/dt "gid")
        (dom/dd (ui/uuid (:label/gid record) nil (dom/props {::dom/disabled true})))

        (dom/dt (dom/text "name"))
        (dom/dd (ui/input (:label/name record)
                  (e/fn [v]
                    (println 'input! v)
                    (e/server #_(when true) (hf/Transact!. [[:db/add e :label/name v]])))
                  (dom/props {:id "name"})))

        (dom/dt (dom/text "name2"))
        (dom/dd (ui/input (:label/name record)
                  (e/fn [v]
                    (println 'input2! v)
                    (e/server #_(when true) (hf/Transact!. [[:db/add e :label/name v]])))
                  (dom/props {:id "name2"})))

        (dom/dt (dom/text "sortName"))
        (dom/dd (ui/input (:label/sortName record)
                          (e/fn [v] (e/server (hf/Transact!. [[:db/add e :label/sortName v]])))))


        (dom/dt (dom/text "type"))
        (dom/dd (e/server
                  (ui/typeahead
                    (:label/type record)
                    (e/fn V! [option] (hf/Transact!. [[:db/add e :label/type (:db/ident option)]]))
                    (e/fn Options [search] (type-options hf/db search))
                    (e/fn OptionLabel [option] (-> option :db/ident name)))))

        ; country

        (dom/dt (dom/text "startYear"))
        (dom/dd (ui/long (:label/startYear record)
                         (e/fn [v] (e/server (hf/Transact!. [[:db/add e :label/startYear v]])))))
        )

      (dom/pre (dom/text (pprint-str record))))))

(e/defn Page []
  #_(e/client (dom/div (if hf/loading "loading" "idle") " " (str (hf/Load-timer.)) "ms"))
  (Form. test/cobblestone)
  #_(Form. test/cobblestone)
  (e/client (Popover. "open" (e/fn [] (e/server (Form. test/cobblestone))))))

(e/defn CrudForm []
  (e/client (dom/h1 (dom/text (str `CrudForm))))
  (e/server
    (let [conn @(requiring-resolve 'contrib.test.datomic-peer-mbrainz/conn)
          secure-db (d/db conn)] ; todo datomic-tx-listener
      (binding [hf/schema (new (dx/schema> secure-db))
                hf/into-tx' hf/into-tx
                hf/with (fn [db tx] ; inject datomic
                          (try (:db-after (d/with db {:tx-data tx}))
                               (catch Exception e
                                 (println "...failure, e: " e)
                                 db)))
                hf/db secure-db]
        (hf/branch
          (Page.)
          (e/client
            (dom/hr)
            (dom/element "style" (str "." (css-slugify `staged) " { display: block; width: 100%; height: 10em; }"))
            (ui/edn (e/server hf/stage) nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)}))))))))
