(ns dustin.y2022.forms.circuit-summation3
  (:require [contrib.str :refer [pprint-str]]
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom] ; no sugar
            [hyperfiddle.photon-ui2 :as ui] ; looped controls
            [hyperfiddle.popover :refer [Popover]]
            #?(:clj [hyperfiddle.txn :refer [minimal-tx]]))
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.forms.circuit-summation3)))

(def cobbblestone 536561674378709)
(def label-form-spec [:db/id :label/gid :label/name :label/sortName :label/startYear])

(p/defn ValueLog [v]
  (let [!log (atom ()), log (p/watch !log)]
    (swap! !log conj v)
    (dom/div (dom/text (str (take 5 log))))
    v))

(p/defn Form [e]
  (p/server ; hack - popover
    (let [record (d/pull hf/db label-form-spec e)]
      (p/client
        ;(p/Unglitch. record)
        (dom/div (dom/text (name hf/loading) " " (hf/Load-timer.) "ms"))
        (dom/dl
          (dom/dt (dom/text "name"))
          (dom/dd (let [v (ValueLog. (ui/input (:label/name (p/server record))))]
                    (p/Unglitch. v)
                    (p/server (hf/Transact!. [[:db/add e :label/name v]]))))

          (dom/dt (dom/text "sortName"))
          (dom/dd (let [v (ValueLog. (ui/input (:label/sortName (p/server record))))]
                    (p/Unglitch. v)
                    (p/server (hf/Transact!. [[:db/add e :label/sortName v]])))))
        (dom/pre (dom/text (pprint-str record)))))))

(p/defn App [e]
  (Form. e)
  (Form. e))

#?(:clj (defn with! [db tx]
          (if-some [tx (seq (minimal-tx db tx))] ; stabilize first loop (optional)
            (:db-after (d/with db {:tx-data tx}))
            db)))

(p/defn Demo []
  (p/client
    (dom/h1 (dom/text (str `Demo)))
    (p/with-cycle [loading ::hf/idle]
      (binding [hf/loading loading]
        (try
          (p/server
            (let [!db @(requiring-resolve 'test/datomic-conn) ; todo datomic-tx-listener. don't lose stage on rebase!
                  !t (atom (d/with-db !db))]
              (binding [hf/db (p/watch !t) ; global ordering
                        hf/Transact! (p/fn [tx] (p/wrap (swap! !t with! tx)) nil)] ; advance time
                (App. cobbblestone))))
          ::hf/idle (catch Pending e ::hf/loading))))
    nil))