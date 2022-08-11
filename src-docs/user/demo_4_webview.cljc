(ns user.demo-4-webview
  "Photon fullstack query/view composition with client/server transfer"
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            user.util)
  #?(:cljs (:require-macros user.demo-4-webview)))

(defonce conn #?(:cljs nil                                  ; state survives reload
                 :clj  (doto (d/create-conn {:order/email {}})
                         (d/transact! [{:order/email "alice@example.com" :order/gender :order/female}
                                       {:order/email "bob@example.com" :order/gender :order/male}
                                       {:order/email "charlie@example.com" :order/gender :order/male}]))))

(defn teeshirt-orders [db ?email]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(user.util/includes-str? ?email ?needle)]]
            db (or ?email "")))))

(p/def db)                                                  ; server

(p/defn Teeshirt-orders-view []
  (dom/div
    (dom/h2 "frontend/backend webview with server push")
    (let [!filter (atom "") filter (p/watch !filter)]
      (ui/input {::dom/type        :search
                 ::dom/placeholder "Filter…"
                 ::ui/input-event  (p/fn [e] (reset! !filter (:value dom/node)))})
      (dom/table
        (p/server
          (p/for [id (teeshirt-orders db filter)]
            (p/client
              (dom/tr
                (dom/td id)
                (dom/td (p/server (:order/email (d/entity db id))))
                (dom/td (p/server (:order/gender (d/entity db id))))))))))))

(p/defn App []
  (p/server
    (binding [db (p/watch conn)]
      (p/client (Teeshirt-orders-view.)))))

(comment
  #?(:clj (d/transact conn [{:db/id 2 :order/email "bob2@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  )
