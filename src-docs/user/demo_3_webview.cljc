(ns user.demo-3-webview
  "Photon fullstack query/view composition with client/server transfer"
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            user.util)
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-3-webview)))

(defonce conn #?(:cljs nil                                  ; state survives reload
                 :clj  (doto (d/create-conn {:order/email {}})
                         (d/transact! [{:order/email "alice@example.com" :order/gender :order/female}
                                       {:order/email "bob@example.com" :order/gender :order/male}
                                       {:order/email "charlie@example.com" :order/gender :order/male}]))))

(defn orders [db ?email]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(user.util/includes-str? ?email ?needle)]]
            db (or ?email "")))))

(p/def db)                                                  ; server

(p/defn View []
  (dom/h2 (dom/text "frontend/backend webview with server push"))
  (let [email (ui/input {:type :search, :placeholder "Filter…"})]
    (dom/table
     ~@(p/for [id (orders db email)]
         ~@(dom/tr
            (dom/td (dom/text id))
            (dom/td (dom/text ~@(:order/email (d/entity db id))))
            (dom/td (dom/text ~@(:order/gender (d/entity db id)))))))))

(p/defn App []
  (binding [dom/node (dom/by-id "root")]
    ~@(binding [db (p/watch conn)]                          ; server
        ~@(View.))))

(def main #?(:cljs (p/client (p/main (try (App.) (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  #?(:clj (d/transact conn [{:db/id 2 :order/email "bob2@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  )
