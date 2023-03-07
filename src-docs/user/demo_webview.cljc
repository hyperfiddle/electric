(ns user.demo-webview
  "Electric fullstack query/view composition with client/server transfer"
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]))

; A database backed webview with reactive updates.
; The webview is subscribed to the database, which updates with each transaction.
; Run a transaction (from the REPL) and see the connected tabs update live.

(defonce conn #?(:cljs nil ; state survives reload
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
              [(clojure.string/includes? ?email ?needle)]]
            db (or ?email "")))))

(e/defn Teeshirt-orders-view [db]
  (e/client
    (dom/div
      (dom/h2 (dom/text "frontend/backend webview with server push"))
      (let [!search (atom ""), search (e/watch !search)]
        (ui/input search (e/fn [v] (reset! !search v))
          (dom/props {:placeholder "Filter..."}))
        (dom/table (dom/props {:class "hyperfiddle"})
          (e/server
            (e/for [id (e/offload #(teeshirt-orders db search))]
              (let [!e (d/entity db id)]
                (e/client
                  (dom/tr
                    (dom/td (dom/text id))
                    (dom/td (dom/text (e/server (:order/email !e))))
                    (dom/td (dom/text (e/server (:order/gender !e))))))))))))))

(e/defn Webview []
  (let [db (e/watch conn)] ; reactive "database value"
    (Teeshirt-orders-view. db)))

(comment
  #?(:clj (d/transact conn [{:db/id 2 :order/email "bob2@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  )
