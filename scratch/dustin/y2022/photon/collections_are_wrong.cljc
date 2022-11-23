(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(defn foo [& {:keys [::a ::b]}] (println a b))
(defn foo2 [{:keys [::a ::b]}] (println a b))

(tests
  (foo ::a 1 ::b 2)
  (foo2 {::a 1 ::b 2}))

(p/defn Foo3 [props]
  (p/for [p props]
    (println p)))

(tests
  (def !x (atom {::a 1}))
  (def dispose (p/run (Foo3. (p/watch !x))))
  (swap! !x assoc ::b 2)
  (dispose))

(p/defn Foo4 [{:keys [::a ::b] :as props}]
  #_(println props)
  (println a) #_ (m/latest println a)
  (println b)) #_ (m/latest println b)

(tests
  (def !x (atom {::a 1}))
  (def dispose (p/run (Foo4. (p/watch !x))))
  (swap! !x assoc ::b 2)
  (dispose))

(p/defn Foo5 [a b]
  #_(println props)
  (println a) #_ (m/latest println a)
  (println b)) #_ (m/latest println b)

(tests
  (def !x (atom 1))
  (def !y (atom nil))
  (def dispose (p/run (Foo4. (p/watch !x) (p/watch !y))))
  (reset! !y 2)
  (dispose))


(defn teeshirt-orders' [db ?email]
  #?(:clj
     (p/derivative :db/id
                   (sort
                     (d/q '[:find #_[?e ...] [(pull ?e [:db/id :order/email :order/gender]) ...]
                            :in $ ?needle :where
                            [?e :order/email ?email]
                            [(clojure.string/includes-str? ?email ?needle)]]
                          db (or ?email ""))))))

(p/defn Teeshirt-orders-view [db]
  (p/client
    (dom/div {:class "hyperfiddle-hfql"}
      (dom/h2 "frontend/backend webview with server push")
      (let [!filter (atom ""), filter (p/watch !filter)]
        (ui/input {::dom/type :search
                   ::dom/placeholder "Filter…"
                   ::ui/input-event (p/fn [e] (reset! !filter (.. e -target -value)))})
        (dom/table

          (p/server
            (p/integrate
              (teeshirt-orders' db filter)
              (p/client
                (dom/tr
                  (dom/td id)
                  (dom/td (p/server email))
                  (dom/td (p/server email))
                  (dom/td (p/server gender)))))

            (p/for-by :db/id [{:keys [:order/email :order/gender]} (teeshirt-orders db filter)]
              (p/client
                (dom/tr
                  (dom/td id)
                  (dom/td (p/server email))
                  (dom/td (p/server email))
                  (dom/td (p/server gender)))))))))))

(p/defn App []
  (let [db (p/watch conn)]
    (Teeshirt-orders-view. db)))

(comment
  #?(:clj (d/transact conn [{:db/id 2 :order/email "bob2@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  )
