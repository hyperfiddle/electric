(ns user.photon-2-rendering
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(hyperfiddle.rcf/enable!)

(def conn (d/create-conn {:order/email {}}))
(d/transact! conn [{:order/email "alice@example.com"}
                   {:order/email "bob@example.com"}
                   {:order/email "charlie@example.com"}])
(def db @conn)

(defn includes-str? [v needle]
  (clojure.string/includes? (.toLowerCase (str v))
                            (.toLowerCase (str needle))))

(defn orders [db ?email]
  (sort
    (d/q '[:find [?email ...]
           :in $ ?needle :where
           [?e :order/email ?email]
           [(user.photon-2-rendering/includes-str? ?email ?needle)]]
         db (or ?email ""))))

(tests
  (with (p/run (! (orders db ""))) % := ["alice@example.com" "bob@example.com" "charlie@example.com"])
  (with (p/run (! (orders db "alice"))) % := ["alice@example.com"]))

(tests
  (def !state (atom {}))
  (with
    (p/run
      (!
        (let [state (p/watch !state)]
          (orders db (:email state)))))
    % := ["alice@example.com" "bob@example.com" "charlie@example.com"]
    (swap! !state assoc :email "alice")
    % := ["alice@example.com"]
    (swap! !state assoc :email "bob")
    % := ["bob@example.com"]))

(p/defn App [db email]
  [:table
   (p/for [x (orders db email)]                             ; concurrent for with diffing and stabilization
     [:tr x])])

(tests
  (def !state (atom {:email nil}))
  (with (p/run
          (let [state (p/watch !state)]
            (! (App. db (:email state)))))
    % := [:table [[:tr "alice@example.com"] [:tr "bob@example.com"] [:tr "charlie@example.com"]]]
    (swap! !state assoc :email "alice")
    % := [:table [[:tr "alice@example.com"]]]
    (swap! !state assoc :email "bob")
    % := [:table [[:tr "bob@example.com"]]]))
