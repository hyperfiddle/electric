(ns user.photon-2-rendering
  (:require [datahike.api :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(hyperfiddle.rcf/enable!)

(defn includes-str? [v needle]
  (clojure.string/includes? (.toLowerCase (str v))
                            (.toLowerCase (str needle))))

(defn orders [db ?email]
  (->>
    (d/q '[:find [?email ...]
           :in $ ?needle :where
           [?e :order/email ?email]
           [(user.photon-2-rendering/includes-str? ?email ?needle)]]
         db (or ?email ""))
    sort))                                                  ; stabilize tests

(tests (def db @(requiring-resolve 'dev/db)))
(tests
  (with (p/run (! (orders db ""))) % := ["alice@example.com"
                                         "bob@example.com"
                                         "charlie@example.com"])
  (with (p/run (! (orders db "alice"))) % := ["alice@example.com"]))

(tests
  (def !state (atom {}))
  (with
    (p/run
      (!
        (let [state (p/watch !state)]
          (orders db (:email state)))))
    % := [9 10 11]
    (swap! !state assoc :email "alice")
    % := [9]
    (swap! !state assoc :email "bob")
    % := [10]))

(p/defn App [db email]
  [:table
   (p/for [x (orders db email)]                             ; concurrent for with diffing and stabilization
     [:tr x])])

(tests
  (def !state (atom {:email nil}))
  (with (p/run
          (let [state (p/watch !state)]
            (! (App. db (:email state)))))
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    (swap! !state assoc :email "alice")
    % := [:table [[:tr 9]]]
    (swap! !state assoc :email "bob")
    % := [:table [[:tr 10]]]
    (swap! !state assoc :email "")
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    ))


; Next up: how can we prove it is reactive and skipping work?
