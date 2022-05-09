(ns user.photon-2-hiccup
  (:require [datahike.api :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(def db @(requiring-resolve 'dev/db))

(defn includes-str? [v needle]
  (clojure.string/includes? (.toLowerCase (str v))
                            (.toLowerCase (str needle))))

(defn orders [?email]
  (sort                                                     ; stabilize tests
    (d/q '[:find [?e ...] :in $ ?needle :where
           [?e :order/email ?email]
           [(user.photon-2-hiccup/includes-str? ?email ?needle)]]
         db (or ?email ""))))

(tests
  (with (p/run (! (orders ""))) % := [9 10 11])
  (with (p/run (! (orders "alice"))) % := [9]))

(tests
  (def !state (atom {}))
  (with
    (p/run
      (!
        (let [state (p/Watch. !state)]
          (orders (:email state)))))
    % := [9 10 11]
    (swap! !state assoc :email "alice")
    % := [9]
    (swap! !state assoc :email "bob")
    % := [10]))

(p/defn App [email]
  [:table
   (p/for [x (orders email)]                                ; concurrent for with diffing and stabilization
     [:tr x])])

(tests
  (def !state (atom {:email nil}))
  (with (p/run
          (let [state (p/Watch. !state)]
            (! (App. (:email state)))))
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    (swap! !state assoc :email "alice")
    % := [:table [[:tr 9]]]
    (swap! !state assoc :email "bob")
    % := [:table [[:tr 10]]]
    (swap! !state assoc :email "")
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    ))


; Next up: how can we prove it is reactive and skipping work?
