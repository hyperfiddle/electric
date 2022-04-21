(ns user.photon-2-hiccup
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]
            [user.orders :refer [orders genders shirt-sizes]]))

(tests
  (with (p/run (! (orders. "")))
    % := [9 10 11]))

(tests
  (def !state (atom {}))
  (with
    (p/run
      (!
        (let [state (new (m/watch !state))]
          (orders. (:email state)))))
    % := [9 10 11]
    (swap! !state assoc :email "alice")
    % := [9]
    (swap! !state assoc :email "bob")
    % := [10]))

(p/defn App [email]
  [:table
   (p/for [x (orders. email)]
     [:tr x])])

(tests
  (def !state (atom {:email nil}))
  (with (p/run
          (let [state (new (m/watch !state))]
            (! (App. (:email state)))))
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    (swap! !state assoc :email "alice")
    % := [:table [[:tr 9]]]
    (swap! !state assoc :email "bob")
    % := [:table [[:tr 10]]]
    (swap! !state assoc :email "")
    % := [:table [[:tr 9] [:tr 10] [:tr 11]]]
    ))


; how can we prove it is reactive and skipping work?