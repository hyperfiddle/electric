(ns user.crud2
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode node vars main]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]))

(defn select-widget [& {:keys [value on-change options]}]
  (let [!needle (atom "")]
    [:<> [:input.select-needle {:value value :on-change #(reset! !needle %)}]
     [:select {:selected value :on-change on-change}
      (options @!needle)]]))

(defn email-taken [db email]
  (some? (d/q '[:in ?x $ :find ?e . :where [?e :dustingetz/email ?x]] db email)))

(defn example-form [db e]
  (let [!e (d/entity db e)
        !email (atom (:dustingetz/email !e))
        !shirt-size (atom (:dustingetz/shirt-size !e))]
    [:form
     [:div [:label "email"]
      [:input {:class (if (let [x (:dustingetz/email !e)] (email-taken db x)) "invalid" "valid")
               :value @!email :on-change #(reset! !email %)}]]

     [:div [:label "shirt-size"]
      (select-widget
        :value @!shirt-size :on-change #(reset! !shirt-size %)
        :options (fn [needle]
                   (let [gender (:dustingetz/gender (datomic.api/entity db e))
                         es (datomic.api/q
                              '[:in $ ?gender :find [?e ...] :where
                                [?e :dustingetz/type :dustingetz/shirt-size]
                                [?e :dustingetz/gender ?gender]]
                              db gender (or needle ""))]
                     (for [e es]
                       [:option (:db/ident (datomic.api/entity db e))]))))]]))


(tests
  (def *conn*)
  (def !db (atom []))
  (def !e (atom [:dustingetz/email "alice@example.com"]))
  (def sampler (test-dag (example-form (partial d/transact *conn*) ~(m/watch !db) ~(m/watch e))))
  @sampler := 3)
