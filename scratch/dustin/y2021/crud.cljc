(ns user.crud
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests]]))

(defn select-widget [& {:keys [::ref ::options ::render-option]}]
  (let [!needle (atom "")]                                  ; widget local state
    [:<> [:input.select-needle {:value @!needle :on-change #(reset! !needle %)}]
     [:select {:selected @ref :on-change #(reset! ref %)}
      (for [x (options @!needle)]
        (render-option x))]]))

(defn email-taken [db email]
  (some? (d/q '[:in ?x $ :find ?e . :where [?e :dustingetz/email ?x]] db email)))

(defn example-form [*conn* db e]
  (let [!e (d/entity db e)
        !email (atom (:dustingetz/email !e))                ; form dirty state or dom ref
        !gender (atom (:dustingetz/gender !e))
        !shirt-size (atom (:dustingetz/shirt-size !e))]

    [:form
     [:div [:label "email"]
      [:input {:class (if (email-taken db @!email) "invalid" "valid")
               :value @!email :on-change #(reset! !email %)}]]

     [:div [:label "gender"]
      (select-widget
        ::ref !gender
        ::options (constantly [:dustingetz/male :dustingetz/female])
        ::render-option name)]

     [:div [:label "shirt-size"]
      (let [select-widget _]
        (select-widget
          ;:a db
          ::ref !shirt-size
          ::options (fn [needle]
                      (d/q '[:in $ ?gender :find [?e ...] :where
                             [?e :dustingetz/type :dustingetz/shirt-size]
                             [?e :dustingetz/gender ?gender]]
                           db (:dustingetz/gender !e) (or needle "")))
          ::render-option (fn [option db] [:option (:db/ident (d/entity db option))])))]

     [:input {:type     "submit"
              :on-click #(d/transact *conn* [{:db/id                 (:db/id !e)
                                              :dustingetz/email      @!email
                                              :dustingetz/gender     @!gender
                                              :dustingetz/shirt-size @!shirt-size}])}
      "save!"]]))



