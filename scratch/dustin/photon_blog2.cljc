(ns user
  (:require [datomic.api :as d]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon :as photon :refer [defnode]]
            [missionary.core :as m]))

(defnode persons [db needle & [sort-order]]
  (sort (or sort-order <)
        (datomic.api/q '[:find [?e ...] :in $ ?needle :where
                         [?e :person/email ?email]
                         [(clojure.string/includes? ?email ?needle)]]
                       db (or needle ""))))

(defnode render-persons [db needle]
  ~@(dom/div
      (dom/h1 "submissions")
      (dom/table
        (photon/for [e ~@(persons db needle)]
          ~@(let [{:keys [:db/id
                          :person/email
                          :person/gender
                          :person/shirt-size]} (datomic.api/entity db e)]
              ~@(dom/tr
                  (dom/td (str id))
                  (dom/td email)
                  (dom/td (pr-str gender))
                  (dom/td (pr-str shirt-size))))))))


(defnode render-persons [xs props]
  (dom/div
    (dom/h1 "submissions")
    (dom/table
      (photon/for [{:keys [:db/id
                           :person/email
                           :person/gender
                           :person/shirt-size]} xs]
        (dom/tr
          (dom/td (str id))
          (dom/td email)
          (dom/td (pr-str gender))
          (dom/td (pr-str shirt-size)))))))

(defnode query-persons [db needle sort-order]
  (let [xs (sort sort-order
                 (datomic.api/q
                   '[:find [?e ...] :in $ ?needle :where
                     [?e :person/email ?email]
                     [(clojure.string/includes? ?email ?needle)]]
                   db (or needle "")))]
    (photon/for [x xs]
      (select-keys (datomic.api/entity db x)
                   [:db/id
                    :person/email
                    :person/gender
                    :person/shirt-size]))))

(binding )
