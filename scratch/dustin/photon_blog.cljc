(ns dustin.photon-blog
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars system node thread]]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hfdl.impl.util :as u])
  )

(defnode typeahead-select [& {:keys []}])

(defnode example-form [db e]
  (dom/form
    (dom/field
      ::hf/label "shirt size"
      (typeahead-select
        ::hf/eav [e :dustingetz/shirt-size]
        ::options (node [needle]
                    (let [gender (thread (:dustingetz/gender (datomic.api/entity db e)))
                          es (thread
                               (datomic.api/q
                                 '[:in $ ?gender :find [?e ...] :where
                                   [?e :dustingetz/type :dustingetz/shirt-size]
                                   [?e :dustingetz/gender ?gender]]
                                 db gender (or needle "")))]
                      (for [e es]
                        (dom/option ~@(thread (:db/ident (datomic.api/entity db e)))))))))))
