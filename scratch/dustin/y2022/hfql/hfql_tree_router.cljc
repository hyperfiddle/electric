(ns dustin.y2022.hfql-tree-router
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            #?(:clj [hyperfiddle.hfql :refer [hfql]])
            [hyperfiddle.rcf :refer [tests tap %]]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [user.persons :refer [persons genders shirt-sizes]]
            dustin.y2022.edn-render)
  #?(:cljs (:require-macros [hyperfiddle.hfql :refer [hfql]]
                            [user.hytradboi :refer [view App]]
                            [user.persons :refer [persons genders shirt-sizes]])))

(comment

  `(persons "alice")

  ; if the field is gray we can route it

  `{(persons "bob" 100) {10 {:dustingetz/gender     (genders . "ma" 100)
                             :dustingetz/shirt-size (shirt-sizes {}
                                                                 "sm"
                                                                 100)}}}
  )


(p/defn App []
  (binding []

    (hfql
      {(persons "")
       [:db/id
        :dustingetz/email
        {(props :dustingetz/gender {::hf/options #_(p/client) (genders)
                                    ::hf/option-label         :db/ident #_(p/fn [x] ((comp name :db/ident) x))
                                    ::hf/render               dustin.y2022.edn-render/edn-select})
         [:db/ident]}
        {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes {(props dustingetz/gender {::hf/options (genders)})
                                                                        [:db/ident]}
                                                                       "small"
                                                                       100)
                                        ::hf/option-label :db/ident
                                        ::hf/render       dustin.y2022.edn-render/edn-select
                                        #_#_::hf/render (p/fn [>v props]
                                                          (hfql {(eval (::hf/options props))
                                                                 (::hf/pull props)}
                                                                ))})
         [:db/ident]}]
       })

    ))

(tests
  (p/run
    (binding [hf/db (hf/->DB "$" 0 nil hf/*$*)]
      (tap (p/$ App "bob"))))
  % := '{(user.persons/persons .)
         [{:dustingetz/gender     #:db{:id 1, :ident :dustingetz/male},
           :dustingetz/email      "bob@example.com",
           :dustingetz/shirt-size #:db{:ident :dustingetz/mens-large},
           :db/id                 10}]})

(p/defn view []
  (p/$ codemirror/edn ~@(ui/with-spec-render (p/$ App))))
