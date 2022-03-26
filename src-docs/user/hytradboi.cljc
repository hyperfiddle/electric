(ns user.hytradboi
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            #?(:clj [hyperfiddle.q9 :refer [hfql]])
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui6 :as ui]
            [user.teeshirt-orders :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render)
  #?(:cljs (:require-macros [hyperfiddle.q9 :refer [hfql]]
                            [user.hytradboi :refer [view App]]
                            [user.teeshirt-orders :refer [orders genders shirt-sizes]])))

(p/defn App []

  (hfql

    {(orders .)
     [:dustingetz/email
      {(props :dustingetz/gender {::hf/options (genders)})
       [:db/ident]}
      {(props :dustingetz/shirt-size {::hf/options (shirt-sizes dustingetz/gender .)})
       [:db/ident]}]}

    ))

(p/defn view []
  (p/$ codemirror/edn ~@#_"server" (ui/with-spec-render (p/$ App))))

(comment
  (def !x (atom "alice"))

  (p/run
    (binding [hf/db (hf/->DB "$" 0 nil hf/*$*)]
      (!
        (p/$ App ~(m/watch !x)))))

  % := '{(user.teeshirt-orders/orders _)
         [{:dustingetz/gender     #:db{:ident :dustingetz/female},
           :dustingetz/email      "alice@example.com",
           :dustingetz/shirt-size _}]}

  (reset! !x "bob")

  % := '{(user.teeshirt-orders/orders _)
         [{:dustingetz/gender     #:db{:ident :dustingetz/male},
           :dustingetz/email      "bob@example.com",
           :dustingetz/shirt-size _}]}
  )
