(ns wip.orders-ui
  "wip"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes]])
  (:import (hyperfiddle.photon Pending Remote))
  #?(:cljs (:require-macros wip.orders-ui)))

(p/defn Orders []
  ~@(hf/hfql
      {(orders .)
       [:order/email
        {(props :order/gender {::hf/options      (genders)
                               ::hf/option-label :db/ident
                               ::hf/render       ui/select-options})
         [(props :db/ident {::hf/as gender})]}
        {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                   ::hf/option-label :db/ident
                                   ::hf/render       ui/select-options})
         [:db/ident]}]}))

(p/defn App []
  (dom/div {:id    "main"
            :class "browser"}
    (dom/div {:class "view"}
      (Orders.))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/node (dom/by-id "root")]
                                            ~@(binding [hf/db     hf/*db* ; why
                                                        hf/Render ui/Render]
                                                (ui/with-spec-render
                                                  ~@(App.))))
                                          (catch Pending _)
                                          (catch Remote _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
