(ns user.orders-ui
  "This is a self-contained example; run it with:
  clj -X:devkit :main user.orders-ui/main"
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]]))

(p/defn Orders []
  ~@(ui/with-spec-render
      (binding [hf/db     hf/*db*
                hf/Render ui/Render]
        (hf/hfql
          {(orders .)
           [:order/email
            {(props :order/gender {::hf/options      (genders)
                                   ::hf/option-label :db/ident
                                   ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
            {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                       ::hf/option-label :db/ident
                                       ::hf/render       ui/select-options}) [:db/ident]}]}))))


(def main
  (p/client
    (p/main
      (binding [dom/parent (dom/by-id "root")]
        (dom/div
          (dom/attribute "id" "main")
          (dom/class "browser")
          (dom/div
            (dom/class "view")
            (new user.orders-ui/Orders)))))))
