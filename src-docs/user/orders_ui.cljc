(ns user.orders-ui
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]])
  (:import (hyperfiddle.photon Pending)))

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

(p/defn App []
  (binding [dom/parent (dom/by-id "root")]
    (dom/div
      (dom/attribute "id" "main")
      (dom/class "browser")
      (dom/div
        (dom/class "view")
        (Orders.)))))

(def main #?(:cljs (p/client (p/main (try (App.) (catch Pending _))))))
