(ns user.orders-ui
  "todo modernize"
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]]
            [hyperfiddle.photon-dom :as dom]))

(p/defn Orders []
  ~@(ui/with-spec-render
      (binding [hf/db     hf/*db*
                hf/render ui/render]
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

(def ^:export reactor)

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))

(defn ^:dev/after-load ^:export start! []
  (set! reactor (main js/console.log js/console.error)))