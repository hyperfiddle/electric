(ns user.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.hfql.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes]])
  #?(:cljs (:require-macros user.demo-hfql)))


(p/defn Tee-shirt-orders []
  ;; Warning: HFQL is unstable
  (hf/hfql
    {(orders .)
     [:order/email
      {(props :order/gender {::hf/options (genders)
                             ::hf/option-label :db/ident
                             ::hf/render ui/select-options})
       [:db/ident]}
      {(props :order/shirt-size {::hf/options (shirt-sizes order/gender .)
                                 ::hf/option-label :db/ident
                                 ::hf/render ui/select-options})
       [:db/ident]}]}))

(p/defn Browser []
  (p/client
    (dom/div {::dom/id "main"
              ::dom/class "browser hyperfiddle-hfql"}
      (dom/div {::dom/class "view"}
        (p/server (Tee-shirt-orders.))))))

(p/defn App []
  (binding [hf/db hf/*db* ; why
            hf/Render ui/Render] ; remove for livecoding demo
    (ui/with-spec-render
      (Browser.))))

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
