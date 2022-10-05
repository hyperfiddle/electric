(ns wip.hfql2 ; adapted from user.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql2]
            [hyperfiddle.hfql2.ui :as ui2]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [order orders genders shirt-sizes]]
            [hyperfiddle.photon-ui :as pui])
  #?(:cljs (:require-macros user.demo-hfql)))


(p/defn Tee-shirt-orders []
  ;; Warning: HFQL is unstable
  (p/client
    (let [!needle (atom "")
          needle (p/watch !needle)]
      (pui/input {::dom/placeholder "alice …"
                  ::pui/input-event (p/fn [e] (reset! !needle (.. e -target -value)))})
      (p/server
        (hfql2/hfql
          {(orders needle) [#_:order/email
                            {(props :order/gender {} #_{::hf/options (genders)}) [(props :db/ident {::hf/as gender})]}
                            {(props :order/shirt-size {::hf/options (shirt-sizes gender "")
                                                       ::hf/option-label :db/ident}) [:db/ident]}]}) 
        #_{(orders "") [:db/id :order/email]}
        #_{(orders .)
              [:order/email
               {(props :order/gender {::hf/options      (genders)
                                      ::hf/option-label :db/ident
                                      ::hf/render       ui/select-options})
                [:db/ident]}
               {(props :order/shirt-size {::hf/options      (shirt-sizes order/gender .)
                                          ::hf/option-label :db/ident
                                          ::hf/render       ui/select-options})
                [:db/ident]}]}))))

(p/defn Browser []
  (p/client
    (dom/div {::dom/id "main"
              ::dom/class "browser hyperfiddle-hfql"}
      (dom/div {::dom/class "view"}
        (p/server (Tee-shirt-orders.))))))

(p/defn App []
  (binding [hf/db hf/*db* ; why
            hf/Render ui2/Render] ; remove for livecoding demo
    (ui/with-spec-render
      (Browser.))))

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
