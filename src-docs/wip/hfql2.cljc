(ns wip.hfql2 ; adapted from user.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql2]
            [hyperfiddle.hfql2.ui :as ui2]
            [hyperfiddle.hfql2.router :as router]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [order orders genders shirt-sizes one-order]]
            [hyperfiddle.photon-ui :as pui]
            [hyperfiddle.ui.codemirror :as cm]
            #?(:cljs [user.router :as html5-router])
            [contrib.ednish :as ednish]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec])
  #?(:cljs (:require-macros wip.hfql2)))

;; (p/defn NavBar []
;;   (let [route-state (p/watch hf/route-state)]
;;     (dom/div {:class "navbar"}
;;       #_(ui/link (second route-state) hf/navigate-back! (dom/text "< back"))
;;       (dom/div {:class "navbar-route"}
;;         (cm/CodeMirror. {:parent dom/node :inline true} cm/read-edn cm/write-edn (first route-state))))))

(defn route-state->route [route-state]
  (let [[k v] (first route-state)
        args (spec/args (first k))]
    [k (into {} (filter #(keyword? (key %))) v) (first k) (pr-str args) (spec/parse `order)]))

(p/defn Route []
  (let [!steady (atom false)]
    (dom/label "Route state")
    (pui/input {::pui/value       (pr-str (if (p/watch !steady) (p/current hf/route) hf/route))
                ::pui/input-event (p/fn [e] (try (reset! hf/!route-state (list (clojure.edn/read-string (.. e -target -value))))
                                                 (catch js/Error e
                                                   (js/console.log "Error" (.-message e)))))
                ::pui/focus-event (p/fn [e] (reset! !steady true))
                ::pui/blur-event  (p/fn [e] (reset! !steady false))})
    (dom/label "Route")
    (dom/pre (dom/text (route-state->route hf/route)))))

(p/defn Tee-shirt-orders []
  ;; Warning: HFQL is unstable
  (p/client
    (let [route (first (p/watch hf/!route-state))]
      (binding [hf/route route]
        (dom/div
          (Route.)
          (dom/hr)
          (p/server
            (binding [hf/route route]
              (let [route '(wip.orders/orders "alice")]
                (ui2/with-ui-renderers


                  (router/router route
                    {(one-order 9) [:db/id]}
                    {(orders .)
                     [(props :db/id {::hf/link one-order})
                      :order/email

                      {(props :order/gender {::hf/options (genders)})
                       [(props :db/ident {::hf/as gender})]}
                      {(props :order/shirt-size {::hf/options (shirt-sizes gender .)})
                       [:db/ident]}]})  

                  #_(hfql2/hfql
                      [#_{(genders) [:db/ident]}
                       {(orders .) [:order/email
                                    {(props :order/gender {::hf/options (genders)}) [(props :db/ident {::hf/as gender})]}
                                    {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                                               ::hf/option-label :db/ident}) [:db/ident :db/id]}]}]) 
                  )))
            ))))))

;; (p/defn Tee-shirt-orders []
;;   ;; Warning: HFQL is unstable
;;   (p/client
;;     (let [!path   (m/mbx)
;;           route (Route. !path)]
;;       (dom/code (dom/text route))
;;       (p/server
;;         (ui2/with-ui-renderers
;;           (hfql2/hfql
;;             [#_{(genders) [:db/ident]}
;;              {(orders .) [:order/email
;;                           {(props :order/gender {::hf/options (genders)}) [(props :db/ident {::hf/as gender})]}
;;                           {(props :order/shirt-size {::hf/options (shirt-sizes gender .)
;;                                                      ::hf/option-label :db/ident}) [:db/ident :db/id]}]}]) 
;;           )
;;         ))))

(p/defn Browser []
  (p/client
    (dom/div {::dom/id "main"
              ::dom/class "browser hyperfiddle-hfql"}
      (dom/div {::dom/class "view"}
        (p/server
          (Tee-shirt-orders.)
          )))))

(p/defn App []
  (binding [hf/db hf/*db* ; why
            hf/Render ui2/Render] ; remove for livecoding demo
    (ui/with-spec-render
      (Browser.))))

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
