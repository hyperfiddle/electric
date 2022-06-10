(ns wip.hfql-links
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes one-order]]
            [hyperfiddle.hfql.router :as router])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.hfql-links)))

#?(:cljs (reset! hf/route-state `((wip.orders/orders "alice")))) ;; set initial (client-side) route state

(p/defn App []
  (dom/div {:id "main", :class "browser"}
   (let [[current prev] (p/watch hf/route-state)]
     (dom/div {:class "view"}
      (dom/button {:disabled (not (some? prev))
                   :style    {:grid-row 1, :justify-self :flex-start}}
             (dom/text (str "< " (some-> (first prev) name)))
             (new (dom/events "click" (map hf/navigate-back!))))
      (let [route current]
        ~@(router/router ;; router is on the server
           route
           {(one-order sub) [:db/id :order/email]}
           {(orders .) [(props :db/id {::hf/link one-order})
                        :order/email
                        {(props :order/gender
                                {::hf/options      (genders)
                                 ::hf/option-label :db/ident
                                 ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
                        {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                                   ::hf/option-label :db/ident}) [:db/ident]}]}))
      ))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            ~@(binding [hf/db     hf/*db*
                                                        hf/Render ui/Render]
                                                (ui/with-spec-render
                                                  ~@(App.))))
                                          (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
