(ns dustin.y2022.livecoding-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [wip.orders-datascript :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render)
  (:import (hyperfiddle.photon Pending Remote))
  #?(:cljs (:require-macros dustin.y2022.livecoding-hfql)))


(p/defn Orders []
  (dom/h1 (dom/text "hello world")))

#_(codemirror/edn. ~@(orders ""))
#_(codemirror/edn.
    ~@ (hf/hfql
         {(orders .)
          :order/email}))
#_(binding [hf/Render ui/Render])
#_(hf/hfql
    {(orders .)
     [:order/email
      {(props :order/gender {::hf/options      (genders)
                             ::hf/option-label :db/ident
                             ::hf/render       ui/select-options})
       [:db/ident]}
      {(props :order/shirt-size {::hf/options      (shirt-sizes order/gender .)
                                 ::hf/option-label :db/ident
                                 ::hf/render       ui/select-options})
       [:db/ident]}]})

(p/defn App []
  (dom/div {:id    "main"
            :class "browser"}
    (dom/div {:class "view"}
      (Orders.))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            ~@(binding [hf/db     hf/*db* ; why
                                                        hf/Render ui/Render] ; remove for livecoding demo
                                                (ui/with-spec-render
                                                  ~@(App.))))
                                          (catch Pending _)
                                          (catch Remote _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
