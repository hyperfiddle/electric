(ns wip.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render)
  (:import (hyperfiddle.photon Pending Remote))
  #?(:cljs (:require-macros wip.demo-hfql)))


(p/defn Orders []
  ; Warning: HFQL is unstable
  ~@(hf/hfql
      {(orders .)
       [:order/email
        {(props :order/gender {::hf/options      (genders)
                               ::hf/option-label :db/ident
                               ::hf/render       ui/select-options})
         [:db/ident]}
        {(props :order/shirt-size {::hf/options      (shirt-sizes order/gender .)
                                   ::hf/option-label :db/ident
                                   ::hf/render       ui/select-options})
         [:db/ident]}]}))

(p/defn App []
  (dom/div
    (dom/attribute "id" "main")
    (dom/class "browser")
    (dom/div
      (dom/class "view")
      (let [tx (Orders.)]
        (codemirror/edn. tx)))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            ~@(binding [hf/db     hf/*db* ; why
                                                        hf/Render ui/Render]
                                                (ui/with-spec-render
                                                  ~@(App.))))
                                          (catch Pending _)
                                          (catch Remote _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
