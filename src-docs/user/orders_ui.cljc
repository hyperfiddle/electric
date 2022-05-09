(ns user.orders-ui
  "todo modernize"
  (:require [hyperfiddle.api :as hf]
            #?(:clj [hyperfiddle.hfql :refer [hfql]])
            [hyperfiddle.photon :as p]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]]
            #?(:cljs [hyperfiddle.client :refer [client]])
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [hyperfiddle.hfql :refer [hfql]])))

(p/defn Orders []
  ~@(ui/with-spec-render
      (binding [hf/db     hf/*db*
                hf/render ui/render]
        (hfql
         {(orders .)
          [:order/email
           {(props :order/gender {::hf/options      (genders)
                                  ::hf/option-label :db/ident
                                  ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
           {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                      ::hf/option-label :db/ident
                                      ::hf/render       ui/select-options}) [:db/ident]}]}))))

#?(:cljs
   (def ^:export entrypoint
     (client
      (p/main
       (binding [dom/parent (dom/by-id "root")]
         (dom/div
          (dom/attribute "id" "main")
          (dom/class "browser")
          (dom/div
           (dom/class "view")
           (new user.orders-ui/Orders))))))))