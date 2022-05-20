(ns wip.hytradboi
  "wip - unvalidated, may be broken"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render)
  (:import (hyperfiddle.photon Pending Remote))
  #?(:cljs (:require-macros wip.hytradboi)))

(p/defn App []
  ;(binding [hf/Render ui/Render]) ; broken
  (hf/hfql
    {(orders .)
     [:order/email
      {(props :order/gender {::hf/options (genders)})
       [:db/ident]}
      {(props :order/shirt-size {::hf/options (shirt-sizes order/gender .)})
       [:db/ident]}]}))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (dom/div
                     (dom/attribute "id" "main")
                     (dom/class "browser")
                     (dom/div
                       (dom/class "view")
                       (codemirror/edn.
                         ~@(ui/with-spec-render (App.))))))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main)))
  )
