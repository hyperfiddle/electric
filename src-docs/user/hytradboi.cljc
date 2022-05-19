(ns user.hytradboi
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render)
  (:import (hyperfiddle.photon Pending)))

(p/defn App []
  (binding [] ; default to dom so issues show up in test
    (hf/hfql
      {(orders .)
       [:order/email
        {(props :order/gender {::hf/options (genders)})
         [:db/ident]}
        {(props :order/shirt-size {::hf/options (shirt-sizes order/gender .)})
         [:db/ident]}]})))

(def main                                 ; http://localhost:8080/
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
                         ~@#_"server"
                           (binding []
                             (ui/with-spec-render (App.)))))))
                 (catch Pending _))))))
