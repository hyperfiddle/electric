(ns user.hytradboi
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [hyperfiddle.ui.codemirror :as codemirror]
            [hyperfiddle.ui :as ui]
            [user.orders :refer [orders genders shirt-sizes]]
            dustin.y2022.edn-render))

#?(:clj (def db @(requiring-resolve 'dev/db)))

(p/defn App []
  (binding [hf/render ui/render] ; default to dom so issues show up in test
    (hf/hfql
      {(orders .)
       [:order/email
        {(props :order/gender {::hf/options (genders)})
         [:db/ident]}
        {(props :order/shirt-size {::hf/options (shirt-sizes order/gender .)})
         [:db/ident]}]})))

(def main                                 ; http://localhost:8080/#user.hytradboi
  (p/client
    (p/main
      (binding [dom/parent (dom/by-id "root")]
        (dom/div
          (dom/attribute "id" "main")
          (dom/class "browser")
          (dom/div
            (dom/class "view")
            (codemirror/edn.
              ~@#_"server"
                (binding [hf/db hf/*db*]
                  (ui/with-spec-render (App.))))))))))

(def ^:export reactor)

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))

(defn ^:dev/after-load ^:export start! []
  (set! reactor (main js/console.log js/console.error)))

(comment tests
  (def !x (atom "alice"))
  (with (p/run (! (App. (p/Watch !x))))

    % := '{(user.orders/orders _)
           [{:order/gender     #:db{:ident :order/female},
             :order/email      "alice@example.com",
             :order/shirt-size _}]}

    (reset! !x "bob")

    % := '{(user.orders/orders _)
           [{:order/gender     #:db{:ident :order/male},
             :order/email      "bob@example.com",
             :order/shirt-size _}]}))
