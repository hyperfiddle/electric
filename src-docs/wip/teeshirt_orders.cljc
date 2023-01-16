(ns wip.teeshirt-orders
  (:require datascript.core
            dev
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [hyperfiddle.photon-ui4 :as ui4]
            [hyperfiddle.router :as router]
            wip.orders-datascript
            [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [wip.teeshirt-orders])))

(defn names [] ["alice" "bob" "charlie"])
(s/fdef names :ret (s/coll-of names))

(p/defn OneOrderPage [order]
  (ttgui/with-gridsheet-renderer
    (dom/style {:grid-template-columns "1fr 1fr"})
    (binding [ttgui/grid-width 2] ; TODO auto compute grid width from HFQL expression
      (p/server
        (hf/hfql
          [hf/*$*      hf/db
           hf/*schema* hf/*schema*
           hf/*nav!*   hf/*nav!*]
          {order
           [(props :db/id {#_#_::hf/link ['wip.orders-datascript/one-order %]})
            (props :order/email {::hf/tx (p/fn [{::hf/keys [entity attribute]} v] [[:db/add entity attribute v]])})
            {(props :order/gender {::hf/options      (wip.orders-datascript/genders)
                                   ::hf/option-label (p/fn [v] (name (:db/ident v)))})
             [:db/id
              (props :db/ident {::hf/as gender})]}
            {(props :order/shirt-size {::hf/options      (wip.orders-datascript/shirt-sizes gender .)
                                       ::hf/option-label (p/fn [v] (name (:db/ident v)))
                                       ::hf/tx           (p/fn [{::hf/keys [entity attribute]} v] [[:db/add entity attribute v]])})
             [#_:db/id
              :db/ident]}
            :order/tags
            ]})
        nil ))))

(p/defn OrdersPage []
  (ttgui/with-gridsheet-renderer
    (dom/style {:grid-template-columns "repeat(6, 1fr)"})
    (binding [ttgui/grid-width 6] ; TODO auto compute grid width from HFQL expression
      (p/server
        (hf/hfql
          {(props (wip.orders-datascript/orders (props . {::hf/options (names)}))
             {::hf/height 3})
           [(props :db/id {::hf/link ['wip.orders-datascript/one-order %]})
            (props :order/email {::hf/tx (p/fn [{e ::hf/entity, a ::hf/attribute} v] [[:db/add e a v]])})
            :order/email      ; duplicate, readonly, for checking the loop
            {(props :order/gender {::hf/options      (wip.orders-datascript/genders)
                                   ::hf/option-label (p/fn [v] (name (:db/ident v)))
                                   ::hf/tx           (p/fn [{::hf/keys [entity attribute]} v] [[:db/add entity attribute v]])})
             [#_:db/id
              (props :db/ident {::hf/as gender})]}
            :order/tags
            {(props :order/shirt-size {::hf/options      (wip.orders-datascript/shirt-sizes gender .)
                                       ::hf/option-label (p/fn [v] (name (:db/ident v)))})
             [:db/ident]}
            ]})))))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "HFQL as a grid"))
    (binding [hf/db-name "$"
              hf/Link    (p/fn [[page eid] _] (router/Link. [page eid] eid) nil)]
      (p/server
        (binding
            [hf/db           hf/*$*
             hf/*schema*     wip.orders-datascript/schema
             hf/*nav!*       wip.orders-datascript/nav!
             ;; hf/schema (new (dx/schema> secure-db))
             hf/into-tx'     (fn [schema tx0 tx] (concat tx0 tx))
             hf/with         (fn [db tx]  ; inject datomic
                               (try (:db-after (datascript.core/with db tx))
                                    (catch Exception e (println "...failure, e: " e))))]
          (hf/branch
            (p/client
              (let [[page & args] (::hf/route hf/route `(wip.orders-datascript/orders))]
                (case page
                  wip.orders-datascript/orders    (OrdersPage.)
                  wip.orders-datascript/one-order (let [[sub] args]
                                                    (OneOrderPage. sub))
                  (dom/h2 (dom/text "Page not found")))))
            (p/client
              (dom/hr)
              (dom/element "style" (str "dustin-stage" " { display: block; width: 100%; height: 10em; }"))
              (ui4/edn (p/server hf/stage) false (dom/props {::dom/disabled true ::dom/class "dustin-stage"}))))))
      nil)))

