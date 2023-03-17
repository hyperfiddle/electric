(ns wip.teeshirt-orders
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            #?(:clj user.example-datascript-db)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql-tree-grid :as ttgui]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.history :as router]
            wip.orders-datascript
            [clojure.string :as str]))

(defn names [needle] (filter #(str/includes? % needle) ["" "alice" "bob" "charlie"]))
(s/fdef names :args (s/cat :needle string?) :ret (s/coll-of names))

;; TODO option-label shouldn't get nil as value
(e/defn IdentName [v] (some-> (:db/ident v) name))
(e/defn Tx [ctx v] (hf/Transact!. [[:db/add (hf/entity ctx) (hf/attribute ctx) v]]))

(e/defn OneOrderPage [order]
  (ttgui/with-gridsheet-renderer
    (dom/style {:grid-template-columns "1fr 1fr"})
    (binding [ttgui/grid-width 2] ; TODO auto compute grid width from HFQL expression
      (e/server
        (hf/hfql
          [hf/*$*      hf/db
           hf/*schema* hf/*schema*
           hf/*nav!*   hf/*nav!*]
          {order
           [(props :db/id {#_#_::hf/link ['wip.orders-datascript/one-order %]})
            (props :order/email {::hf/tx (e/fn [ctx v] [[:db/add (hf/entity ctx) (hf/attribute ctx) v]])})
            {(props :order/gender {::hf/options      (wip.orders-datascript/genders)
                                   ::hf/option-label IdentName
                                   ::hf/tx Tx})
             [:db/id (props :db/ident {::hf/as gender})]}
            {(props :order/shirt-size {::hf/options      (wip.orders-datascript/shirt-sizes gender .)
                                       ::hf/option-label IdentName
                                       ::hf/tx           Tx})
             [#_:db/id
              :db/ident]}
            :order/tags
            ]})
        nil ))))

(e/defn OrdersPage []
  (ttgui/with-gridsheet-renderer
    (dom/style {:grid-template-columns "repeat(6, 1fr)"})
    (binding [ttgui/grid-width 6] ; TODO auto compute grid width from HFQL expression
      (e/server
        (hf/hfql
          {(props (wip.orders-datascript/orders (props . {::hf/options (names .)}))
             {::hf/height 3})
           [(props :db/id {::hf/link ['wip.orders-datascript/one-order %]})
            (props :order/email {::hf/tx Tx})
            :order/email      ; duplicate, readonly, for checking the loop
            {(props :order/gender {::hf/options      (wip.orders-datascript/genders)
                                   ::hf/option-label IdentName
                                   ::hf/tx           Tx})
             [#_:db/id
              (props :db/ident {::hf/as gender})]}
            :order/tags
            {(props :order/shirt-size {::hf/options      (wip.orders-datascript/shirt-sizes gender .)
                                       ::hf/option-label IdentName
                                       ::hf/tx Tx})
             [:db/ident]}
            ]})))))

(e/defn Webview-HFQL []
  (e/client
    (dom/h1 (dom/text "Teeshirt orders"))
    (dom/pre (dom/text (contrib.str/pprint-str router/route)))
    (binding [hf/db-name "$"
              router/build-route (fn [[self state _route] route'] `[~self ~state ~route'])]
      (e/server
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
            (e/client
              (let [[_self state local-route] router/route
                    [page x & args] (or local-route `(wip.orders-datascript/orders))]
                (router/router 1
                  (case page
                    wip.orders-datascript/orders    (OrdersPage.)
                    wip.orders-datascript/one-order (OneOrderPage. x)
                    (dom/h2 (dom/text "Page not found"))))))
            (e/client
              (dom/element "style" (dom/text ".dustin-stage { display: block; width: 100%; height: 10em; }"))
              (ui/edn (e/server hf/stage) false (dom/props {:disabled true :class "dustin-stage"}))))))
      nil)))

