(ns wip.teeshirt-orders
  (:require contrib.ednish
            clojure.edn
            datascript.core
            dev
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [hyperfiddle.photon-ui3 :as ui3]
            [missionary.core :as m]
            wip.orders-datascript
            [contrib.ednish :as ednish]
            [clojure.spec.alpha :as s]
            [hyperfiddle.router :as router]
            [clojure.string :as str])
  #?(:cljs (:require-macros [wip.teeshirt-orders])))

#?(:cljs (defn encode-path [route] (->> route pr-str ednish/encode (str "/"))))

(defn decode-route [route] ; TODO Fishy. Maybe the HTML5 path value should not be an encoded sexpr. Maybe "domain.com/ns!fn/arg0#{state…}"?
  (cond (nil? route)    nil
        (map? route)    (hyperfiddle.walk/prewalk (fn [form] ; HACK ugly
                                                    (if (and (map-entry? form) (vector? (key form)))
                                                      [(seq (key form)) (val form)]
                                                      form))
                          route)
        (vector? route) (seq route)
        :else           (throw (ex-info "A route should be a sexpr or a map" {:route route}))))

#?(:cljs (defn decode-path [path read-edn-str]
           {:pre [(string? path) (some? read-edn-str)]}
           (if (= path "/")
             ['wip.orders-datascript/orders]
             (let [path (if (str/starts-with? path "/") (subs path 1) path)]
               (decode-route (contrib.ednish/decode-uri path))))))

(defn parse-route [route]
  (cond (map? route) (hf/route-state->route route)
        :else        route))

(defn names [] ["alice" "bob" "charlie"])
(s/fdef names :ret (s/coll-of names))

(p/defn OrdersPage []
  (let [stage-from-hfql-ignored
        (ttgui/with-gridsheet-renderer
          (dom/style {:grid-template-columns "repeat(6, 1fr)"})
          (binding [ttgui/grid-width 6] ; TODO auto compute grid width from HFQL expression
            (p/server
              (hf/hfql
                {(props (wip.orders-datascript/orders (props . {::hf/options (names)}))
                   {::hf/height 3})
                 [(props :db/id {::hf/link ['wip.orders-datascript/one-order db/id]})
                  (props :order/email {::hf/tx (p/fn [{e ::hf/entity, a ::hf/attribute} v] [[:db/add e a v]])})
                  :order/email          ; duplicate, readonly, for checking the loop
                  {(props :order/gender {::hf/options      (wip.orders-datascript/genders)
                                         ::hf/option-label (p/fn [v] (name (:db/ident v)))
                                         ::hf/tx           (p/fn [{::hf/keys [entity attribute]} v] [[:db/add entity attribute v]])})
                   [#_:db/id
                    (props :db/ident {::hf/as gender})]}
                  :order/tags
                  {(props :order/shirt-size {::hf/options      (wip.orders-datascript/shirt-sizes gender .)
                                             ::hf/option-label (p/fn [v] (name (:db/ident v)))})
                   [:db/ident]}]}))))]))

(p/defn OneOrderPage [order]
  (let [stage
        (ttgui/with-gridsheet-renderer
          (dom/style {:grid-template-columns "repeat(5, 1fr)"})
          (binding [ttgui/grid-width 5] ; TODO auto compute grid width from HFQL expression
            (p/server
              (hf/hfql
                [hf/*$*      hf/db
                 hf/*schema* hf/*schema*
                 hf/*nav!*   hf/*nav!*]
                {order
                 [(props :db/id {::hf/link ['wip.orders-datascript/one-order %]})
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
                  :order/tags]}) )))]
    (dom/pre stage)))

(p/defn App []
  (p/client
    (dom/h1 "HFQL as a grid")
    (let [!path (m/mbx)
          route (decode-path (router/path !path) hf/read-edn-str)]
      (binding [router/Link (router/->Link. !path encode-path)]
        (binding [hf/route          route
                  hf/navigate!      #(router/pushState! !path (ednish/encode-uri %))
                  hf/replace-route! #(router/replaceState! !path (ednish/encode-uri %))
                  hf/navigate-back! #(.back js/window.history)
                  hf/db-name        "$"
                  hf/Link           (p/fn [[page eid] _] (router/Link. [page eid] eid) nil)]
          (p/server
            (binding
                [hf/db       hf/*$*
                 hf/*schema* wip.orders-datascript/schema
                 hf/*nav!*   wip.orders-datascript/nav!
                 hf/route    route
                 ;hf/schema (new (dx/schema> secure-db))
                 hf/into-tx' (fn [schema tx0 tx] (concat tx0 tx))
                 hf/with (fn [db tx] ; inject datomic
                           (try (:db-after (datascript.core/with db tx))
                                (catch Exception e (println "...failure, e: " e))))]
              (hf/branch
                (p/client
                  (let [[page & args] (parse-route route)]
                    (case page
                      wip.orders-datascript/orders (OrdersPage.)
                      wip.orders-datascript/one-order (let [[sub] args]
                                                        (OneOrderPage. sub))
                      (dom/h2 "Page not found"))))
                (p/client
                  (dom/hr)
                  (dom/element "style" (str "dustin-stage" " { display: block; width: 100%; height: 10em; }"))
                  (ui3/edn-editor (p/server hf/stage) false (dom/props {::dom/disabled true ::dom/class "dustin-stage"}))))))
          nil)))))

