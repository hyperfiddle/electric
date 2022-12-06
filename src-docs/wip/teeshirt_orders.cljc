(ns wip.teeshirt-orders
  (:require contrib.ednish
            clojure.edn
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [missionary.core :as m]
            [contrib.ednish :as ednish]
            [clojure.spec.alpha :as s]
            #?(:cljs [hyperfiddle.router :as html5-router]))
  #?(:cljs (:require-macros [wip.teeshirt-orders])))

(defn path-hash [path]
  (when (clojure.string/includes? path "#")
    (not-empty (last (clojure.string/split path #"#" 2)))))

(defn decode-route [route] ; TODO Fishy. Maybe the HTML5 path value should not be an encoded sexpr. Maybe "domain.com/ns!fn/arg0#{state…}"?
  (cond (nil? route)    nil
        (map? route)    (hyperfiddle.walk/prewalk (fn [form] ; HACK ugly
                                                    (if (and (map-entry? form) (vector? (key form)))
                                                      [(seq (key form)) (val form)]
                                                      form))
                          route)
        (vector? route) (seq route)
        :else           (throw (ex-info "A route should be a sexpr or a map" {:route route}))))

#?(:cljs
   (defn route> [!path]
     (->> (html5-router/path> !path)
       (m/eduction (map path-hash) (map ednish/decode-uri) (map decode-route))
       (m/reductions {} nil)
       (m/relieve {}))))

(defn names [] ["alice" "bob" "charlie"])
(s/fdef names :ret (s/coll-of names))

(p/defn Page []
  (p/client
    (binding [ttgui/GridWidth 6] ; TODO auto compute grid width from HFQL expression
      (let [stage (p/server
                    (hf/hfql
                      {(props (wip.orders-datascript/orders (props . {::hf/options (names)}))
                         {::hf/height 3})
                       [:db/id
                        (props :order/email {::hf/tx (p/fn [{::hf/keys [entity attribute]} v] [:db/add entity attribute v])})
                        {(props :order/gender {::hf/options (wip.orders-datascript/genders)})
                         [#_:db/id
                          (props :db/ident {::hf/as gender})]}
                        (props :order/tags {#_#_::hf/render (p/fn [{::hf/keys [Value]}]
                                                              (let [v (Value.)]
                                                                (p/client (dom/pre {:style {:grid-row ttgui/GridRow, :grid-column ttgui/GridCol}}
                                                                            (pr-str (into [] v))))))
                                            #_#_::hf/render2 (dom/pre (pr-str (map smart-identity order/tags)))})
                        {(props :order/shirt-size {::hf/options (wip.orders-datascript/shirt-sizes gender .)})
                         [:db/ident]}]}) )]
        (dom/pre stage))))
  )

(p/defn App []
  (p/client
    (dom/h1 "HFQL as a grid")
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    (let [!path (m/mbx)]
      (binding [hf/route          (or (new (route> !path)) '(wip.orders-datascript/orders ""))
                hf/navigate!      #(html5-router/pushState! !path (str "#" (ednish/encode-uri %)))
                hf/replace-route! #(html5-router/replaceState! !path (str "#" (ednish/encode-uri %)))
                hf/navigate-back! #(.back js/window.history)
                hf/db-name        "$"]
        (let [route hf/route]
          (p/server
            (binding
                [hf/db       hf/*$*
                 hf/*schema* wip.orders-datascript/schema
                 hf/*nav!*   wip.orders-datascript/nav!
                 hf/route    route]
              (ttgui/with-gridsheet-renderer
                (dom/style {:grid-template-columns "repeat(6, 1fr)"})
                (p/server (Page.)))))
          nil)))))



;; [hf/*$* hf/db
;;  hf/*schema* hf/*schema*
;;  hf/*nav!* hf/*nav!*]
