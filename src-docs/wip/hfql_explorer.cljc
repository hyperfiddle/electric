(ns wip.hfql-explorer
  (:require contrib.ednish
            clojure.edn
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.explorer :as explorer]
            [missionary.core :as m]
            [contrib.ednish :as ednish]
            [clojure.spec.alpha :as s]
            #?(:cljs [hyperfiddle.router :as html5-router]))
  #?(:cljs (:require-macros [wip.hfql-explorer])))

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

(p/defn App []
  (p/client
    (dom/h1 "HFQL as a grid")
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    (dom/div {:class "photon-demo-explorer"}
      (let [!path (m/mbx)]
        (binding [hf/route          (or (new (route> !path)) '(wip.orders-datascript/orders ""))
                  hf/navigate!      #(html5-router/pushState! !path (str "#" (ednish/encode-uri %)))
                  hf/replace-route! #(html5-router/replaceState! !path (str "#" (ednish/encode-uri %)))
                  hf/navigate-back! #(.back js/window.history)
                  ]
          (let [route hf/route]
            (p/server
              (binding
                  [hf/db       hf/*$*
                   hf/*schema* wip.orders-datascript/schema
                   hf/*nav!*   wip.orders-datascript/nav!
                   hf/route    route]
                (let [!needle (atom "")
                      needle  (p/watch !needle)
                      query   (hf/hfql #_[hf/*$* hf/db
                                          hf/*schema* hf/*schema*
                                          hf/*nav!* hf/*nav!*]
                                {(wip.orders-datascript/orders (props . {::hf/options (names)}))
                                 [:db/id
                                  (props :order/email {::hf/tx (fn [ctx] (prn "tx:" ctx))})
                                  {(props :order/gender {::hf/summarize (p/fn [v] (name (:db/ident v)))
                                                         ::hf/options   (wip.orders-datascript/genders)})
                                   [(props :db/ident {::hf/as gender})]}
                                  :order/tags
                                  {(props :order/shirt-size {::hf/summarize (p/fn [v] (name (:db/ident v)))
                                                             ::hf/options   (wip.orders-datascript/shirt-sizes gender .)})
                                   [:db/ident]}]})]
                  (explorer/ExplorerWithUI.
                    {::explorer/columns                5
                     ::explorer/page-size              15
                     ::explorer/row-height             24
                     ::gridsheet/grid-template-columns "8rem repeat(4,1fr)"}
                     query))))))))))

