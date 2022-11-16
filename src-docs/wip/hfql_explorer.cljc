(ns wip.hfql-explorer
  (:require contrib.ednish
            clojure.edn
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.explorer :as explorer]
            [hyperfiddle.hfql2 :refer [hfql]]
            [hyperfiddle.hfql2.explorer :as hfql-explorer]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2.ui :as ui]
            [missionary.core :as m]
            [contrib.ednish :as ednish]
            #?(:cljs [hyperfiddle.router :as html5-router]))
  #?(:cljs (:require-macros [wip.hfql-explorer])))

(p/defn Email [V]
  (let [v (V.)]
    (p/client
      (dom/a {:href (str "mailto:" v)} (dom/text v))
      nil)))

(p/defn Input [V] (let [v (V.)]
                    (ui/Input. v (meta V)))
  nil)

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
                  [explorer/cols [:a :b :c :d :e]
                   hf/db         hf/*$*
                   hf/*schema*   wip.orders-datascript/schema
                   hf/*nav!*     wip.orders-datascript/nav!
                   hf/route      route]
                (let [!needle (atom "")
                      needle  (p/watch !needle)]
                  (hfql-explorer/Explorer.
                    {::dom/style                       {:height "calc((30 + 1) * 24px)"}
                     ::explorer/page-size              30
                     ::explorer/row-height             24
                     ::gridsheet/grid-template-columns "16rem 1fr 1fr 1fr 1fr"}
                    (hfql #_[hf/*$* hf/db
                             hf/*schema* hf/*schema*
                             hf/*nav!* hf/*nav!*]
                      {(props (wip.orders-datascript/orders .) {::hf/render hfql-explorer/FormsTransposedToRows})
                       [:db/id
                        (props :order/email {::hf/render Input
                                             ::hf/tx     (fn [v] (prn "tx:" v))})
                        {(props :order/gender {::hf/summarize (p/fn [v] (name (:db/ident v)))
                                               ::hf/options (wip.orders-datascript/genders)})
                         [(props :db/ident {::hf/as gender})]}
                        :order/tags
                        {(props :order/shirt-size {::hf/summarize (p/fn [v] (name (:db/ident v)))
                                                   ::hf/options (wip.orders-datascript/shirt-sizes gender .)})
                         [:db/ident]}]}) ))))))))))

