(ns wip.hfql ; adapted from user.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.ui :as hfui]
            [hyperfiddle.hfql.router :as router]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [wip.orders :refer [orders genders shirt-sizes one-order]]
            [hyperfiddle.photon-ui :as ui]
            #?(:cljs [hyperfiddle.router :as html5-router])
            [missionary.core :as m]
            [contrib.ednish :as ednish])
  #?(:cljs (:require-macros wip.hfql)))

(p/defn Route []
  (dom/label "Route")
  (let [!steady (atom false)
        route   hf/route]
    (ui/input {::ui/value       (pr-str (if (p/watch !steady) (p/current route) route))
               ::ui/input-event (p/fn [e] (try (hf/replace-route! (clojure.edn/read-string (.. e -target -value)))
                                               (.setCustomValidity dom/node "")
                                               (catch js/Error e
                                                 (.setCustomValidity dom/node (.-message e))
                                                 (.reportValidity dom/node))))
               ::ui/focus-event (p/fn [e] (reset! !steady true))
               ::ui/blur-event  (p/fn [e] (reset! !steady false))})))

(p/defn Tee-shirt-orders []
  ;; Warning: HFQL is unstable
  (p/client
    (dom/div
      (Route.)
      (dom/hr)
      (let [route hf/route]
        (p/server
          (hfui/with-ui-renderers
            (router/router route
              {(one-order .) [(props :db/id {::hf/link (one-order db/id)})
                              (props :order/email {::hf/link   (orders order/email)
                                                   ::hf/render hfui/Default-renderer})

                              {(props :order/gender {::hf/options (genders)})
                               [(props :db/ident {::hf/as gender})]}
                              {(props :order/shirt-size {::hf/options (shirt-sizes gender .)})
                               [:db/ident]}]}
              {(orders .)
               [(props :db/id {::hf/link (one-order db/id)})
                :order/email

                {(props :order/gender {::hf/options (genders)})
                 [(props :db/ident {::hf/as gender})]}
                {(props :order/shirt-size {::hf/options (shirt-sizes gender .)})
                 [:db/ident]}]})

            ))))))

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
    (let [!path (m/mbx)]
      (binding [hf/route          (or (new (route> !path)) '(wip.orders/orders ""))
                hf/navigate!      #(html5-router/pushState! !path (str "#" (ednish/encode-uri %)))
                hf/replace-route! #(html5-router/replaceState! !path (str "#" (ednish/encode-uri %)))
                hf/navigate-back! #(.back js/window.history)
                hf/db-name        "$"   ; enrich UI with db info
                ]
        (dom/div {::dom/id    "main"
                  ::dom/class "browser hyperfiddle-hfql"}
          (dom/div {::dom/class "view"}
            (p/server
              (Tee-shirt-orders.)
              )))))))

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
