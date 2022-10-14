(ns wip.hfql2 ; adapted from user.demo-hfql
  "wip, unstable"
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql2]
            [hyperfiddle.hfql2.ui :as ui2]
            [hyperfiddle.hfql2.router :as router]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [order orders genders shirt-sizes one-order]]
            [hyperfiddle.photon-ui :as pui]
            [hyperfiddle.ui.codemirror :as cm]
            #?(:cljs [hyperfiddle.router :as html5-router])
            [contrib.ednish :as ednish]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec]
            [clojure.datafy :refer [datafy]])
  #?(:cljs (:require-macros wip.hfql2)))

;; (p/defn NavBar []
;;   (let [route-state (p/watch hf/route-state)]
;;     (dom/div {:class "navbar"}
;;       #_(ui/link (second route-state) hf/navigate-back! (dom/text "< back"))
;;       (dom/div {:class "navbar-route"}
;;         (cm/CodeMirror. {:parent dom/node :inline true} cm/read-edn cm/write-edn (first route-state))))))



(defn set-route-in-route-state [route-state route]
  (when (not-empty route-state)
    (let [args (zipmap (::spec/keys (datafy (spec/args (first route)))) (rest route))
          [k v] (first route-state)]
      (assoc route-state k (merge v args)))))

(p/defn Route []
  (dom/label "Route")
  (let [!steady (atom false)
        route   hf/route]
    (pui/input {::pui/value       (pr-str (if (p/watch !steady) (p/current route) route))
                ::pui/input-event (p/fn [e] (try (html5-router/replaceState! hf/!path (str "#" (pr-str (clojure.edn/read-string (.. e -target -value)))))
                                                 (.setCustomValidity dom/node "")
                                                 (catch js/Error e
                                                   (.setCustomValidity dom/node (.-message e))
                                                   (.reportValidity dom/node))))
                ::pui/focus-event (p/fn [e] (reset! !steady true))
                ::pui/blur-event  (p/fn [e] (reset! !steady false))}))
  #_(dom/label "Ednish route state")
  #_(dom/pre (dom/text (ednish/encode (pr-str hf/route))))
  #_(dom/label "Ednish route state - uri decoded")
  #_(dom/pre (dom/text (ednish/decode-uri (ednish/encode-uri hf/route)))))

(defn path-hash [path]
  (when (clojure.string/includes? path "#")
    (not-empty (last (clojure.string/split path #"#" 2)))))

(defn decode-route [route]
  (prn "route" route)
  (cond (nil? route)    nil
        (map? route)    (hyperfiddle.walk/prewalk (fn [form]
                                                    (if (and (map-entry? form) (vector? (key form)))
                                                      [(seq (key form)) (val form)]
                                                      form))
                          route)
        (vector? route) (seq route)
        :else           (throw (ex-info "A route should be a sexpr or a map" {:route route}))))

#?(:cljs
   (defn route> [!path]
     (->> (html5-router/path> !path)
       (missionary.core/eduction (map path-hash) (map ednish/decode-uri) (map decode-route))
       (missionary.core/reductions {} nil)
       (missionary.core/relieve {}))))

(p/defn Tee-shirt-orders []
  ;; Warning: HFQL is unstable
  (p/client
    (binding [hf/!path (m/mbx)]
      (binding [hf/route (new (route> hf/!path))]
        (dom/div
          (Route.)
          (dom/hr)
          (let [route hf/route]
            (p/server
              (ui2/with-ui-renderers
                (router/router route
                  {(one-order .) [(props :db/id {::hf/link (one-order db/id)})
                                  (props :order/email {::hf/link (orders order/email)
                                                       ::hf/render ui2/Default-renderer})

                                  #_{(props :order/gender {::hf/options (genders)})
                                     [(props :db/ident {::hf/as gender})]}
                                  {:order/gender [(props :db/ident {::hf/as gender})]}
                                  {(props :order/shirt-size {::hf/options (shirt-sizes gender .)})
                                   [:db/ident]}]} 
                  {(orders .)
                   [(props :db/id {::hf/link (one-order db/id)})
                    :order/email

                    {(props :order/gender {::hf/options (genders)})
                     [(props :db/ident {::hf/as gender})]}
                    {(props :order/shirt-size {::hf/options (shirt-sizes gender .)})
                     [:db/ident]}]})  

                #_(hfql2/hfql
                    [#_{(genders) [:db/ident]}
                     {(orders .) [:order/email
                                  {(props :order/gender {::hf/options (genders)}) [(props :db/ident {::hf/as gender})]}
                                  {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                                             ::hf/option-label :db/ident}) [:db/ident :db/id]}]}]) 
                )
              )))))))

;; (p/defn Tee-shirt-orders []
;;   ;; Warning: HFQL is unstable
;;   (p/client
;;     (let [!path   (m/mbx)
;;           route (Route. !path)]
;;       (dom/code (dom/text route))
;;       (p/server
;;         (ui2/with-ui-renderers
;;           (hfql2/hfql
;;             [#_{(genders) [:db/ident]}
;;              {(orders .) [:order/email
;;                           {(props :order/gender {::hf/options (genders)}) [(props :db/ident {::hf/as gender})]}
;;                           {(props :order/shirt-size {::hf/options (shirt-sizes gender .)
;;                                                      ::hf/option-label :db/ident}) [:db/ident :db/id]}]}]) 
;;           )
;;         ))))

(p/defn Browser []
  (p/client
    (dom/div {::dom/id "main"
              ::dom/class "browser hyperfiddle-hfql"}
      (dom/div {::dom/class "view"}
        (p/server
          (Tee-shirt-orders.)
          )))))

(p/defn App []
  (binding [hf/db hf/*db* ; why
            hf/Render ui2/Render] ; remove for livecoding demo
    (ui/with-spec-render
      (Browser.))))

; Takeaways:
; 1. no REST, no GraphQL, all client/server network management handled automatically. Eliminates BFF problem
; 2. simple to understand, easy to use, unified programming model (multiplier for sr devs, and makes jr devs useful)
; 3. A consequence of this is it permits a data driven approach that enables us to build a low-code GUI on top of it.
