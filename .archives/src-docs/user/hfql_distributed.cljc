(ns user.hfql-distributed
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.hfql :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :as router]
            [clojure.edn :as edn])
  #?(:cljs (:require-macros [user.hfql-distributed :refer [page query]]
                            [user.gender-shirt-size :refer [submissions genders shirt-sizes sub-profile]]
                            [hyperfiddle.hfql :refer [hfql]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?


(p/defn query []
  (binding [hf/db "$"]
    (router/router {(sub-profile 9) [:db/id]}
                   {(submissions .) [(props :db/id {::hf/link sub-profile})
                                     :dustingetz/email
                                     {(props :dustingetz/gender {::hf/options      (genders)
                                                                 ::hf/option-label :db/ident
                                                                 ::hf/render       ui/typeahead}) [(props :db/ident {::hf/as gender})]}
                                     {(props :dustingetz/shirt-size {::hf/options (shirt-sizes gender .)
                                                                     ::hf/option-label :db/ident
                                                                     #_#_::hf/render        ui/typeahead}) [:db/ident]}]}) 
    ))

(p/defn page []
  ~@(binding [hf/route (atom `(submissions "alice"))]
      (let [route ~(m/eduction (dedupe) (m/watch hf/route))]
        ~@
        (dom/div (dom/class "distribution")
                 ;; (dom/p (dom/text (str  "Render MODE " hyperfiddle.q6/render-mode)))

                 (let [go!       (dom/button (dom/text "Go!")
                                             ~(->> (dom/events dom/parent dom/click-event)
                                                   (m/eduction (map (constantly true)))
                                                   (m/reductions {} true)
                                                   (m/relieve {})))
                       route-str (p/$ ui/input {:dom.property/value (pr-str route)
                                                :dom.attribute/type "text"
                                                :dom.property/style {"width" "100%"}}
                                      dom/target-value)
                       route     (edn/read-string route-str)]
                   (hyperfiddle.logger/info "route" route)
                   ;; (js/console.log route)
                   (when go!
                     (dom/div
                      ;; (dom/text)
                      ~@
                      (p/$ ui/with-spec-render
                           #'(binding [hf/Render            ui/Render
                                       #_#_router/not-found #'~@(dom/h1 (dom/text "Page not found"))]
                               (prn "route:" route)
                               (reset! hf/route route)
                               (p/$ query))))))))))

(def exports (p/vars vector? map-indexed pr-str get last get-in clojure.core/reset! atom second
                     router/validate-route!
                     dedupe))

