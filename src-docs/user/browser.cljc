(ns user.browser
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui6 :as ui]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.q9 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :as router]
            [clojure.edn :as edn]
            [hyperfiddle.dev.logger :as log])
  #?(:cljs (:require-macros [user.browser :refer [view NavBar NotFoundPage]]
                            [user.gender-shirt-size :refer [submissions genders shirt-sizes sub-profile]]
                            [hyperfiddle.q9 :refer [hfql]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?

(p/defn NavBar []
  (dom/div (dom/class "navbar")
           (let [route-str (p/$ ui/input {:dom.property/value       (str (or ~(m/eduction (dedupe) (m/watch hf/route))
                                                                             `(submissions "alice")))
                                          :dom.attribute/type       "text"
                                          :dom.property/placeholder "(page …)"
                                          :dom.property/class       "route"}
                                dom/target-value)
                 route     (try (edn/read-string route-str)
                                (catch :default t
                                  (hyperfiddle.dev.logger/error t)
                                  nil))
                 go!       (dom/button (dom/class "go-button")
                                       ~(->> (dom/events dom/parent dom/click-event)
                                             ;; (m/eduction (map (constantly true)))
                                             (m/reductions {} false)
                                             (m/relieve {})))]
             (do route
                 (if go!
                   route
                   '(about:homepage))))))

(p/defn NotFoundPage []
  ~@
  (let [route @hf/route]
    (cond
      (= route nil) nil
      :else         (dom/div
                     (dom/h1 (dom/text "Page not found"))
                     (dom/code (pr-str route))))))

(p/defn view []
  ~@(binding [hf/db "$"]
      ~@
      (dom/div (dom/class "browser")
               ;; (dom/p (dom/text (str  "Render  MODE " hyperfiddle.q6/render-mode)))
               (let [route     (p/$ NavBar)
                     #_#_x-ray (dom/input (dom/attribute "type" "checkbox")
                                          ~(->> (dom/events dom/parent "input")
                                                (m/eduction (map dom/target-checked))
                                                (m/reductions {} false)
                                                (m/relieve {})))]
                 (do route ;; force dom node order
                     (dom/div (dom/class "view")
                              ~@
                              (p/$ ui/with-spec-render
                                   #'(binding [hf/render        ui/render #_(if true ui/render ui/spec-renderer)
                                               hf/db            "$"
                                               hf/route         (atom route)
                                               router/not-found NotFoundPage]
                                       (router/router {(sub-profile 9) [:db/id]}
                                                      {(submissions .) [(props :db/id {::hf/link sub-profile})
                                                                        :dustingetz/email
                                                                        {(props :dustingetz/gender {::hf/options      (genders)
                                                                                                    ::hf/option-label :db/ident
                                                                                                    ::hf/render       ui/typeahead}) [(props :db/ident {::hf/as gender})]}
                                                                        {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes gender .)
                                                                                                        ::hf/option-label :db/ident
                                                                                                        #_#_::hf/render   ui/typeahead}) [:db/ident]}]}

                                                      )
                                       ))))))))

(def exports (p/vars vector? map-indexed pr-str get last get-in clojure.core/reset! atom second
                     router/validate-route!
                     dedupe))
