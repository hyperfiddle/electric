(ns user.hfql-distributed
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui4 :as ui]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.q6 :as hfql :refer [hfql]])
            #?(:clj [user.gender-shirt-size :refer [submissions genders shirt-sizes]]))
  #?(:cljs (:require-macros [user.hfql-distributed :refer [page query]]
                            [user.gender-shirt-size :refer [submissions genders shirt-sizes]]
                            [hyperfiddle.q6 :as hfql :refer [hfql]])))

(p/def query
  #'(hfql {(submissions "") [:dustingetz/email
                             {(:dustingetz/gender . ::hf/options (genders)
                                                  ::hf/option-label :db/ident
                                                  ;; ::hf/render ui/typeahead
                                                  )
                              [(:db/ident . ::hf/as gender)]}
                             {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender "")
                                                      ::hf/option-label :db/ident
                                                      ;; ::hf/render ui/typeahead
                                                      )
                              [:db/ident]}]})  )

(p/defn page []
  (dom/div (dom/class "distribution")
           (dom/p (dom/text (str  "Render mode " hyperfiddle.q6/render-mode)))
           (let [go! (dom/button (dom/text "Go!")
                                 ~(->> (dom/events dom/parent dom/click-event)
                                       (m/eduction (map (constantly true)))
                                       (m/reductions {} false)
                                       (m/relieve {})))]
             (when go!
               (dom/div
                ~@
                (p/$ ui/with-spec-render
                     #'(binding [hf/render ui/render]
                         ~query)))))))

(def exports (p/vars vector? map-indexed pr-str get last get-in))
