(ns user.hfql-distributed
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui5 :as ui]
            [missionary.core :as m]
            [hyperfiddle.q8.lib]
            #?(:clj [hyperfiddle.q8 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders shirt-sizes]])
  #?(:cljs (:require-macros [user.hfql-distributed :refer [page query]]
                            [user.gender-shirt-size :refer [submissions genders shirt-sizes]]
                            [hyperfiddle.q8 :refer [hfql]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?

(p/def query
  #'(binding [hf/db "$"]
      (hfql {(submissions needle) [:dustingetz/email
                               {(:dustingetz/gender . ::hf/options      #'(p/$ genders)
                                                      ::hf/option-label :db/ident
                                                      #_#_::hf/render        ui/typeahead) [(:db/ident . ::hf/as gender)]}
                               {(:dustingetz/shirt-size . ::hf/options      #'(p/$ shirt-sizes gender needle)
                                                          ::hf/option-label :db/ident
                                                          #_#_ ::hf/render        ui/typeahead) [:db/ident]}]}) ))

(p/defn page []
  (dom/div (dom/class "distribution")
           ;; (dom/p (dom/text (str  "Render MODE " hyperfiddle.q6/render-mode)))
           (let [go! (dom/button (dom/text "Go!")
                                 ~(->> (dom/events dom/parent dom/click-event)
                                       (m/eduction (map (constantly true)))
                                       (m/reductions {} true)
                                       (m/relieve {})))]
             (when go!
               (dom/div
                (dom/text
                 ~@
                 (p/$ ui/with-spec-render
                      #'(binding [hf/render ui/render]
                          ~query))))))))

(def exports (p/vars vector? map-indexed pr-str get last get-in hyperfiddle.q8.lib/reference clojure.core/reset!))

