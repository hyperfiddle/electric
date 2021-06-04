(ns hyperfiddle.client.ui.demo-edn
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(def EDN '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                              :gender {:db/ident                       :dustingetz/female,
                                                       (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                                        #:db{:ident :dustingetz/womens-medium}
                                                                                        #:db{:ident :dustingetz/womens-large}]}}
                                 #:dustingetz{:email  "bob@example.com",
                                              :gender {:db/ident                       :dustingetz/male,
                                                       (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                        #:db{:ident :dustingetz/mens-medium}
                                                                                        #:db{:ident :dustingetz/mens-large}]}}
                                 #:dustingetz{:email  "charlie@example.com",
                                              :gender {:db/ident                       :dustingetz/male,
                                                       (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                        #:db{:ident :dustingetz/mens-medium}
                                                                                        #:db{:ident :dustingetz/mens-large}]}}],
           (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]})

;; (def EDN {:a 1 :b 2})

(defn brackets [edn]
  (cond
    (list? edn)   "()"
    (vector? edn) "[]"
    (set? edn)    ["#{" "}"]
    :else         "<>"))

(defn node-color [edn]
  (cond
    (symbol? edn)  "#708"
    (number? edn)  "#164"
    (string? edn)  "#a11"
    (keyword? edn) "#219"
    :else          nil))

(defn edn-view
  ([edn]
   (edn-view identity edn))
  ([propsf edn]
   (cond
     (map? edn)       [:div {:style {:display               :grid
                                     :width                 :fit-content
                                     :grid-auto-flow        :row
                                     :grid-template-columns "auto auto 1fr auto"
                                     :grid-gap              "0 0.25rem"}}
                       [:div "{"]
                       (into [:<>]
                             (let [ks (map (partial edn-view #(assoc-in % [:style :grid-column] 2)) (map key edn))
                                   vs (map edn-view (map val edn))]
                               (interleave ks vs)))
                       [:div {:style {:display     :flex
                                      :align-items :flex-end}
                              :class "hf-edn-view-close-braket"}
                        "}"]]
     (map-entry? edn) [:<>
                       (edn-view #(assoc (propsf %) :content-editable false) (first edn))
                       (edn-view (second edn))]
     (list? edn)      (let [[left right] (brackets edn)
                            [f & args]   edn]
                        [:div (propsf {:style {:display        :grid
                                               :grid-auto-flow :column
                                               :grid-gap       "0 0.25rem"}})
                         [:div {} (str left)]
                         (into [:<>] (cons (edn-view #(assoc % :content-editable false) f)
                                           (map edn-view args)))
                         [:div {:class "hf-edn-view-close-braket"} (str right)]])
     (vector? edn)    (let [[left right] (brackets edn)]
                        [:div {:style {:display               :grid
                                       :grid-template-columns "auto 1fr auto"
                                       :grid-auto-flow        :column}}
                         [:div {} (str left)]
                         (into [:div (propsf {:style {:display  :grid
                                                      :grid-gap "0 0.25rem"}})
                                (map (partial edn-view #(assoc-in % [:style :grid-column] 2)) edn)])
                         [:div {:style {:display :flex :align-items :flex-end}
                                :class "hf-edn-view-close-braket"} (str right)]])
     :else            [:input (propsf {:type  :text
                                       :width :fit-content
                                       :value (pr-str edn)
                                       :style {:color (node-color edn)}})])))

(defn ^:export main []
  (rd/render [:div {:style {:font-family "\"Fira Mono\", monaco, monospace"}}
              [edn-view EDN]]
             (.getElementById js/document "hf-ui-dev-root")))


(defn ^:dev/before-load stop [] (rd/unmount-component-at-node (.getElementById js/document "hf-ui-dev-root")))
(defn ^:dev/after-load start [] (main))

