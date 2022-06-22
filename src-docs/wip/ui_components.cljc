(ns wip.ui-components
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [hyperfiddle.ui2 :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.ui-components)))        ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/hiccup
   [:div
    [:h1 "UI Components"]
    [:hr]
    [:h2 "Button"]

    (ui/button {:on-click (map (partial js/alert "hello"))}
               (dom/text "log to console"))
    [:hr]
    [:h2 "Checkbox"]

    [:label
     (let [checked? (ui/checkbox {:checked true})]
       (dom/text " Checked? " checked?))]

    [:hr]
    [:h2 "Numeric input"]

    [:span (let [num (ui/numeric-input {:format "%.2f"
                                        :step   0.5
                                        :value  (/ 10 3)})]
             (dom/text " value: " num))]

    [:hr]
    [:h2 "Text input"]
    [:span (let [num (ui/input {:placeholder "Text …"
                                :value       ""})]
             (dom/text " value: " num))]

    [:hr]
    [:h2 "Date"]
    [:span (let [num (ui/input {:placeholder "Date …"
                                :type        :datetime-local})]
             (dom/text " value: " num))]

    [:hr]
    [:h2 "Select"]
    [:span (let [selected (ui/select {:value   1
                                      :options [{:value 1, :text "One"}
                                                {:value 2, :text "Two"}
                                                {:value 3, :text "Three"}]})]
             (dom/text " value: " (pr-str selected)))]

    [:hr]
    [:h2 "Typeahead"]

    [:span (let [value (ui/typeahead {:placeholder "Search…"
                                      :options (p/fn [needle]
                                                 [{:value 1, :text "One"}
                                                  {:value 2, :text "Two"}
                                                  {:value 3, :text "Three"}
                                                  {:value 4, :text needle}])})]
             (dom/text " value: " value))]

    ]))

(def main #?(:cljs (p/client (p/main
                              (try
                                (binding [dom/node (dom/by-id "root")]
                                  (App.))
                                (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
