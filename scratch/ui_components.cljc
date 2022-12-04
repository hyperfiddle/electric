(ns wip.ui-components
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [clojure.string :as str]
            #?(:cljs [goog.date.DateTime :as dt]))
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.ui-components)))

(defn query-names [needle]
  (->> [{:id 1, :text "alice"},
        {:id 2, :text "bob"}
        {:id 3, :text "charlie"}]
       (filter #(str/includes? (:text %) needle))))

(defn run-long-task! []
  (prn "Running long task")
  #?(:clj (try (Thread/sleep 2000)
               :result
               (catch InterruptedException _ (prn :interrupted)))))

(p/defn App []
  (dom/hiccup
   [:div
    [:h1 "UI Components"]
    [:hr]
    [:h2 "Button"]

    (let [event (ui/button {::ui/click-event (p/fn [event] true)}
                  (dom/text "log to console"))]
      (prn "clicked! " event))

    [:hr]
    [:h2 "Button with pending state"]

                            ::ui/pending {::dom/disabled true}}
    (let [event (ui/button {::ui/click-event (p/fn [e] (p/server (p/wrap (run-long-task!))))
                  (dom/text "Long running task (log to console)"))]
      (prn "clicked! " event))

    [:h2 "Button with cancelable pending state"]

    (let [event (ui/button {;; Won’t fire if element is aria-disabled or an descendant of an aria disabled
                            ::ui/pending {;; These props are set when button enters pending state
                                          ::dom/aria-busy     true
                                          ::dom/aria-disabled true
                                          ::ui/click-event    (p/fn [_] [::ui/cancel true])
                                          ::ui/keychords      #{"esc"}
                                          ;; This event will fire even if the element is aria-disabled
                                          ::ui/keychord-event (p/fn [_] [::ui/cancel true])  ; cancel pending state
                                          }}
                            ::ui/click-event (p/fn [event] (p/server (p/wrap (run-long-task!))))
                  (dom/text "Long running task (log to console)"))]
      (prn "clicked! 2 " event))

    [:hr]
    [:h2 "Checkbox"]

    [:label
     (let [checked? (ui/checkbox)]
       (dom/text " Checked? " checked?))]

    [:hr]
    [:h2 "Text input"]
    [:span (let [value (ui/input {::dom/placeholder "Text …"
                                  ::ui/value       "init"})]
             (dom/text value))]

    [:hr]
    [:h2 "Numeric input"]

    [:span (let [value (ui/input {::ui/type          :number
                                  ::ui/format        "%.2f"
                                  ::dom/step          0.5
                                  ::ui/value         (/ 10 3)
                                  ::ui/value-changed (p/fn [value] value)})]
             (dom/text value))]

    [:hr]
    [:h2 "Date"]
    [:span (let [input (ui/input {::dom/placeholder "Date …"
                                  ::dom/type        :datetime-local})]
             (dom/text (if (some? (::ui/value input))
                         (pr-str (:date (dt/fromIsoString (::ui/value input))))
                         "Invalid date")))]

    [:hr]
    [:h2 "Select"]
    [:span (let [selected (ui/select {::ui/value   {:value 0, :text "Initial"}
                                      ::ui/options [{:value 1, :text "One"}
                                                    {:value 2, :text "Two"}
                                                    {:value 3, :text "Three"}]})]
             (dom/text selected))]

    [:hr]
    [:h2 "Native Typeahead"]

    [:span (let [value (ui/native-typeahead {::dom/placeholder "Search…"
                                             ::ui/options     (p/fn [needle] (query-names (or needle "")))})]
             (dom/text value))]

    ]))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))


(comment
  #?(:clj (user/browser-main! `main))
  )
