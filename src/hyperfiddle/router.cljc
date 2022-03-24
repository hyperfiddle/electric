(ns hyperfiddle.router
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.todomvc :as t]
            [hyperfiddle.examples.seven-guis.counter :as counter]
            [hyperfiddle.examples.seven-guis.temperatures :as temperature]
            [user.browser :as browser]
            )
  #?(:cljs (:require-macros [hyperfiddle.router :refer [router hello-world]])))

(p/def router
  #'(dom/div (dom/style {"display"        "grid"
                         "grid-auto-flow" "rows"})
             (let [selected (dom/select
                             (dom/option (dom/text "Pick an example"))
                             (dom/option (dom/attribute "value" "counter") (dom/text "Counter"))
                             (dom/option (dom/attribute "value" "todomvc") (dom/text "TodoMVC"))
                             (dom/option (dom/attribute "value" "temperature") (dom/text "Temperature"))
                             (dom/option (dom/attribute "value" "distributed") (dom/text "Distribution"))
                             (dom/option (dom/property "selected" true) (dom/attribute "value" "block-sub-from-school")
                                         (dom/text "Block-sub-from-school"))
                             ~(->> (dom/events dom/parent "change")
                                   (m/eduction (map dom/target-value))
                                   (m/reductions {} "browser" #_"block-sub-from-school")
                                   (m/relieve {})))]
               (dom/element "br")
               (dom/element "label"
                            (dom/text "Global client latency (ms) †")
                            (dom/attribute "title" "Will throttle all websocket client writes and reads."))

               (dom/element "br")
               (case selected
                 "todomvc"     ~t/app
                 "counter"     ~counter/counter
                 "temperature" ~temperature/main
                 "browser" ~browser/view
                 nil))))
