(ns hyperfiddle.router
  (:require [hfdl.lang :as p]
            [hyperfiddle.api]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.todomvc :as t]
            [hyperfiddle.examples.seven-guis.counter :as counter]
            [hyperfiddle.examples.seven-guis.temperatures :as temperature]
            [user.hfql-full :as hfql-full]
            [user.hfql-block :as block-sub])
  #?(:cljs (:require-macros [hyperfiddle.router :refer [router hello-world]])))


(p/def hello-world #'(dom/text (str "hello " ~@ ~(m/watch hyperfiddle.api/info) " !")))

(defn set-latency! [x]
  #?(:cljs (set! js/window.hyperfiddle.client.LATENCY (js/parseInt x))))

(p/def router
  #'(dom/div (dom/style {"display"        "grid"
                         "grid-auto-flow" "rows"})
             (let [selected (dom/select
                             (dom/option (dom/text "Pick an example"))
                             (dom/option (dom/attribute "value" "hello-world") (dom/text "Hello World"))
                             (dom/option (dom/attribute "value" "counter") (dom/text "Counter"))
                             (dom/option (dom/attribute "value" "todomvc") (dom/text "TodoMVC"))
                             (dom/option (dom/attribute "value" "temperature") (dom/text "Temperature"))
                             (dom/option (dom/attribute "value" "submissions") (dom/text "Submissions"))
                             (dom/option (dom/property "selected" true) (dom/attribute "value" "block-sub-from-school")
                                         (dom/text "Block-sub-from-school"))
                             ~(->> (dom/events dom/parent "change")
                                   (m/eduction (map dom/target-value))
                                   (m/reductions {} "block-sub-from-school")
                                   (m/relieve {})))]
               (dom/element "br")
               (dom/element "label"
                            (dom/text "Global client latency (ms) †")
                            (dom/attribute "title" "Will throttle all websocket client writes and reads."))
               (dom/input (dom/attribute "type" "number")
                          (dom/attribute "value" hyperfiddle.client/LATENCY)
                          ~(->> (dom/events dom/parent "input")
                                (m/eduction (map dom/target-value)
                                            (map set-latency!))
                                (m/reductions {} 0)
                                (m/relieve {})))

               (dom/element "br")
               (case selected
                 "hello-world" ~hello-world
                 "todomvc"     ~t/app
                 "counter"     ~counter/counter
                 "temperature" ~temperature/main
                 "submissions" ~hfql-full/page-submissions
                 "block-sub-from-school" ~block-sub/block-sub-from-school-renderer
                 nil))))
