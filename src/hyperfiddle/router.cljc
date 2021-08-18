(ns hyperfiddle.router
  (:require [hfdl.lang :as p]
            [hyperfiddle.api]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.todomvc :as t])
  #?(:cljs (:require-macros [hyperfiddle.todomvc :refer [router hello-world]])))

(p/def hello-world #'(dom/text (str "hello " ~@ ~(m/watch hyperfiddle.api/info) " !")))

(p/def router
  #'(dom/div (dom/style {"display"        "grid"
                         "grid-auto-flow" "rows"})
             (let [selected (dom/select
                             (dom/option (dom/property "selected" true) (dom/text "Pick an example"))
                             (dom/option (dom/attribute "value" "hello-world") (dom/text "Hello World"))
                             (dom/option (dom/attribute "value" "todomvc") (dom/text "TodoMVC"))
                             ~(->> (dom/events dom/parent "change")
                                   (m/eduction (map dom/target-value))
                                   (m/reductions {} nil)
                                   (m/relieve {})))]
               (case selected
                 "hello-world" ~hello-world
                 "todomvc"     ~t/app
                 nil))))
