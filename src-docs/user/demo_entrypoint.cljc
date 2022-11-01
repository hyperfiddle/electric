(ns user.demo-entrypoint
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            user.demo-1-hello-world
            user.demo-2-toggle
            user.demo-3-system-properties
            user.demo-4-chat
            user.demo-4-chat-extended
            user.demo-4-webview
            user.demo-5-todomvc
            user.demo-todomvc-composed
            user.demo-6-two-clocks
            user.demo-7-explorer
            user.demo-8-10k-elements
            #_user.demo-hfql
            user.todos-simple
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            #_user.datomic-browser ; requires datomic dep
            hyperfiddle.scrollview
            wip.demo-bubbles
            wip.demo-color
            ;; wip.photon-tree
            user.popover
            user.healthcheck
            wip.typeahead
            #_wip.hfql
            )
  #?(:cljs (:require-macros user.demo-entrypoint)))

(defonce !demo-index #?(:clj (atom 0) :cljs nil)) (comment (reset! !demo-index 0))
;(p/def !demo-index (p/server (atom {:text "hello world" ::value user.demo-1-hello-world/App})))
(p/def index (p/server (p/watch !demo-index)))

(p/def demos [{:text "hello world" ::value user.demo-1-hello-world/App}
              {:text "toggle" ::value user.demo-2-toggle/App}
              {:text "system properties" ::value user.demo-3-system-properties/App}
              {:text "chat" ::value user.demo-4-chat/App}
              {:text "chat-extended" ::value user.demo-4-chat-extended/App}
              {:text "webview" ::value user.demo-4-webview/App}
              #_{:text "todos simple" ::value user.todos-simple/Todo-list} ; css fixes
              {:text "todomvc" ::value user.demo-5-todomvc/App}
              {:text "todomvc composed" ::value user.demo-todomvc-composed/App}
              {:text "two clocks" ::value user.demo-6-two-clocks/App}
              {:text "explorer" ::value user.demo-7-explorer/App}
              {:text "10k elements" ::value user.demo-8-10k-elements/App}
              #_{:text "HFQL" ::value user.demo-hfql/App}
              #_{:text "HFQL" ::value wip.hfql/App}
              #_{:text "Scroll view" ::value hyperfiddle.scrollview/Demo} ; takes whole page, covering the demo picker control
              ; {:text "Datomic browser" ::value user.datomic-browser/App}
              {:text "7GUIs - counter" ::value user.seven-gui-1-counter/Counter}
              {:text "7GUIs - temperature converter" ::value user.seven-gui-2-temperature-converter/App}
              {:text "7GUIs - timer" ::value user.seven-gui-4-timer/Timer}
              {:text "7GUIs - crud" ::value user.seven-gui-5-crud/App}
              {:text "Typeahead" ::value wip.typeahead/App}
              #_{:text "TreeView" ::value wip.photon-tree/App}
              #_{:text "popover" ::value user.popover/App}
              #_{:text "bubbles" ::value wip.demo-bubbles/App}
              #_{:text "color" ::value wip.demo-color/App}
              #_{:text "healthcheck" ::value user.healthcheck/App}])

(p/defn App []
  (p/client
    (dom/h1 "Photon Demos")
    (dom/p "See source code in src-docs.")
    (ui/select {::ui/value        (get demos (p/server index))
                ::ui/options      demos
                ::ui/change-event (p/fn [[event value]] (let [index (.indexOf demos value)]
                                                          (p/server (reset! !demo-index index))))})
    (dom/hr)
    (dom/div {:style {:width "90vw"}}
      (p/server (new (::value (get demos index)))))))
