(ns user.demo-entrypoint
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            user.demo-1-hello-world
            user.demo-2-toggle
            user.demo-3-system-properties
            user.demo-4-chat
            user.demo-4-webview
            user.demo-5-todomvc
            user.demo-6-two-clocks
            user.demo-7-explorer
            user.demo-8-10k-elements
            user.demo-hfql
            user.auth
            user.todos-simple
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            wip.demo-bubbles
            wip.demo-color
            ;; wip.photon-tree
            wip.popover
            user.healthcheck
            wip.typeahead)
  #?(:cljs (:require-macros user.demo-entrypoint)))

(defonce !demo #?(:clj (atom {:text "hello world" ::value `user.demo-1-hello-world/App}) :cljs nil))
;(p/def !demo (p/server (atom {:text "hello world" ::value user.demo-1-hello-world/App})))
(p/def demo (p/server (p/watch !demo)))

(p/def demos                                                ; help
  {`user.demo-1-hello-world/App                user.demo-1-hello-world/App
   `user.demo-2-toggle/App                     user.demo-2-toggle/App
   `user.demo-3-system-properties/App          user.demo-3-system-properties/App
   `user.demo-4-chat/App                       user.demo-4-chat/App
   `user.demo-4-webview/App                    user.demo-4-webview/App
   ;`user.todos-simple/Todo-list                user.todos-simple/Todo-list
   `user.demo-5-todomvc/App                    user.demo-5-todomvc/App
   `user.demo-6-two-clocks/App                 user.demo-6-two-clocks/App
   `user.demo-7-explorer/App                   user.demo-7-explorer/App
   `user.demo-8-10k-elements/App               user.demo-8-10k-elements/App
   `user.demo-hfql/App                         user.demo-hfql/App
   `user.auth/App                              user.auth/App
   `user.seven-gui-1-counter/Counter           user.seven-gui-1-counter/Counter
   `user.seven-gui-2-temperature-converter/App user.seven-gui-2-temperature-converter/App
   `user.seven-gui-4-timer/Timer               user.seven-gui-4-timer/Timer
   `user.seven-gui-5-crud/App                  user.seven-gui-5-crud/App
   ;; `wip.photon-tree/App                        wip.photon-tree/App
   `wip.typeahead/App                        wip.typeahead/App
   ;`wip.popover/App                            wip.popover/App
   ;`wip.demo-bubbles/App                       wip.demo-bubbles/App
   ;`wip.demo-color/App                         wip.demo-color/App
   ;`user.healthcheck/App                       user.healthcheck/App
   })

(p/defn App []
  (dom/h1 "Photon Demos")
  (dom/p "See source code in src-docs.")
  (ui/select {::ui/value        (p/server demo)
              ::ui/options      [{:text "hello world" ::value `user.demo-1-hello-world/App}
                                 {:text "toggle" ::value `user.demo-2-toggle/App}
                                 {:text "system properties" ::value `user.demo-3-system-properties/App}
                                 {:text "chat" ::value `user.demo-4-chat/App}
                                 {:text "webview" ::value `user.demo-4-webview/App}
                                 #_{:text "todos simple" ::value `user.todos-simple/Todo-list} ; css fixes
                                 {:text "todomvc" ::value `user.demo-5-todomvc/App}
                                 {:text "two clocks" ::value `user.demo-6-two-clocks/App}
                                 {:text "explorer" ::value `user.demo-7-explorer/App}
                                 {:text "10k elements" ::value `user.demo-8-10k-elements/App}
                                 {:text "HFQL" ::value `user.demo-hfql/App}
                                 {:text "Authentication" ::value `user.auth/App}
                                 {:text "7GUIs - counter" ::value `user.seven-gui-1-counter/Counter}
                                 {:text "7GUIs - temperature converter" ::value `user.seven-gui-2-temperature-converter/App}
                                 {:text "7GUIs - timer" ::value `user.seven-gui-4-timer/Timer}
                                 {:text "7GUIs - crud" ::value `user.seven-gui-5-crud/App}
                                 {:text "Typeahead" ::value `wip.typeahead/App}
                                 #_{:text "TreeView" ::value `wip.photon-tree/App}
                                 #_{:text "popover" ::value `wip.popover/App}
                                 #_{:text "bubbles" ::value `wip.demo-bubbles/App}
                                 #_{:text "color" ::value `wip.demo-color/App}
                                 #_{:text "healthcheck" ::value `user.healthcheck/App}]
              ::ui/change-event (p/fn [[event value]] (p/server (reset! !demo value)))})
  (dom/hr)
  (dom/div
    {:style {:max-width "90vw" :overflow-x :auto}}
    (new (get demos (p/server (::value demo))))))
