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
            user.todos-simple
            user.demo-6-two-clocks
            user.demo-7-explorer
            user.demo-8-10k-elements
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            wip.demo-bubbles
            wip.demo-color
            wip.demo-hfql
            wip.popover
            user.healthcheck)
  #?(:cljs (:require-macros user.demo-entrypoint)))


(defonce !demo #?(:cljs  (atom nil #_{:text "counter" ::value user.demo-1-counter/App})
                  :clj nil))
(p/def demo (p/client (p/watch !demo)))

(p/defn App []
  (dom/div
    (dom/h1 "Photon Demos")
    (dom/p "See source code in src-docs.")
    (ui/select {::ui/value        demo
                ::ui/options      [{:text "hello world" ::value user.demo-1-hello-world/App}
                                   {:text "toggle" ::value user.demo-2-toggle/App}
                                   {:text "system properties" ::value user.demo-3-system-properties/App}
                                   {:text "chat" ::value user.demo-4-chat/App}
                                   {:text "webview" ::value user.demo-4-webview/App}
                                   #_{:text "todos basic" ::value user.todos-simple/Todo-list} ; css fixes
                                   {:text "todomvc" ::value user.demo-5-todomvc/App}
                                   {:text "two clocks" ::value user.demo-6-two-clocks/App}
                                   {:text "explorer" ::value user.demo-7-explorer/App}
                                   {:text "10k elements" ::value user.demo-8-10k-elements/App}
                                   {:text "HFQL" ::value wip.demo-hfql/App}
                                   {:text "Authentication" ::value user.auth/App}

                                   {:text "7GUIs - counter" ::value user.seven-gui-1-counter/Counter}
                                   {:text "7GUIs - temperature converter" ::value user.seven-gui-2-temperature-converter/App}
                                   {:text "7GUIs - timer" ::value user.seven-gui-4-timer/Timer}
                                   {:text "7GUIs - crud" ::value user.seven-gui-5-crud/App}

                                   #_{:text "popover" ::value wip.popover/App}
                                   #_{:text "bubbles" ::value wip.demo-bubbles/App}
                                   #_{:text "color" ::value wip.demo-color/App}
                                   #_{:text "healthcheck" ::value user.healthcheck/App}]
                ::ui/change-event (p/fn [[event value]] (reset! !demo value))})
    (dom/hr)
    (dom/div {:style {:max-width  "90vw" :overflow-x :auto}}
             (let [{::keys [::value]} demo]
               (new (or value user.demo-1-hello-world/App))))))     ; work around broken default state
