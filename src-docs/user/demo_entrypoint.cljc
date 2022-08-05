(ns user.demo-entrypoint
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            user.demo-1-counter
            user.demo-2-chat
            user.demo-3-system-properties
            user.demo-4-webview
            user.demo-5-todos-basic
            user.demo-6-two-clocks
            user.todomvc
            user.demo-7-explorer
            user.demo-8-10k-elements
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            wip.demo-bubbles
            wip.demo-color
            user.healthcheck)
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-entrypoint)))


(defonce !demo (atom {:text "counter" ::value user.demo-1-counter/App}))              ; broken initial state

(p/defn Username []
  ;; Optional. Browse to `/auth`` to authenticate, any user/password will do.
  (when-let [username (p/server (get-in hf/*http-request* [:cookies "username" :value]))]
    (dom/p (dom/text "Authenticated as ")
           (dom/span {:style {:font-weight :bold}} (dom/text username)))))

(p/defn App []
  (let [selected (p/watch !demo)]
    (dom/div
      (Username.)
      (dom/h1 (dom/text "Photon Demos"))
      (dom/p (dom/text "Pick a demo. See source code in src-docs."))
      (ui/select {::ui/value        selected
                  ::ui/options      [{:text "counter" ::value user.demo-1-counter/App}
                                     {:text "two clocks" ::value user.demo-6-two-clocks/App}
                                     {:text "chat" ::value user.demo-2-chat/App}
                                     {:text "system properties" ::value user.demo-3-system-properties/App}
                                     {:text "webview" ::value user.demo-4-webview/App}
                                     {:text "todos basic" ::value user.demo-5-todos-basic/Todo-list}
                                     {:text "todomvc" ::value user.todomvc/App}
                                     {:text "explorer" ::value user.demo-7-explorer/App}
                                     {:text "10k elements" ::value user.demo-8-10k-elements/App}
                                     {:text "7GUIs - counter" ::value user.seven-gui-1-counter}
                                     {:text "7GUIs - temperature converter" ::value user.seven-gui-2-temperature-converter}
                                     {:text "7GUIs - timer" ::value user.seven-gui-4-timer}
                                     {:text "7GUIs - crud" ::value user.seven-gui-5-crud}
                                     {:text "bubbles" ::value wip.demo-bubbles/App}
                                     {:text "color" ::value wip.demo-color/App}
                                     {:text "healthcheck" ::value user.healthcheck/App}]
                  ::ui/change-event (p/fn [[event value]] (reset! !demo value) nil)})
      (dom/div {:style {:max-width  "90vw"
                        :overflow-x :auto}}
               (let [{::keys [::value]} selected]
                 (when value (new value))
                 (user.healthcheck/App.))))))               ; work around broken default state

(def ^:export main #?(:cljs (p/boot
                              (try
                                (binding [dom/node (dom/by-id "root")]
                                  (App.))
                                (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
