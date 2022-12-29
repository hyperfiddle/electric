(ns user.demo-entrypoint
  #?(:cljs (:require-macros user.demo-entrypoint))
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.router :as router]
            [missionary.core :as m]
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
            user.demo-controlled-input
            user.todos-simple
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            hyperfiddle.scrollview
            user.demo-color
            user.tic-tac-toe
            #_user.demo-hfql
            #_user.datomic-browser ; requires datomic dep
            #_wip.typeahead-ui1
            #_wip.hfql))

(p/def pages [[::hello-world user.demo-1-hello-world/App]
              [::toggle user.demo-2-toggle/App]
              [::system-properties user.demo-3-system-properties/App]
              [::chat user.demo-4-chat/App]
              [::chat-extended user.demo-4-chat-extended/App]
              [::webview user.demo-4-webview/App]
              #_[::todos-simple user.todos-simple/Todo-list] ; css fixes
              [::todomvc user.demo-5-todomvc/App]
              [::todomvc-composed user.demo-todomvc-composed/App]
              [::two-clocks user.demo-6-two-clocks/App]
              #_[::explorer user.demo-7-explorer/App]
              #_[::demo-10k-dom-elements user.demo-8-10k-elements/App]
              #_[::hfql user.demo-hfql/App]
              #_[::hfql2 wip.hfql/App]
              #_[::scroll-view hyperfiddle.scrollview/Demo] ; takes whole page, covering nav
              #_[::datomic-browser user.datomic-browser/App]
              [::seven-guis-counter user.seven-gui-1-counter/Counter]
              [::seven-guis-temperature-converter user.seven-gui-2-temperature-converter/App]
              [::seven-guis-timer user.seven-gui-4-timer/Timer]
              [::seven-guis-crud user.seven-gui-5-crud/App]
              #_[::typeahead user.typeahead-ui1/App]
              #_[::tic-tac-toe user.tic-tac-toe/App]
              #_[::color user.demo-color/App]
              #_[::controlled-input user.demo-controlled-input/App]])

#?(:cljs (defn decode-path [path read-edn-str]
           {:pre [(string? path) (some? read-edn-str)]}
           (case path
             "/" ::index
             (-> path (subs 1) contrib.ednish/decode read-edn-str))))

#?(:cljs (defn encode-path [route] (->> route pr-str contrib.ednish/encode (str "/"))))

(p/defn App []
  (p/client
    (let [!path (m/mbx)
          route (decode-path (router/path !path) hf/read-edn-str)]
      (binding [router/Link (router/->Link. !path encode-path)]

        (dom/div {:style {:width "90vw"}}
          (case route
            ::index (do (dom/h1 "Photon Demos")
                        (dom/p "See source code in src-docs.")
                        (p/for [[k _] pages]
                          (dom/div (router/Link. k (name k)))))
            (p/server (new (get (into {} pages) route)))))))))
