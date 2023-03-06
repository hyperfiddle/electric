(ns user-main
  (:require contrib.uri ; data_readers
            contrib.ednish
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as history]
            [user.demo-index :as demos]

            user.demo-two-clocks
            user.demo-toggle
            user.demo-system-properties
            user.demo-chat
            user.demo-chat-extended
            user.demo-webview
            user.demo-todomvc
            user.demo-todomvc-composed

            user.demo-explorer
            wip.demo-explorer2
            user.demo-10k-dom-elements
            user.demo-svg
            user.todos-simple
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            user.demo-scrollview
            user.demo-color
            user.tic-tac-toe
            user.tutorial-blinker
            wip.demo-branched-route
            #_wip.hfql
            wip.tag-picker
            wip.teeshirt-orders

            ; this demo require `npm install`
            #_user.demo-reagent-interop

            ; these demos require extra deps alias
            #_wip.dennis-exception-leak
            #_wip.demo-stage-ui4
            #_wip.datomic-browser
            ))

(e/defn NotFoundPage []
  (e/client (dom/h1 (dom/text "Page not found"))))

; todo: macro to auto-install demos by attaching clj metadata to e/defn vars?

(e/defn Pages [page]
  (e/server
    (case page
      `user.demo-index/Demos user.demo-index/Demos
      `user.demo-index/Secrets user.demo-index/Secrets
      `user.demo-two-clocks/Two-clocks user.demo-two-clocks/Two-clocks
      ::demos/hfql-teeshirt-orders wip.teeshirt-orders/App
      `user.demo-explorer/DirectoryExplorer user.demo-explorer/DirectoryExplorer
      ::demos/explorer2 wip.demo-explorer2/App
      ;::demos/demo-10k-dom-elements user.demo-10k-dom-elements/App ; todo too slow to unmount, crashes
      ::demos/router-recursion wip.demo-branched-route/App
      ::demos/tag-picker wip.tag-picker/App
      ::demos/toggle user.demo-toggle/App
      ::demos/system-properties user.demo-system-properties/App
      ::demos/chat user.demo-chat/App
      ::demos/chat-extended user.demo-chat-extended/App
      ::demos/webview user.demo-webview/App
      ::demos/todos-simple user.todos-simple/Todo-list ; css fixes
      ::demos/todomvc user.demo-todomvc/App
      ::demos/todomvc-composed user.demo-todomvc-composed/App
      ::demos/color user.demo-color/App
      ::demos/infinite-scroll user.demo-scrollview/Demo
      ::demos/seven-guis-counter user.seven-gui-1-counter/Counter
      ::demos/seven-guis-temperature-converter user.seven-gui-2-temperature-converter/App
      ::demos/seven-guis-timer user.seven-gui-4-timer/Timer
      ::demos/seven-guis-crud user.seven-gui-5-crud/App
      ::demos/tic-tac-toe user.tic-tac-toe/App
      `user.demo-svg/SVG user.demo-svg/SVG
      `user.tutorial-blinker/Blinker user.tutorial-blinker/Blinker
      ;`user.demo-reagent-interop/ReagentInterop (when react-available user.demo-reagent-interop/ReagentInterop)
      ;::demos/dennis-exception-leak wip.dennis-exception-leak/App2
      ;::demos/demo-stage-ui4 wip.demo-stage-ui4/Demo
      ;`wip.datomic-browser/DatomicBrowser wip.datomic-browser/DatomicBrowser
      NotFoundPage)))

(e/defn Main []
  (binding [history/encode contrib.ednish/encode-uri
            history/decode #(or (contrib.ednish/decode-path % hf/read-edn-str)
                               [`user.demo-index/Demos])]
    (history/router (history/HTML5-History.)
      (set! (.-title js/document) (str (clojure.string/capitalize (name (first history/route))) " - Hyperfiddle"))
      (binding [dom/node js/document.body]
        (dom/pre (dom/text (contrib.str/pprint-str history/route)))
        (let [[page & args] history/route]
          (e/server (new (Pages. page #_args))))))))
