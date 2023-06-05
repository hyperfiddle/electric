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
            user.demo-todomvc-branched
            user.demo-todomvc-composed

            user.demo-explorer
            user.demo-10k-dom
            user.demo-svg
            user.demo-todos-simple
            wip.demo-todos-advanced-old
            wip.demo-todos-advanced
            user.tutorial-7guis-1-counter
            user.tutorial-7guis-2-temperature
            user.tutorial-7guis-4-timer
            user.tutorial-7guis-5-crud
            user.demo-virtual-scroll
            user.demo-color
            user.demo-tic-tac-toe
            user.tutorial-blinker
            wip.tag-picker
            wip.demo-custom-types
            wip.tracing

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
      `user.demo-two-clocks/TwoClocks user.demo-two-clocks/TwoClocks
      `user.demo-explorer/DirectoryExplorer user.demo-explorer/DirectoryExplorer
      ;user.demo-10k-dom/Dom-10k-Elements user.demo-10k-dom/Dom-10k-Elements ; todo too slow to unmount, crashes
      `wip.tag-picker/TagPicker wip.tag-picker/TagPicker
      `user.demo-toggle/Toggle user.demo-toggle/Toggle
      `wip.demo-custom-types/CustomTypes wip.demo-custom-types/CustomTypes
      `user.demo-system-properties/SystemProperties user.demo-system-properties/SystemProperties
      `user.demo-chat/Chat user.demo-chat/Chat
      `user.demo-chat-extended/ChatExtended user.demo-chat-extended/ChatExtended
      `user.demo-webview/Webview user.demo-webview/Webview
      `user.demo-todos-simple/TodoList user.demo-todos-simple/TodoList ; css fixes
      `user.demo-todomvc/TodoMVC user.demo-todomvc/TodoMVC
      `user.demo-todomvc-branched/TodoMVCBranched user.demo-todomvc-branched/TodoMVCBranched
      `user.demo-todomvc-composed/TodoMVC-composed user.demo-todomvc-composed/TodoMVC-composed
      `user.demo-color/Color user.demo-color/Color
      `user.demo-virtual-scroll/VirtualScroll user.demo-virtual-scroll/VirtualScroll
      `user.tutorial-7guis-1-counter/Counter user.tutorial-7guis-1-counter/Counter
      `user.tutorial-7guis-2-temperature/TemperatureConverter user.tutorial-7guis-2-temperature/TemperatureConverter
      `user.tutorial-7guis-4-timer/Timer user.tutorial-7guis-4-timer/Timer
      `user.tutorial-7guis-5-crud/CRUD user.tutorial-7guis-5-crud/CRUD
      `user.demo-tic-tac-toe/TicTacToe user.demo-tic-tac-toe/TicTacToe
      `user.demo-svg/SVG user.demo-svg/SVG
      `user.tutorial-blinker/Blinker user.tutorial-blinker/Blinker
      `wip.tracing/TracingDemo wip.tracing/TracingDemo
      ;`user.demo-reagent-interop/ReagentInterop (when react-available user.demo-reagent-interop/ReagentInterop)
      ;::demos/dennis-exception-leak wip.dennis-exception-leak/App2
      ;`wip.demo-stage-ui4/CrudForm wip.demo-stage-ui4/CrudForm
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
