(ns ;^:dev/always ; force rebuild here? We don't understand why
  user.demo-index
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as router])) ; for link only

(def pages
  [`user.demo-two-clocks/TwoClocks
   `user.demo-toggle/Toggle
   `user.demo-system-properties/SystemProperties
   `user.demo-chat/Chat
   `user.demo-chat-extended/ChatExtended
   `user.demo-webview/Webview
   `user.demo-todos-simple/TodoList
   `user.demo-todomvc/TodoMVC
   `user.demo-todomvc-branched/TodoMVCBranched
   `user.demo-todomvc-composed/TodoMVC-composed
   `user.demo-explorer/DirectoryExplorer
   `user.demo-virtual-scroll/VirtualScroll
   `user.tutorial-blinker/Blinker
   `user.demo-color/Color
   `user.demo-svg/SVG
   `user.demo-tic-tac-toe/TicTacToe
   #_`user.demo-reagent-interop/ReagentInterop])

(def seven-guis
  [`user.tutorial-7guis-1-counter/Counter
   `user.tutorial-7guis-2-temperature/TemperatureConverter
   `user.tutorial-7guis-4-timer/Timer
   `user.tutorial-7guis-5-crud/CRUD])

(def secret-pages
  [;`user.demo-10k-dom/Dom-10k-Elements
   `wip.tag-picker/TagPicker
   `wip.demo-custom-types/CustomTypes
   `wip.tracing/TracingDemo

   ; need extra deps alias
   ;::dennis-exception-leak
   #_`wip.demo-stage-ui4/CrudForm
   #_`wip.datomic-browser/DatomicBrowser])

(e/defn Demos []
  (e/client
    (dom/h1 (dom/text "Demos — Electric Clojure"))
    (dom/p (dom/text "See source code in src-docs."))
    (e/for [k pages]
      (dom/div (router/link [k] (dom/text (name k)))))

    (dom/h2 (dom/text "7 GUIs"))
    (e/for [k seven-guis]
      (dom/div (router/link [k] (dom/text (name k)))))

    (dom/div (dom/style {:opacity 0})
      (router/link [`Secrets] (dom/text "secret-hyperfiddle-demos")))))

(e/defn Secrets []
  (e/client
    (dom/h1 (dom/text "Wip unpublished demos (unstable/wip)")
      (dom/comment_ "ssh" "it's a secret"))
    (dom/p "Some require a database connection and are often broken.")
    (e/for [k secret-pages]
      (dom/div (router/link [k] (dom/text (name k)))))))
