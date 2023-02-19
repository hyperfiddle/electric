(ns ;^:dev/always ; force rebuild here? We don't understand why
  user.demo-index
  #?(:cljs (:require-macros user.demo-index))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as router])) ; for link only

(def pages
  [`user.demo-1-hello-world/HelloWorld
   ::toggle
   ::system-properties
   ::chat
   ::chat-extended
   ::webview
   ::todos-simple
   ::todomvc
   ::todomvc-composed
   ::color
   ::two-clocks
   ::infinite-scroll
   ::seven-guis-counter
   ::seven-guis-temperature-converter
   ::seven-guis-timer
   ::seven-guis-crud
   ::tic-tac-toe
   #_`user.demo-reagent-interop/ReagentInterop])

(def secret-pages
  [::hfql-teeshirt-orders
   `wip.demo-explorer/DirectoryExplorer
   ::explorer2
   ::demo-10k-dom-elements
   ::router-recursion
   ::tag-picker
   ; need extra deps alias
   ::dennis-exception-leak
   #_::demo-stage-ui4
   #_`wip.datomic-browser/DatomicBrowser])

(e/defn Demos []
  (e/client
    (dom/h1 (dom/text "Demos — Electric Clojure/Script"))
    (dom/p (dom/text "See source code in src-docs."))
    (e/for [k pages]
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
