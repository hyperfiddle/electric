(ns ;^:dev/always ; force rebuild here? We don't understand why
  user.demo-index
  #?(:cljs (:require-macros user.demo-index))
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.router :as router])) ; for link only

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
   ::tic-tac-toe])

(def secret-pages
  [::hfql-teeshirt-orders
   `wip.demo-explorer/DirectoryExplorer
   ::explorer2
   ::demo-10k-dom-elements
   ::router-recursion
   ::tag-picker
   ; need extra deps alias
   ::dennis-exception-leak
   ::demo-stage-ui4
   #_`wip.datomic-browser/DatomicBrowser])

(p/defn Demos []
  (p/client
    (dom/h1 (dom/text "Photon Demos"))
    (dom/p (dom/text "See source code in src-docs."))
    (p/for [k pages]
      (dom/div (router/link [k] (dom/text (name k)))))
    (dom/div (dom/style {:opacity 0})
      (router/link [`Secrets] (dom/text "secret-hyperfiddle-demos")))))

(p/defn Secrets []
  (p/client
    (dom/h1 (dom/text "Wip unpublished demos (unstable/wip)")
            (dom/comment_ "ssh" "it's a secret"))
    (dom/p "Some require a database connection and are often broken.")
    (p/for [k secret-pages]
      (dom/div (router/link [k] (dom/text (name k)))))))
