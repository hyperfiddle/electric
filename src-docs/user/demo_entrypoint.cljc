(ns ^:dev/always ; force rebuild here? We don't understand why
  user.demo-entrypoint
  #?(:cljs (:require-macros user.demo-entrypoint))
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.router :as router])) ; for link only

(p/def pages
  [::hello-world
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

(p/def secret-pages
  [::hfql-teeshirt-orders
   ::explorer
   ::demo-10k-dom-elements
   ::router
   ::tag-picker
   ; need extra deps alias
   ::dennis-exception-leak
   ::demo-stage-ui4
   ::datomic-browser])

(p/defn Demos []
  (p/client
    (dom/h1 (dom/text "Photon Demos"))
    (dom/p (dom/text "See source code in src-docs."))
    (p/for [k pages]
      (dom/div (router/link [k] (dom/text (name k)))))
    (dom/div (dom/style {:opacity 0})
      (router/link [::Secrets] (dom/text "secret-hyperfiddle-demos")))))

(p/defn Secrets []
  (p/client
    (dom/h1 "Hyperfiddle demos, unstable/wip")
    (dom/p "These may require a Datomic connection and are unstable, wip, often broken")
    (p/for [k secret-pages]
      (dom/div (router/link [k] (dom/text (name k)))))))
