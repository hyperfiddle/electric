(ns hyperfiddle.client.examples
  (:require
   [hyperfiddle.client.examples.reagent1]
   ;; [hyperfiddle.client.examples.reagent2]
   ;; [hyperfiddle.client.examples.reagent3]
   ;; [hyperfiddle.client.examples.reagent4]
   ;; [hyperfiddle.client.examples.reagent5]
   ;; [hyperfiddle.client.examples.reagent6]
   ;; [hyperfiddle.client.examples.reagent7]
   ;; [hyperfiddle.client.examples.reagent8]
   ;; [hyperfiddle.client.examples.reagent9]
   ;; [hyperfiddle.client.examples.reagent10]
   ;; [hyperfiddle.client.examples.reagent11]

   [hyperfiddle.client.examples.seven-guis.counter]
   [hyperfiddle.client.examples.seven-guis.temperatures]
   [hyperfiddle.client.examples.seven-guis.flight-booker]
   [hyperfiddle.client.examples.seven-guis.timer]
   [hyperfiddle.client.examples.seven-guis.crud]
   ;; [hyperfiddle.client.examples.7GUIs-4]
   ;; [hyperfiddle.client.examples.7GUIs-5]
   ;; [hyperfiddle.client.examples.7GUIs-6]
   ;; [hyperfiddle.client.examples.7GUIs-7]

   ["highlight.js" :as hljs]
   ["marked" :as marked]
   [devcards.core :as dc]
   ))

;; Devcards deps patch
;; See https://github.com/bhauman/devcards/issues/168
(js/goog.exportSymbol "DevcardsMarked" marked)
(js/goog.exportSymbol "DevcardSyntaxHighlighter" hljs)

(dc/start-devcard-ui!)
