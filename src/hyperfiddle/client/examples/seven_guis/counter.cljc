(ns hyperfiddle.client.examples.seven-guis.counter
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [devcards.core :as dc]
            [missionary.core :as m]
            [hyperfiddle.client.examples.card :refer [dom-node]])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.counter :refer [Counter]])))

(defn inc-reducer [r _] (inc r))

(photon/defn Counter []
  (let [!count (atom 0)]
    (dom/div
     (dom/input (dom/style {"margin-right" "1rem"})
                (dom/attribute "value" ~(m/watch !count))
                (dom/attribute "disabled" true))
     (dom/button
      (dom/text "Count")
      (reset! !count ~(->> (dom/events dom/parent dom/click-event)
                           (m/reductions inc-reducer 0)))))))

(dc/defcard counter
  "# 1 — Counter

   Challenge: Understanding the basic ideas of a language/toolkit.

   ![](https://eugenkiss.github.io/7guis/static/counter.9cd92091.png)

   The task is to build a frame containing a label or read-only textfield T and
   a button B. Initially, the value in T is “0” and each click of B increases
   the value in T by one.

   Counter serves as a gentle introduction to the basics of the language,
   paradigm and toolkit for one of the simplest GUI applications imaginable.
   Thus, Counter reveals the required scaffolding and how the very basic
   features work together to build a GUI application. A good solution will have
   almost no scaffolding."
  (dom-node
   (fn [_ node]
     (photon/run
       (photon/binding [dom/parent node]
         (photon/$ Counter))))))
