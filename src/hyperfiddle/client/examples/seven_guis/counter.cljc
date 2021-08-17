(ns hyperfiddle.client.examples.seven-guis.counter
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [devcards.core :as dc :include-macros true]
            [missionary.core :as m]
            [hyperfiddle.client.examples.card :refer [dom-node]])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.counter :refer [Counter]])))

(defn inc-reducer [r _] (inc r))

(photon/defn Counter [c]
  (dom/div
   (dom/input (dom/style {"margin-right" "1rem"})
              (dom/attribute "value" c)
              (dom/attribute "disabled" true))
   (dom/button
    (dom/text "Count")
    ~(->> (dom/events dom/parent dom/click-event)
          (m/reductions inc-reducer 0)))))

(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    ;; we have to dedupe maybe it’s a hack. Is there something in the language
    ;; forcing us to do that. Should we change the language?
    (fn
      ([v] (reset! !state v))
      ([notify terminate] (>state notify terminate)))))

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
         (let [>count! (state 0)]
           (>count! (photon/$ Counter ~>count!))))))))

(comment
  (defmacro rec [binding expr]
    `(let [state# (state nil)
           ~binding (unquote state#)]
       (state# ~expr)))

  (macroexpand-1 '(rec x (inc x))))
