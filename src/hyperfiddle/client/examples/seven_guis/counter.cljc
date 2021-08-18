(ns hyperfiddle.client.examples.seven-guis.counter
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
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
          (m/eduction (map (constantly 1)))
          (m/relieve +)
          (m/reductions +)))))

(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    ;; we have to dedupe maybe it’s a hack. Is there something in the language
    ;; forcing us to do that. Should we change the language?
    (fn
      ([v] (reset! !state v))
      ([notify terminate] (>state notify terminate)))))

(defn counter []
  (photon/run
    (photon/binding [dom/parent (dom/by-id "counter")]
      (let [>count! (state 0)]
        (>count! (photon/$ Counter ~>count!))))))

#?(:cljs (counter))

(comment
  (defmacro rec [binding expr]
    `(let [state# (state nil)
           ~binding (unquote state#)]
       (state# ~expr)))

  (macroexpand-1 '(rec x (inc x))))
