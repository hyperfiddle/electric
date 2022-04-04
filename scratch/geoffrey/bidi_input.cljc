(ns scratch.bidi-input
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))

(defn toggle! [!switch bool]
  (prn "switch state" bool)
  (reset! !switch bool)
  nil)

;; A dom input tailored for bi-directial binding.
;; Starts by emiting the value of `input`, then only emit what the user typed.
;; Does not overwrite its content when the user is typing.
;; - when not focused, display the latest value of `input`
;; - when focused, display the latest user input.
(p/defn BidiInput [input]
  (let [!switch (atom true)
        >input  (->> #'input
                     (m/eduction (filter (partial deref !switch))))
        output ~(->> #'(dom/input (dom/property "value" ~(m/relieve {} >input))
                                  ~(->> (dom/events dom/parent "focus")
                                        (m/eduction (map (partial toggle! !switch false)))
                                        (m/reductions {} nil)
                                        (m/relieve {}))
                                  ~(->> (dom/events dom/parent "blur")
                                        (m/eduction (map (partial toggle! !switch true)))
                                        (m/reductions {} nil)
                                        (m/relieve {}))
                                  ~(->> (dom/events dom/parent "input")
                                        (m/eduction (map dom/target-value))
                                        (m/reductions {} ~(m/relieve {} (m/eduction (take 1) >input)))
                                        (m/relieve {})))
                     (m/eduction (dedupe))
                     (m/reductions {} nil)
                     (m/relieve {}))]
    output))

(def !state (atom 0))

(p/defn view []
  (let [left ~(->> (m/watch !state)
                   (m/eduction (dedupe))
                   (m/relieve {}))
        right (+ 10 left)
        left' (int (p/$ BidiInput left))
        right' (int (p/$ BidiInput right))]
    (dom/div (dom/div (dom/text (str "left: " left)))
             (dom/div (dom/text (str "left': " left')))
             (dom/div (dom/text (str "right': " right'))))
    (reset! !state left')
    (reset! !state (- right' 10))))