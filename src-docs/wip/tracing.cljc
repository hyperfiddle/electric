(ns wip.tracing
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [contrib.trace :as ct]
            [contrib.trace.datascript-tracer :as ds-tracer]
            [hyperfiddle.electric-ui4 :as ui]
            [clojure.string :as str]))

(e/defn FizzBuzzText [n]
  (ct/trace :text
    (ct/trace :n n)
    (if-some [strs (ct/trace :strs
                     (transduce (filter identity) conj nil
                       [(when (zero? (mod n 5)) "Buzz")
                        (when (zero? (mod n 3)) "Fizz")]))]
      (str/join "" strs)
      (str n))))

(e/defn CodeFor [sym height]
  (ui/edn (e/server (nth (:hyperfiddle.electric.impl.compiler/node (meta (eval `(var ~sym)))) 3))
    (e/fn [_])
    (dom/props {:disabled true})
    (dom/style {:display "block" :width "50em", :height height})))

(e/defn FizzBuzz []
  (dom/div
    (dom/h2 (dom/text "an over-engineered fizzbuzz"))
    (CodeFor. `FizzBuzzText "22em")
    (dom/br)
    (let [!n (atom 0), n (e/watch !n)]
      (ui/long n (e/fn [v] (reset! !n v)) (dom/style {:width "8em"}))
      (dom/span (dom/text (FizzBuzzText. n))))))

(e/defn ExceptionOrValue [n]
  (ct/trace :result
    [(ct/trace :throw-on-even (if (even? n) (throw (ex-info "even!" {:n n})) n))
     (ct/trace :triple (* n 3))]))

(e/defn Exceptions []
  (dom/div
    (dom/h2 (dom/text "exceptions and concurrency"))
    (CodeFor. `ExceptionOrValue "8em")
    (let [!n (atom 0), n (e/watch !n)]
      (ui/long n (e/fn [v] (reset! !n v)) (dom/style {:width "8em"}))
      (ExceptionOrValue. n))))

(e/defn Note [text] (dom/em (dom/style {:display "block"}) (dom/text text)))

(e/defn TracingDemo []
  (e/client
    (ds-tracer/with-defaults
      (FizzBuzz.)
      (dom/br)
      (ds-tracer/DatascriptTraceView.)
      (dom/br)
      (Note. "It's like println's, but better!")
      (Note. "Click on 2 trace values to get the distance between them!")
      (Note. "If the values are too close to each other increase the time granularity."))
    (ds-tracer/with-defaults
      (Exceptions.)
      (dom/br)
      (ds-tracer/DatascriptTraceView.)
      (dom/br)
      (Note. "Note that `:triple` runs even if `:throw-on-even!` throws!")
      (Note. "That's because Electric code runs concurrently"))))
