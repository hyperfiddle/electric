(ns hyperfiddle.photon-ui2
  (:require
   [missionary.core :as m]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.rcf :as rcf :refer [tests tap with %]])
  #?(:cljs (:require-macros hyperfiddle.photon-ui2)))

(p/def recurframe)
(defmacro loopframe [bindings & body]
  `(let [atm# (atom ~(vec (take-nth 2 (rest bindings))))
         ~(vec (take-nth 2 bindings)) (p/watch atm#)]
     (binding [recurframe (fn [& args#] (reset! atm# (vec args#)) ::recur)]
       ~@body)))

#_
(tests
  (with (p/run (tap (loopframe [x 2]
                      (tap :side-effect)
                      (if (pos? x) (recurframe (dec x)) x))))
    % := :side-effect
    % := ::recur
    % := 0
    ))

(tests
  (with (p/run (tap (p/with-cycle [x 2] (if (pos? x) (dec x) x))))
    % := 1
    % := 0))

(tests
  (with (p/run (let [!x (atom 2) x (p/watch !x)]
                 (when (pos? x) (reset! !x (dec x)))
                 (tap x)))
    % := 2 % := 1 % := 0))

(p/defn Focused? []
  (p/with-cycle [focused false]
    (case focused
      true (not (some? (dom/Event. "blur" false)))
      false (some? (dom/Event. "focus" false)))))

(defmacro input "
A dom input text component.
Purpose of this component is to eventually sync the input with the database which is also an input
of this component.

When component doesn't have focus, input value and return value reflect argument.
When component has focus, argument is ignored and return value reflects user input.

TODO: what if component loses focus, but the user input is not yet committed ?
" [controlled-value & body]
  `(dom/with
     (dom/dom-element dom/node "input")
     (.setAttribute dom/node "type" "text")
     ~@body
     (case (new Focused?) ; avoid syntax-quote bug
       false (do (.setAttribute dom/node "value" ~controlled-value) ; throw away local value
                 ~controlled-value)
       true (p/with-cycle [input-value ~controlled-value]
              (or (some-> (dom/Event. "input" false) ; never busy - process synchronously
                          .-target .-value) ; set new local value
                  input-value))))) ; use local value when focused

(p/defn Demo []
  (dom/h1 "a controlled input that reverts on blur")
  (let [a (hyperfiddle.photon-ui2/input "hello world")]
    (dom/pre a))

  (dom/h1 "a controlled input with looped state")
  (let [a (p/with-cycle [a "hello world"]
            (hyperfiddle.photon-ui2/input a))]
    (dom/pre a)))