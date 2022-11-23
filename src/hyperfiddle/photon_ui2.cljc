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

(defmacro input "
A dom input text component.
Purpose of this component is to eventually sync the input with the database which is also an input
of this component.

When component doesn't have focus, input value and return value reflect argument.
When component has focus, argument is ignored and return value reflects user input.

TODO: what if component loses focus, but the user input is not yet committed ?
" [controlled-value & body]
  ; data State = Editing local-value | NotEditing controlled-value
  `(:value ; local or controlled
    (let [cv# ~controlled-value]
      (dom/element "input"
        ~@body
        (.setAttribute dom/node "type" "text")
        (p/with-cycle [state {:edit? false}]
          (if (:edit? state)
            (merge state
              {:edit? (not (some? (dom/Event. "blur" false)))}
              (when-some [e (dom/Event. "input" false)]
                {:value (.-value (.-target e))}))          ; use local value
            (do (.setAttribute dom/node "value" (str cv#)) ; throw away local value
                {:edit? (some? (dom/Event. "focus" false)) ; never busy - process synchronously
                 :value cv#})))))))
