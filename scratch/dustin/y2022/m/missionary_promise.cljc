(ns dustin.y2022.missionary-promise
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(defn await-promise "Returns a task completing with the result of given promise"
  [p]
  ; p is already running, we have no control of that
  ; (Note that promise guarantees that even if already completed, a new .then chain will fire)
  (let [v (m/dfv)]                                          ; dataflow "atom"
    (.then p
           #(v (fn [] %))                                   ; wrap result in closure and put closure in atom
           #(v (fn [] (throw %))))                          ; delayed throw
    (m/absolve v)))

(tests
  (def f (js/Promise.resolve.bind js/Promise))
  ((m/sp (tap (m/? (await-promise (f "1")))))
   tap tap)
  % := "1"
  % := "1")

(defn f [x]
  (js/Promise.resolve.call js/Promise x))

(tests
  ((m/sp (tap (m/? (await-promise (f "2")))))
   tap tap)
  % := "2"
  % := "2")

(tests
  ((m/sp (tap (m/? (await-promise (js/Promise.resolve.call js/Promise "3")))))
   tap tap)
  % := "3"
  % := "3")

(tests
  ((m/sp (tap (m/? (await-promise (.-call (.-resolve js/Promise) js/Promise "3")))))
   tap tap)
  % := "3"
  % := "3")