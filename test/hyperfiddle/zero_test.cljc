(ns hyperfiddle.zero-test
  (:require [hyperfiddle.rcf :refer [tests tap % with]]
            [hyperfiddle.zero :as z]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]))

(tests
  (def !x (atom 0))
  (def !e (atom nil))
  (defn sub [!]
    (reset! !e !)
    #(reset! !e nil))
  (with
    (p/run
      (let [x (new (m/watch !x))]
        (tap (p/impulse x (m/observe sub)))))
    % := nil
    (@!e 1)
    % := 1
    @!e := nil
    (swap! !x inc)
    % := nil))
