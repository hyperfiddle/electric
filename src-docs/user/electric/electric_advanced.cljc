(ns user.electric.electric-advanced
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(tests
  "Photon lambdas of non-zero arity also compile to missionary continuous flows (not just thunks)"
  (def !x (atom 0))
  (with (e/run
          (let [flow (->> (e/fn [y] y) ; this Photon lambda is also a missionary continuous flow
                          (m/eduction (dedupe))
                          (m/relieve {}))]
            (tap (new flow (e/watch !x))))) ; so long as you bind the parameters on construction, when the flow eventually runs the parameters are present
        % := 0
        (swap! !x inc)
        % := 1
        (swap! !x identity)
        ; % := 1 -- skipped by dedupe
        (swap! !x inc)
        % := 2))

; e/fn actually is not primitive. e/fn macroexpands to continuous flows with parameters injected
; through dynamic scope. Therefore, F (a Photon closure) is concretely a missionary continuous flow,
; whose argv must be injected by dynamic bindings, which is done by the special form (new).

; the problem is buffering, eduction has a buffer, whenever the buffer is full, eduction
; is ready to transfer. so if downstream doesn't sample and upstream emits again, eduction won't
; poll the input because the buffer is full, and when the sampling will happen you see the value
; inside the buffer (not the latest). so relieve ensures the buffers are flushed.


(comment
  "infinite loop on construction, hangs RCF"
  (def !x (atom 0))
  (with (e/run (reset! !x (tap (inc (new (m/watch !x))))))))

