(ns dustin.y2022.photon.tco
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.photon.tco)))

(p/def Rec)

(p/defn Fac [n]
  (binding [Rec (p/fn [n]
                  (if (zero? n)
                    1
                    (* n (Rec. (dec n)))))]
    (Rec. n)))

(tests
  (def !n (atom 5))
  (with (p/run (tap (Fac. (p/watch !n))))
    % := 120
    (reset! !n 20)
    % := 2432902008176640000))


(p/defn Fac [n]
  (:ret
    (p/with-cycle [{:keys [n]} {:n n
                                :ret ::undefined}]
      (if (zero? n)
        {:ret 1}
        {:n (dec n)
         :ret (* n (Rec. (dec n)))})))) ; cannot be expressed

(tests
  (def !n (atom 5))
  (with (p/run (tap (Fac. (p/watch !n))))
    % := 120))


;; future
(comment
  (p/defn Fac [n]
    (if (zero? n)
      1
      (* n (Fac. (dec n))))))






(comment
  "reactive recursion"
  (p/defn fib [n]
    ; todo, recursion doesn't work yet
    (case n
      0 0 1 1
      (+ (new fib (- n 2))                                        ; self recur
         (new fib (- n 1)))))
  (def !x (atom 5))
  (with (p/run (tap (fib (new (m/watch !x)))))
    % := 5
    (swap! !x inc)
    % := 8         ; this will reuse the topmost frame, it is still naive though
    ))

(comment
  "recur special form"
  (p/defn fib' [n]
    (case n
      0 0 1 1
      (+ (recur (- n 2)) ; todo
         (recur (- n 1)))))
  (def !x (atom 5))
  (with (p/run (tap (fib' (new (m/watch !x)))))
    % := 5
    (swap! !x inc)

    % := 8         ; this will reuse the topmost frame, it is still naive though
    ))

; todo loop recur

(comment ; still no mutual recursion on p/defn
  "mutual recursion"
  (declare Pong)
  (p/defn Ping [x]
    (dom/div x)
    (case x 0 :done (Pong. (dec x))))
  ; can static call infer $ here? Leo needs to think
  (p/defn Pong [x] (Ping. x))
  (with (p/run (tap (Ping. 3)))
    % := :done))

(comment ; this is different than cc/letfn
  "mutual recursion - letfn"
  (with (p/run (p/letfn [(Ping [x] (case x 0 :done (Pong. (dec x))))
                         (Pong [x] (Ping. x))]
                        (tap (Ping. 3))))
    % := :done))