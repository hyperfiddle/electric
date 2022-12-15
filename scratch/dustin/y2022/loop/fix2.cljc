(ns fix2)


; db = App(db) -- no initial value needed, they are same object
; (App (App (App ...)))
; the argument passed to app, and the result of app, are the same object

; iteration:

:= (iterate App .) ; what initial value?
; this can converge or not converge, depending on base case

(defn factorial [[n a]]
  (cond
    (< n 0) 0
    (= n 0) 1
    (= n 1) a
    ;() (recur (dec n) (* n a))
    () [(dec n) (* n a)]))
;
;(defn fib0 [x]
;  (case x
;    0 0
;    1 0
;    (+ x (fib (dec x)))))
;
;(map fib (range 5))

(defn fib [x]
  (case x
    0 1
    1 1
    (+ (fib (dec x)) (fib (dec (dec x))))))

(map fib (range 5)) := [1 1 2 3 5]

;(take 5 (map second (iterate factorial [5 1]))) ; it has an answer, there are 10 multiplies

(defn fixed-fib [[a b]]
  [b (+' a b)])

(def fib2 (map first (iterate fixed-fib [1 1])))
(take 5 fib2) := [1 1 2 3 5]


(require '[hyperfiddle.rcf :as rcf :refer [with tests % tap]])
(require '[hyperfiddle.photon :as p])
(tests
  (with (p/run
          (try
            (p/with-cycle [[a b] [0 1]] ; delayed, the initial value was never the body
              (tap 'mount)
              (tap b) ; work skipped the 1
              (case a
                5 (assert false)
                (fixed-fib [a b])))
            (catch Throwable e (tap ::terminate))))
    % := 'mount
    [% % % % %] := [1 2 3 5 8]
    % := ::terminate))

(defn fixed-dec-until [a x]
  (if (= x a)
    ))


; with-cycle will iterate until it reaches a fixed point,
; and then work-skipping will terminate at this fixed point.
; The point is with-cycle stops iterating when the state of the cycle is the same
; as the result.

(tests
  (take 3 (iterate #(Math/abs %) -1))
  := [-1 1 1]

  (with (p/run (p/with-cycle [x -1]
                 (tap x)
                 (Math/abs x))))
  [% % %] := [-1 1 ::rcf/timeout])


; continuous time
; quantized time
; discrete time


(p/run (do (println 1)
           (println 2)))

(p/with-cycle [x 1] (inc x)) ; singularity
