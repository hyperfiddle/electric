(ns dustin.fix
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [missionary.core :as m]))

; fix :: (a -> a) -> a
; fix f = let {x = f x} in x
(r/defn fix [f]
  (let {x (f x)}                                            ; recursive binding is defined if sufficiently lazy
    x))

(r/defn fact [rec n]
  (case n
    1 1                                                     ; fixed point, doesn't sample n
    2 1
    (+ (rec (- n 1))
       (rec (- n 2)))))

(tests
  "fix"
  (fix fib) := 1
  (fix (constantly "a")) := "a"
  (try (fix inc) (catch StackOverflowError _ ::overflow)) := ::overflow)

; http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
;
; class Arrow a => ArrowLoop a where
;   loop :: a (b,d) (c,d) -> a b c
;
; instance ArrowLoop (->) where
;   loop f b = let (c,d) = f (b,d) in c

(r/defn loop [f b]
  (let [[c d] (f [b d])]
    c))

; f (b,d) = (drop (d-2) b, length b)
; main = print (loop f "Hello World")

(tests
  "ArrowLoop"
  (def dispose (r/run (! 1)))
  % := 1
  (dispose))

; counter :: ArrowCircuit a => a Bool Int
; counter = proc reset -> do
;         rec     output <- returnA -< if reset then 0 else next
;                 next <- delay 0 -< output+1
;         returnA -< output






