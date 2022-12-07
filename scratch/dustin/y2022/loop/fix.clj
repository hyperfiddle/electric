(ns dustin.fix
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

; fix :: (a -> a) -> a
; fix f = let {x = f x} in x
(p/defn fix [f]
  (let {x (f x)} ; recursive binding is defined if sufficiently lazy
    x))

(p/defn fact [rec n]
  (case n
    1 1 ; fixed point, doesn't sample n
    2 1
    (+ (rec (- n 1))
       (rec (- n 2)))))

(tests
  "fix"
  (fix fib) := 1
  (fix (constantly "a")) := "a"
  (try (fix inc) (catch StackOverflowError _ ::overflow)) := ::overflow)

(defn fix [f] (let [x (f x)] x))
(defn loop [f b] (let [[c d] (f [b d])] c))

; let fact = fix $ \ f i -> if i > 1 then i * f (i - 1) else i

; fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5

((fix (fn [rec]
        (fn [n]
          (if (<= n 1)
            1
            (* n (rec (- n 1))))))) 5)

; Q0: show an example of loop to implement feedback - fixed fib
; Q1: what is loop useful for in practice? forward references
; Q2: what's the right syntax to express loop? can be added to let
; Q3: do we want rec let in Photon?
; Q4: is it a DAG

; RT is a requirement for a lazy language, not the opposite

; Loop can be implemented as an atom in Photon, don't need laziness?
; but requires missionary to be changed such that this returns 10:
(tests
  (def <x (m/cp
            (let [!x (atom 0)
                  x (m/?< (m/watch !x))]
              (if (< x 10)
                (reset! !x (inc x)) x))))
  (def it (<x #() #()))
  @it := 1
  @it := 10
  (it))
✅✅

(tests
  (def <x (m/cp
            (let [!x (atom 3)
                  x (m/?< (m/watch !x))]
              (cond
                (> x 0) (reset! !x (dec x))
                (< x 0) (reset! !x (inc x))
                (= x 0) x))))
  (def it (<x #(println :notify) #(println :terminate)))
  @it := 2
  @it := 1
  @it := 0
  (it))
✅✅

(tests
  "what if missionary had recursive do?"
  (def <x (m/cp (let [x (m/?< <x)]
                  (cond
                    (> x 0) (dec x)
                    (< x 0) (inc x)
                    (= x 0)))))
  (def it (<x #() #()))
  ; infinite supervision tree
  @it := 0
  (it))

(comment
  (defn f [x]
    (cond
      (> x 0) (f (dec x))
      (< x 0) (f (inc x))
      (= x 0) x))

  (f 9)

  )


; rec
;    name <- function -< input
;    input <- otherFunction -< name

(p/defn Foo [F G]
  (let [name (F. input)
        input (G. name)]

    ))

(p/defn Loop [F b]
  (let [[c d] (F. [b d])]
    c))

(p/defn F [[b d]] [(drop (- d 2) b)
                   (count b)])

(tests
  "ArrowLoop"
  (with (p/run (tap (Loop. F "Hello World")))
    % := "ld"))
