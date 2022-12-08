(ns dustin.y2022.loop.photon-loop2
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :refer [tests tap % with]]))

(hyperfiddle.rcf/enable!)

(declare rec)

(p/defn Fix1 [f]
  (rec [x (f x)]
       x))

(p/defn Fix2 [f]
  (loop [x .]
    (recur (f x))
    x))

(p/defn Loop1 [f b]
  (rec [[c d] (f [b d])]
       c))

(p/defn Loop2 [f b]
  (loop [[c d] [. .]]
    (recur (f [b d]))
    c))

(declare linseg oscils breath tone)
(p/defn Flute [dur freq amp vfreq]
  (let [amp1 (linseg)
        amp2 (linseg)
        ampv (linseg)
        flow (rand 1 amp1)
        vibr (oscils vfreq (* 0.1 ampv))]
    (loop [[x out feedbk body] (repeat .)] ; PROBLEM
      (recur [x (delay (/ 1 freq 2) (+ (* breath flow) amp1 vibr feedbk))
              out (tone (+ x (- (* x x x)) feedbk) 2000)
              feedbk (* body 0.4)
              body (delay (/ 1 freq) out)])
      (* out (* amp amp2)))))

(p/run (loop [a true]
         [(recur a)
          (recur (not a))]))
:= [[[[] []] [[] []]] [[[] []] [[] []]]] ; cantor set


(loop [[a b] ["a" "b"]]
  (recur [a (ui/input a)])
  (recur [(ui/input b) b])
  [a b])

(loop [t nil]
  (recur (f->c (ui/input (c->f a))))
  (recur (ui/input a)) ; undefined for simultaneous events
  t)

(let [!x (atom 0) x (p/watch !x)]
  (reset! !x (f->c (ui/input (c->f x))))
  (reset! !x (ui/input x)) ; undefined for simultaneous events
  x)


(loop [t 0]
  (recur (ui/input a))
  t)

(tests
  (with
    (p/run (tap (let [!x (atom 0) x (p/watch !x)]
                  (reset! !x (ui/input x))
                  x)))
    % := 0
    ))


; must stabilize

(p/defn mfix [F]
  (let [!x (atom .) x (p/watch !x)]
    (reset! !x (F. x))))

(p/defn mfix [F]
  (p/with-cycle [x .]
    (F. x)))

(defn fix [f]
  (let [x (f x)]
    x))

(p/defn fix2 [F]
  (let [x (F. x)] ; what is the initial value?
    x))

(p/defn Fix3 [F]
  (loop [x .]         ; no possible initial value without laziness
    (recur (F. x))
    x))

(comment
  (define Y!
          (lambda (f-maker)
                  ((lambda (f)
                           (set! f (f-maker (lambda (x) (f x)))) ;; assignment statement
                           f)
                   'NONE))))

(p/def Factorial-gen (p/fn [Rec]
                       (p/fn [n]
                         (if (zero? n)
                           1
                           (* n (new Rec (dec n)))))))

(p/defn Y! [Gen]
  (new (p/fn [!F]
         (reset! !F (new Gen (p/fn [x] (new (p/watch !F) x))))) ; assignment statement
       (atom ::undefined)))

(tests
  (def !n (atom 5))
  (with (p/run (tap (new
                      (Y!. Factorial-gen)
                      (p/watch !n))))
    % := 120))

(p/def Y "Y-Combinator"
  (p/fn [f]
    (new
      (p/fn [x] (new x x))
      (p/fn [x] (new f (p/fn [y] (new (new x x) y)))))))

(p/def Factorial-gen (p/fn [Rec]
                       (p/fn [n]
                         (if (zero? n)
                           1
                           (* n (new Rec (dec n)))))))

(comment
  "Y-Combinator"
  (let [!n (atom 5)]
    (with (p/run (tap (new (p/Y. Factorial-gen) (p/watch !n))))
      % := 120
      (reset! !n 20)
      % := 2432902008176640000)))

(p/defn LabelForm [e]
  (p/server
    (loop [[a b :as state] (repeat nil)]
      (recur [a
              (ui/input (p/server (:label/gid (d/pull hf/db [:label/gid] e))))])

      (recur [(ui/input (p/server (:label/name (d/pull hf/db [:label/name] e))))
              b])

      (when (and (empty->nil a)
                 (empty->nil b))
        [`(my-txn ~a ~b)]))))