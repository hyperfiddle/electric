(ns dustin.y2022.loop.flute
  (:require [hyperfiddle.photon :as p]))

(p/defn linseg [] ...)
(p/defn oscils [a b] ...)
(declare breath)
(p/defn tone [x n])

(p/defn flute [dur freq amp vfreq]
  (let [amp1 (linseg)
        amp2 (linseg)
        ampv (linseg)
        flow (rand 1 amp1)
        vibr (oscils vfreq (* 0.1 ampv))]
    (letrec [x (delay (/ 1 freq 2) (+ (* breath flow) amp1 vibr feedbk))
             out (tone (+ x (- (* x x x)) feedbk) 2000)
             feedbk (* body 0.4)
             body (delay (/ 1 freq) out)]
      (* out (* amp amp2)))))


(p/defn Flute [dur freq amp vfreq]
  (let [amp1 (linseg)
        amp2 (linseg)
        ampv (linseg)
        flow (rand 1 amp1)
        vibr (oscils vfreq (* 0.1 ampv))]
    (loop [[x out feedbk body] (repeat .)] ; PROBLEM – What is initial value?
      (recur [x (delay (/ 1 freq 2) (+ (* breath flow) amp1 vibr feedbk))
              out (tone (+ x (- (* x x x)) feedbk) 2000)
              feedbk (* body 0.4)
              body (delay (/ 1 freq) out)])
      (* out (* amp amp2)))))

; This structure does NOT quite match because of the nil initial values


; https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html

; rec { b <- f a c     ===>    (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
;     ; c <- f b a }                                         ; c <- f b a
;                                                            ; return (b,c) })

(p/defn Mfix [F]) ; first law - non-strict bind/join

;(defn add [x y] (+ x y)) ; Clojure is always strict
;add x y = x + y ; lazy and strict


(p/defn Flute [dur freq amp vfreq]
  (let [amp1 (linseg)
        amp2 (linseg)
        ampv (linseg)
        flow (rand 1 amp1)
        vibr (oscils vfreq (* 0.1 ampv))]

    (let [[x out feedbk body]
          (Mfix (p/fn [[x out feedbk body]]
                  (let [x (delay (/ 1 freq 2) (+ (* breath flow) amp1 vibr feedbk))
                        out (tone (+ x (- (* x x x)) feedbk) 2000)
                        feedbk (* body 0.4)
                        body (delay (/ 1 freq) out)]
                    (recur [x out feedbk body]))))]
      (* out (* amp amp2)))))


(defn foo [x] (println x) (foo (dec x)))

; a -> m b

; a -> b


(p/defn foo (p/fn [rec]
              (p/fn [x]
                (m/observe (fn [!] (println 'mount) #(println 'unmount)))
                (rec (dec x)))))
