(ns dustin.missionary3
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m :refer
     [? ?? ?! ?= sp ap join latest aggregate enumerate]]))


(tests
  "continuous hello-world"

  (def r (atom 0))
  (def >a (m/watch r))
  (def >b (ap (inc (?? >a))))
  (def b! (>b #(println :ready) #(println :done)))
  ; ready
  @b! := 1
  (reset! r 5)
  ; ready
  @b! := 6
  (b!)
  ; done

  )

(tests
  "messing with latest"

  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (m/latest vector
               (ap (inc (?? >a)))
               (ap (dec (?? >a)))
               (m/watch (atom :x))))

  (def !z (>z #(println :ready) #(println :done)))
  @!z := [1 -1 :x]
  (reset! !a 1)
  @!z := [2 0 :x]
  (reset! !a 2)
  @!z := [3 1 :x]

  )

(tests
  "backpressure 1"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def !z (>a #(println :ready) #(println :done)))
  (reset! !a 1)
  (reset! !a 2)
  @!z := 2
  )

(tests
  "when does the work happen"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (m/relieve {} (ap (println 'work1) (let [a (?? >a)] (println 'work2) a))))
  (def !z (>z #(println :ready) #(println :done)))
  (reset! !a 1)
  (reset! !a 2)
  ;@!z := 2
  ; you can mix continuous and discrete, but the discrete parts will be run eagerly
  ; you can only be fully lazy if you have continuous operators all the way down, watch latest signal!, ie the good old spreadsheet model
  )

(tests
  "when does the work happen - lazy operators only"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (m/latest inc >a))
  (def !z (>z #(println :ready) #(println :done)))
  (reset! !a 1)
  (reset! !a 2)
  @!z := 3
  ; you can mix continuous and discrete, but the discrete parts will be run eagerly
  ; you can only be fully lazy if you have continuous operators all the way down, watch latest signal!, ie the good old spreadsheet model
  )

(tests
  "backpressure 3 w latest"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (m/latest inc >a))
  (def !z (>z #(println :ready) #(println :done)))
  (reset! !a 1)
  (reset! !a 2)
  (reset! !a 3)
  @!z := 4
  )

(tests
  "backpressure 3 w latest"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (ap (?? >a)))
  (def !z (>z #(println :ready) #(println :done)))
  (reset! !a 1)
  @!z := 0
  @!z := 1
  )

(tests
  "backpressure 3 works"
  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (ap (inc (?? >a))))
  (def !z (>z #(println :ready) #(println :done)))
  ;(reset! !a 1)
  @!z
  )


(tests
  "backpressure 3"

  (def !a (atom 0))
  (def >a (m/watch !a))
  (def >z (m/latest vector
               (ap (inc (?? >a)))
               (ap (dec (?? >a)))
               (m/watch (atom :x))))
  (def !z (>z #(println :ready) #(println :done)))

  ;(->>
  ;     (iterate inc 0)
  ;     (take 10)
  ;     (run! #(reset! !a %)))
  ;@!a := 9
  @!z                                                       ; := [9 7 :x]
  )

(defn bind [>a f] (ap (?! (f (?! >a)))))

(tests
  "discrete bind: use a >control signal to toggle between signals >p and >q at runtime"

  (def !p (atom 1))
  (def !q (atom 2))
  (def !control (atom :p))

  (def >p (m/watch !p))
  (def >q (m/watch !q))
  (def >control (m/watch !control))
  (def >cross (bind >control (fn [c]
                               (case c :p >p :q >q))))
  (def >z >cross #_(m/zip vector >p >q >cross))

  (def !z (>z #(println :ready) #(println :done)))

  @!z := 1

  (reset! !control :q)
  @!z := 2

  (reset! !control :p)
  @!z := 1

  )

(tests
  "discrete bind: harder version"

  (defn bind [>a f] (ap (?! (f (?! >a)))))

  (def !p (atom 1))
  (def !q (atom 2))
  (def !control (atom :p))

  (def >p (m/watch !p))
  (def >q (m/watch !q))
  (def >control (m/watch !control))
  (def >cross (bind >control (fn [c]
                                  (case c :p >p :q >q))))
  (def >z (m/latest vector >p >q >cross))

  (def !z (>z #(println :ready) #(println :done)))

  @!z := [1 2 1]

  (reset! !control :q)
  @!z := [1 2 2]                                            ; fail

  (reset! !control :p)
  @!z := [1 2 1]                                            ; fail

  )


(tests
  "Continuous bind, emulated with discrete bind"

  (defn bind' [>a f] (m/relieve {} (ap (?! (f (?! >a))))))

  (def !p (atom 1))
  (def !q (atom 2))
  (def !control (atom :p))

  (def >p (m/watch !p))
  (def >q (m/watch !q))
  (def >control (m/watch !control))
  (def >cross (bind' >control (fn [c]
                                  (case c :p >p :q >q))))
  (def >z (m/latest vector >cross >p >q))

  (def !z (>z #(println :ready) #(println :done)))

  @!z := [1 1 2]

  (reset! !control :q)
  @!z := [2 1 2]

  (reset! !control :p)
  @!z := [1 1 2]

  )


(comment

  (->> (iterate inc 0) (take 3) (run! #(reset! !a %)))
  @z!
  ;:= [[1 -1 :x] [2 0 :x] [3 1 :x]]
  )

(tests
  "bug in watch?"

  (def >z (m/watch (atom 1)))
  (def !z (>z #(println :ready) #(println :done)))
  @!z := 1

  )
