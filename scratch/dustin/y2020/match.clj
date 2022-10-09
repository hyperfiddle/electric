(ns dustin.y2020.match
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn form? [x]
  (and (seq? x) ; yep, i bet you're surprised
       (not (empty? x))))

(tests
  (form? '(list)) := true
  (form? '#()) := true
  (form? '()) := false
  (form? 1) := false
  (form? {}) := false
  (form? []) := false)

(defmacro match [& xs]
  (letfn
    [(matcher? [x]
       (and (form? x)
         (= 'let (first x))))
     (denote [xs]
       (cond (empty? xs)
             `(throw (ex-info "no match" {}))
             (matcher? (first xs))
             (let [[_ bs & r] (first xs)
                   bs (destructure bs) ; todo
                   vars (take-nth 2 bs)]
               `(let [~@bs]
                  (if (every? (comp not nil?) [~@vars])
                    (do ~@r)
                    ~(denote (rest xs)))))
             () (throw (ex-info "unexpected" {:x (first xs)}))))]
    (if (not (matcher? (first xs)))
      (let [[v & xs] xs]
        `(let [~'&_ ~v]
           ~(denote xs)))
      (denote xs))))

(tests
  "the first value passed is also bound as &_ so you can branch on that directly"
  (match {:x 123}
    (let [{y :y} &_] y)
    (let [{x :x} &_] [:x 123]))
  := [:x 123]

  (match nil
    (let [{y :y} &_] y))
  :throws Exception

  "works with deep destructures"
  (match [{:a {:b {:c 3}} :d [4 44] :e 5 :f 6}]
    (let [[{:keys [:e :f]
            [d1 d2] :d
            {{c :c} :b} :a}] &_]
      [c [d1 d2] e f]))
  := [3 [4 44] 5 6])

(tests
  "first parameter is optional"
  (match
    (let [x nil] 1)
    (let [x 1 y nil] 2)
    (let [x 1 y 2] 3))
  := 3)

; if you add a helper for inspecting the type/tag for a value you get something like pattern matching
; (this matches the lambda calculus structure we went into before)
; (the let part is like a continuation that reads things out of the topic value)
