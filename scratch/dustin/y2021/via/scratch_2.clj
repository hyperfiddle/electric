
(comment

  (defn foo [a b]
    `(+ ~a ~b))

  (defn ^:special foo [a b]
    `(+ ~a ~b))

  (defn render-table [a b]
    `(div (map render-row ~(inc a)) ~b))

  (defn render-row [a]
    `(div ~a))

  (def a 1)
  (def b 2)
  (render-entrypoint! js/document.body `(render-table ~a ~b))
  (dataflow `(render-table ~a ~b))

  )

(defn println2 [x & args]
  (m/ap (apply println "!" x args)))

(comment
  (m/sp (println 1)) '(println 1)
  (hf/dag (println 1))
  (clojure (println 1))
  '(println 1)
  (eval '(fn [] (println 1)))

  (defn f [a] `(println2 ~(inc a)))
  (defn f [a] [println2 (inc a)])

  (def !a (atom 0))
  (def a (m/watch !a))
  (interpret-dataflow `(f ~a))


  `(f ~a) := '(scratch/f _)
  [f a] := [scratch/f _]

  (defn)
  (defmacro)

  (defn apply* [[f & args :as form]]
    (m/ap (m/?! (m/?! (apply m/latest #_#(% %&) f args)))))

  (defn compile* [[f & args :as form]]
    )

  ; don't want to react to this value, want the reactive thing itself
  ; (quote)


  ; how is unquote like join?
  ; both produce a value from an expression only when you ask for it

  ;clojure
  (defmacro if2 [p? a b] `(if ~p? ~a ~b))
  (defn if3 [p? a b] (if p? a b))

  `(if3 )

  (tests
    (if2 (odd? 0) (inc 1) (dec 1)) := 2
    `~(if3 true :a :b)

    ; unquote works like macros - two ways to achieve same thing and underlying problem is templating


    )




  )

(comment


  )




(comment
  (leo-lang

    (defn foo [a b] (dag (+ a b)))
    ; a -> m b, and up to user to make it lifted if they need it

    (defnode foo2 [a b] ...)
    (defmacro foo3 [a b] `(+ ~a ~b))

    (foo x y)

    (foo (extend x) (extend y))
    ((invert-extend foo) x y)
    (foo x y)

    (m/sp (println 1)) '(println 1)
    (hf/dag (println 1))
    (clojure (println 1))
    '(println 1)
    (eval '(fn [] (println 1)))




    (defmacro clojure [body] body)

    (def x (m/watch ...))

    (dataflow (let [y @x] (foo (extend y) (extend y))))
    ;(dataflow (foo2 (extend x) (extend x)))
    (dataflow (foo3 x x))

    (defn foo [a b] `(println ~a ~b))
    (hf/dag `(foo x x))


    ; quote vs sample
    ; are these all the same

    )

  (inner-lang
    (defn foo [a b] ...)

    (foo x y)
    )

  (host-lang
    (mapply foo (pure 1) (pure 2))))