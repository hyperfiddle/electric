(ns dustin.church-adt)


; data Boolean = True | False
(def ^{:type `Boolean} True (fn [a b] a))
(def ^{:type `Boolean} False (fn [a b] b))

(defn case [v
            & ks]
  (let [ctors (:constructors (:type (meta v)))
        branches (map vector ctors ks)
        k-to-run (get branches v)]
    (k-to-run)))

(comment
  (def v True)
  (case v
    True (fn [] "T")
    False (fn [] "F"))


  => (v "T" "F")
  => (True "T" "F")
  => "T"


  => (v (fn [] "T") (fn [] "F"))
  => (True (fn [] "T") (fn [] "F"))
  => (fn [] "T")

  (*1) := "T"

  )

; data ConsList a = Cons a (ConsList a) | Nil
(fn foo [v]
  (case v
    [(Cons a) (ConsList tail)] (fn [] (recur tail))
    nil (fn [] "done")))



(comment
  ;; M[Int] -> M[Int]
  (def inc-factorial
    (ifn [n]                                                 ; a -> Flow b
      (if (zero? n)
        x (* n ($ inc-factorial (dec n))))))

  (ifn main {'f $}
    []
    (let [x @(m/watch !input1)
          f (ifn factorial [n]
              (if (zero? n)
                x (* n ($ factorial (dec n)))))]
      (str ($ f @(m/watch !input2))))
    )

  ; special form FFN which is an incr lambda
  ; it's first class like fn is first class. Can bind in let
  ; reactive in same way as any other reactive val
  ; it's impossible for compiler to infer at a callsite that you are ordinary or reactive fn
  ;   design choices: 1) special form to mark as bind-ffn
  ;     2) lift all ordinary fns into reactive fns at compile time - WRONG, its just the opposite marking problem


  ; Prototype incr lambdas - ffn ; a generalization of deflow that suports recursion
  ; and first-class value (doesn't have to be top level)
  ; Ordinary fn calls with s-exprs is an ordinary call - e.g. str
  ; Incr fn calls prefix the s-expr with $
  ;     splice the dag produced by this ffn into the current dag
  ;     same as defflow but instead of inlining, we will track the dynamic context,
  ;     and the ffn will build its subdag inside this context


  )

(comment

  (def via eval2)

  (def fac (via (fn [n] ...)))

  (defn factorial [n]
    (via                                                    ; a -> m b
      (if (zero? n)
        x (* n ($ std-factorial (dec n))))))


  (via
    (let [x @(m/watch !input1)
          f (fn factorial [n]
              (via
                (if (zero? n)
                  x (* n ($ factorial (dec n))))))]
      (str ($ f @(m/watch !input2)))
      )

    ; Leo understands how it could be made to work
    ; Dustin asserts this clearly is well-typed

    ))











