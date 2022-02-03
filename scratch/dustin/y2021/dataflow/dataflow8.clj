(ns dustin.dataflow8
  "Scraps not processed yet")


(comment

  ; trying to understand physical types that the compiler produces

  ; biz effect
  (defn submissions! [x] :! a -> Flow b
    (m/via m/cpu (d/q ...)))
  := (Flow [{:db/id 1 :name x}])

  ; submissions :: a -> m b
  ; x :: m a
  ; x >>= submissions!


  ; a -> Flow b is the wrong type!
  (defn submissions! [x] :! Flow a -> Flow b
    (m/signal! (m/cp ... [{:db/id 1 :name x}])))

  := (Flow [{:db/id 1 :name x}])


  ; submissions! :: m a -> m b
  ; x :: m a
  ; submissions! (extend x)

  ; (extend x) :: m (m a)
  ; incr-call submissions (extend x) :: m b
  ; incr-call :: m (m a) -> m (m a -> m b) -> m b
  ; incr-call :: m (m a) -> m (m a -> m b) -> m b

  ; its close to pass by reference

  ; without extend

  ; submissions! :: m a -> m b
  ; x :: m a

  ; incr-call submissions x :: m b
  ; incr-call :: (m a) -> m (m a -> m b) -> m b


  ; incr-call :: (m a) -> m (m a -> m b) -> m b
  ; (>>=)     :: (m a) ->   (  a -> m b) -> m b
  ; (<*>)     :: (m a) -> m (  a ->   b) -> m b
  ; fmap      :: (m a) ->   (  a ->   b) -> m b

  ; https://wiki.haskell.org/If-then-else

  ;                               (>>=)     :: (m a) ->   (  a -> m b) -> m b
  ;                               (<*>)     :: (m a) -> m (  a ->   b) -> m b
  ;                               fmap      :: (m a) ->   (  a ->   b) -> m b


  )

(comment

  ; no way to ... everythign is implicitly joined
  ; instad of putting deref where you want, there's deref's everywhere
  ; need to be explicit where you don't want deref


  (def program '(if' (odd?' a) b 1))

  (eval program {'a 1 'b :b}) := :b
  (dataflow-eval program {'a (m/watch (atom 1))
                          'b (m/watch (atom :b))
                          'odd?' (m/watch (atom odd?))})
  := (Signal :b)

  odd?'  :! m (Num -> Boolean)
  odd?'' :! m (Num -> m Boolean)



  ; What is clojure compatibility?
  ; Expect from HFDL to be able to use existing macros including clojure.core macros, destructuring
  ; if use destructure in let, desugar to get/next/first
  ; s-expressions and ordinary let bindings
  ; expect this to work incrementally – if the input changes, then the result will be propogated to the apply node
  ; that computes the next value (call to next). Want the compiler to figure out that next is clojure.core fn , it's a
  ; constant, it can be optimized

  (def )
  (let [[a b] a]
    )


  )


(comment


  (dataflow (inc x))
  ;x -> (inc %)

  (main
    '(m/latest apply (get env 'inc) [(get env 'x)])
    {'inc (m/watch (atom clojure.core/inc))
     'x   (m/watch (atom 0))})






  (let [mutable! (fn [seed]
                   (m/watch (atom seed)))]
    (compile-dataflow
      (def f (fn [y]
               (let [w (mutable! 0)]
                 (if (odd? y)
                   (inc y)
                   (dec y)))))
      (inc (new f (inc x)))
      ))


  (def f :! a -> m b
    (compile-dataflow
      (fn [y]
        (let [w (mutable! 0)]
          (if (odd? y)
            (inc y)
            (dec y))))))

  (compile-dataflow (inc (new f (inc x))))
  (compile-dataflow (inc (join (f (inc x)))))
  (compile-dataflow (inc (join (submissions-query! (inc x)))))

  (defn submissions-query! :! a -> m b
    [x]
    (m/via m/cpu (d/q ...)))




  (let [mutable! (fn [seed]
                   (m/watch (atom seed)))
        f (fn [y]
            (atom ...)
            (m/signal!
              (cp
                #_(let [w (mutable! 0)]
                    (if (odd? y)
                      (inc y)
                      (dec y))))))]
    (dataflow
      (inc (f. (inc x)))
      ))


  reify























  (dataflow

    (inc (inc (inc x)))

    )
  x ->
  (inc %) ->
  (inc %) ->
  (inc %)

  (run
    '(m/latest apply (get env 'inc) [(get env 'x)])

    {'inc ... 'x ...})


  (dataflow

    (def f (ifn [y]
             (if (odd? y)
               (inc y)
               (dec y))))
    (inc (f. (inc x)))

    )

  (dataflow


    (def f ...)
    (inc (join (f (inc x))))

    )


  (fmap x (fn [%]

            ))







  (+ (inc a) (inc a)))                                      ; diamond



