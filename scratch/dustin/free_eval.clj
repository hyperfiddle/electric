(ns dustin.free-eval
  (:require
    [datomic.api :as d]
    [meander.epsilon :as m :refer [match]]
    [minitest :refer [tests]]
    [hyperfiddle.api :as hf]))


(defn submission [needle]
  (d/q '[:find ?e . #_[?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?email]
         [(hyperfiddle.api/needle-match ?email ?needle)]]
    hf/*$* needle))

(tests
  (submission "bob") := [117592186045441 ...]
  (submission "ali") := 17592186045440
  )

(defn shirt-sizes [gender #_needle]
  (d/q
    '[:in $ ?gender
      :find ?e . #_[?e ...]
      :where
      [?e :scratch/type :scratch/shirt-size]
      [?e :scratch/gender ?gender]]
    hf/*$* gender))

(comment

  (def db-ident :db/ident)
  (defn foo [$ $user gender needle] (get e :db/ident))

  (via ()
    '[{(submissions >needle) [{:dustingetz/gender [:db/ident
                                                   {(shirt-sizes dustingetz/gender ~>needle) [*]}]}]}
      {(genders) [*]}]
    )

  := '{(submission needle) #:dustingetz{:gender #:db{:id 17592186045430}, :email "charlie@example.com"}
       (genders) {}}

  *this := {0 Stream
            1 Stream
            2 Stream}

  (M (M (M (M (M x)))))
  (M x)

  (M (M x))

  (via (Fusing)
    '(let [a 1
           b (via (list)
               (let [a (range 10)] a))
           (+ a 2)]
       (inc (inc b))))

  (via (TracingEval)

    )

  (let [a 1
        b (+ ~a 2)]
    (inc (inc b)))

  (bind 1        (fn [a]
  (bind (+ a 2)  (fn [b]
  (return (inc (inc b)))))))




  )











(comment

  (via (stream)
    (let [a 1
          b (+ ~a 2)]
      (inc (inc b))))
  
  (via (Stream)
    (let [>a (input)
          b (fmap (pure +) >a (pure 2))]
      (inc (inc b))))


  (via (tracing-eval)
    (let [a (eval (doto '1 println))
          b (eval (doto '(+ a 2) println))]
      (inc (eval (doto '(inc (eval (doto 'b println))) println)))))
  := 5
  *this := [{a 1} {(+ a 2) 3} {b 3} {(inc 3) 4} {(inc 4) 5}]

  (via (writer)
    (let [a ~1
          b ~(~+ ~a ~2)]
      (~inc ~(~inc ~b))))
  := 5
  *this := (bind* {a 1} {(+ a 2) 3} {b 3} {(inc 3) 4} {(inc 4) 5})

  (via (writer)
    '(let [a 1
           b (+ a 2)]
       (inc (inc b))))

  (via (Sci)
    '(let [a 1
           b (+ a 2)]
       (inc (inc b))))

  (via (RealClojure)                                        ; identity ?
    '(let [a 1
           b (+ a 2)]
       (inc (inc b))))

  (eval '(let [a 1
               b (+ a 2)]
           (inc (inc b))))

  (prewalk eval
    '(let [a 1
           b (+ a 2)]
       (inc (inc b))))

  (via (React)
    '(let [a 1
           b (+ a 2)]
       (inc (inc b))))

  (via ()
    (let [a 1
          b (+ a 2)]
      (inc (inc b))))










  )






(comment
  (via (reify
         :outer
         :inner
         )
    (let [a (pure 1)
          b ~a
          c 42]
      (inc ~(+ ~a c))))
  )


(comment

  (via (tracing-eval)
    (let [a 1
          b (+ a 2)]
      (inc (inc b))))
  := 5
  *this:= ...

  ; lift every sexp into a monad
  ; by adding an await at every possible slot
  :=
  (via (eval-identity)
    (let [a ~(pure 1)
          b ~(~+ ~a ~(pure 2))]
      (~inc ~(~inc ~b))))

  (deftype eval-identity [trace]
    Do-via
    (resolver-for [_]                                       ; "Clojure" monad
      {:Eval.pure   (fn [[_ v]] (do (set! *this update :trace conj [... v])
                                    v))
       :Eval.fmap   (fn [[_ f fv]] (f fv))             ; Clojure function call
       :Eval.fapply (fn [[_ af & avs]] (apply af avs))     ; Clojure apply
       :Eval.bind   (fn [[_ mv mf]] trace ... (mf (eval mv)))}))


  ; Trace the entire AST evaluation at every slot
  ; Since we await at every possible slot, there is no need to mark anything at all

  (defn f [>a]
    )

  (via (ClojureEval)
    (let [>a (pure 1)
          [*this >b] (via (stream)
                       [*this
                        (+ ~>a 2)])]
      (via (TracingStream *this)
        (inc ~(inc ~>b)))))
  := (pure 5)
  *this := [{a 1} {(+ a 2) 3} {b 3} {(inc 3) 4} {(inc 4) 5}]           ; Trace is returned out of band






  ; We can nest layers ...
  ; Remember, every slot is implicitly await
  (via (eval-trace)
    (let [a (input)]
      (via (eval-stream)
        (let [b (+ a 2)]
          (inc (inc b))))))
  := View
  *this := ...                                              ; trace doesn't see into eval-stream as we switched monads

  ; Finally, we can combine layers
  ; "await" is the primitive for crossing layers, which is why you need to mark things
  (via (reify Do-via (resovler-for [_]
                       {{
                         ; Outer monad
                         :Eval.eval prewalk
                         :Eval.pure   (fn [[_ v]] v)
                         :Eval.fmap   (fn [[_ f fv]] (f fv))
                         :Eval.fapply (fn [[_ af & avs]] (apply af avs))
                         :Eval.bind   (fn [[_ mv mf]] (mf mv))

                         ; Inner monad
                         :Do.pure     (fn [[_ v]] v)
                         :Do.fmap     (fn [[_ f fv]] (f fv))
                         :Do.fapply   (fn [[_ af & avs]] (apply af avs))
                         :Do.bind     (fn [[_ mv mf]] (mf mv))}}
                       ))
    '(let [a (input)
           b (+ a 2)]                                       ; both layers
      (inc (inc b))))


  (via (eval-queuewise)
    '(let [a 1
           b (+ a 2)]                                       ; both layers
       (inc (inc b))))

  (bind
    {a (input)}
    {b (+ a 2)}
    )


  (M (M (M x)))


  ; I think ^ will run the entire computation in trace while only the inner bit is in stream

  )
(comment


  (def >x
    (via (CardinalityManyHFQL)
      '[{(submissions >$ >needle) [{:gender [:db/ident
                                             {(shirt-sizes >$ gender) [*]}]}]}
        {(genders >$) [*]}]
      ))
  := View

  (run >x {'>needle (input)
           '>$ (input)})
  partitons := {1 View
                2 View
                3 View}


  := '{(submission needle) #:dustingetz{:gender #:db{:id 17592186045430}, :email "charlie@example.com"}}
  *this := partitions

  )
