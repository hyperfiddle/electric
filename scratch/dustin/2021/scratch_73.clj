; Free Monad captures the idea of embedding, binding, control flow and nothing more
(via (tracing-eval)
  '(let [a 1
         b (f 10 a)]
     (return
       (inc b))))

(via (eval-identity)
  (let [a 1
        b (+ a 2)]
    (return
      (inc (inc b)))))
=> (bind ...)
=> 5

(def + (LiftAsync +))

(via (eval-async)
  (let [a 1
        b (+ a 2)]
    (inc (inc b))))

(via #_(eval-trace)
  (reify
    Do-via (resolver-for [_] {:X.inc ...}))
  (let [a 1
        b (+ a 2)]
    (dec (inc b))))

'[{(submissions needle) [{:gender [:db/ident
                                   {(shirt-sizes gender) [*]}]}]}
  {(genders) [*]}]

(via (eval-stream)
  (let [a 1
        b (+ a 2)]
    (inc (inc (+ a b)))))

(via (eval-queuewise)
  (let [a 1
        b (+ a 2)]
    (inc (inc b))))

(via (eval-list)                                            ; ?
  (let [a 1
        b (+ a 2)]
    (inc (inc b))))

(via (eval-list)                                            ; ?
  (for [a 1
        b ~(~+ ~a ~2)]
    (inc (inc b))))

(via (eval-queuewise-async)                                 ; ReactFiber
  (let [a 1
        b (+ a 2)]
    (inc (inc b))))

(via (hfql-identity)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])

(via (hfql-trace)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])

(via (hfql-stream)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])

(via (hfql-list)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])

(via (hfql-tracing-stream-list)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])
{0 ReactiveList
 1 ReactiveList
 2 ReactiveList}

(via (hfql-tracing-stream-list-fused-effects)
  '[{(submissions needle) [{:gender [:db/ident
                                     {(shirt-sizes gender) [*]}]}]}
    {(genders) [*]}])

(f ~(g ~x)) => ((comp f g) ~x)






'(mlet [a 1
        b (f 10 a)]
   (inc b))

(deftype M []
  Do-via
  (resolver-for [H]
    {:Nondet.or  (fn [[_ x y]] ...)
     :IO.println (fn [[_ & args]] (apply println args))}))

(via (M.)
  (let [x (Or 1 2)
        y (Or 2 3)]
    (println x y)))
=> 1 2, 1 3, 2 2, 2 3

=>
(via (M.)
  (mlet [x '(Or '1 '2)
         y '(Or 2 3)]
    '(println x y)))

=>
(clojure.core/eval
  (via (M.)
    (mlet [x (! :Nondet.or 1 2)
           y (! :Nondet.or 2 3)]
      (! :IO.println x y))))
*this => ...

(def ast '(Or '(Or 'True 'False) 'False))
;(eval ast) => (Or [1 2] [1 3] [2 2] [2 3])
(via (take-the-left) ast) => True

(or (or 1 2) (or 2 3)) => [1 2]


(via (M.)
  (let [x (Or 1 2)
        y (Or 2 3)]
    (println x y)))

(via (M.)
  (let [x (! :Nondet.or 1 2)
        y (! :Nondet.or 2 3)]
    (! :IO.println x y)))

(eval
  (bind (! :Nondet.or 1 2) (fn [x]
                             (bind (! :Nondet.or 2 3) (fn [y]
                                                        (! :IO.println x y)
                                                        ))))
  )



*this => ...
