(ns dustin.via2
  (:require
    [clojure.walk :refer [prewalk postwalk]]
    [meander.epsilon :as m :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]))


(comment

  (clojure.walk/postwalk prn
    '(1 (2 (4) (5)) (3)))
  (clojure.walk/prewalk prn
    '(1 (2 (4) (5)) (3)))

  (clojure.walk/postwalk (fn [form]
                           (prn form)
                           (eval form))
    '(+ (+ 1 2) (+ 3 4)))

  (defmulti bind (fn [mv mf]))
  (defmulti fmap (fn [f & avs]))
  (defmulti traverse (fn [f! ast]))                         ; fmap on a tree, sequenced for applicative effects

  (def rewrite-monad (r/until = (r/top-down (r/attempt rewrite-await))))
  (def rewrite-inject (r/until = (r/top-down (r/attempt ...))))
  (defn traverse [eval! ast]
    ((r/until = (r/bottom-up (r/attempt eval!)))
     ast))

  (declare !)

  (defprotocol Evaluator
    (traverse [_ ast] ast)
    (inject [_ form] form)
    (bind [_ mv mf] (mf mv))
    (fmap [_ f & avs] ...)                                  ; can't do protocol
    )

  ; Still need a free-form polymorphism construct
  ; inject is this

  (def maybe                                                ; no state
    #{(fn traverse [ast] ast)
      (fn bind [mv mf] (mf mv))
      (fn fmap [f & avs] (apply f avs))
      }

    )

  (defrecord maybe []                                       ; container for evaluator state
    Do-via
    (traverse [_ ast]
      (->> ast
        (macroexpand)
        (postwalk (fn [form]
                    (! form)))))                            ; !!
    (inject [_]
      {:Evaluator.bind  (fn [mv mf] (mf mv))                ; and trace
       :Evaluator.fmap  (fn [f & avs] (apply f avs))        ; and trace
       :Evaluator.try   (fn [] ...)
       :Evaluator.catch (fn [] ...)
       }))

  ; If every function is bound, then every function can trace inside bind
  ; which means every function has *this and must dispatch through !
  ;
  ; are we getting any value from the layers and do we need to?
  ; how does stream layer interact with eval layer?
  ; bind, do

  (defmacro ! [form]
    `(binding [*this (nth *stack (::nth R))]                ; why bother with this for (inc 42)
       (let [result ~form]                                  ; well, we can try/catch, intercept nil
         (set! *stack (assoc *stack (::nth R) *this))
         result)))

  ; This is a mistake
  ; Just rewrite the AST into :E.bind :E.fmap at all points using old pattern
  ; ! dispatches into E.bind etc
  ; Userland does not need !, why does user need to see the *this, it is managed by the monad

  (defmacro via [R ast]
    `(let [R# ~R
           fns# (resolver-for R#)]

       (assert (every? typed-tag? (keys fns#)))

       (let [n# (count *stack)
             resolvers#
             (->> fns#
               (group-by (comp tag-type key))
               (reduce-kv (fn [m# action-type# methods#]
                            (assoc m# action-type# (into {::nth n#} methods#)))
                 {}))]

         (binding [*stack (conj *stack R#)
                   *resolve (merge *resolve resolvers#)]

           (->> ast
             (rewrite-monad)                                ; (:Evaluator.bind ...)
             (rewrite-inject (inject evaluator))            ; (:Evaluator.try ...)
             (traverse evaluator))                          ; every form evals in a !
           ))))

  (defmacro via [evaluator ast]
    ; Wire the monad dispatch at compile time, the rest can happen at runtime
    #_(macroexpand ...)
    (binding [*stack ...]
      (traverse evaluator (rewrite-inject (inject evaluator) (rewrite-monad ast)))))

  (tests
    (via maybe
      (let [a 1
            b (+ a 2)]
        (inc (inc b))))
    := 5

    (via (Trace)
      (let [a 1
            b (+ a 2)]
        (inc (inc b))))
    := 5
    *this := [{a 1} {(+ a 2) 3} {b 3} {(inc 3) 4} {(inc 4) 5}]

    (via (Stream)
      (let [a 1
            b (+ a 2)]                                         ; (fapply (pure +) a (pure 2)) - be smarter here
        (inc (inc b))))
    := (Stream 5)

    (via (TracingStream)
      (let [a 1
            b (+ a 2)]
        (inc (inc b))))
    := (Stream 5)
    *this := [{a 1}
              {(+ a 2) 3}
              {b 3}
              {(inc 3) 4}
              {(inc 4) 5}]

    ))