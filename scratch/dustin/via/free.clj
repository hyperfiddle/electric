(ns dustin.via.free
  (:require
    [clojure.walk :refer [postwalk]]
    [meander.epsilon :as m]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]))

; The Free Monad is an abstract DAG which is evaluated for effect
; by providing at runtime an interpreter for it.

(defprotocol Evaluator
  (pure [_ c])                                              ; bad
  (fmap [_ f ma])
  (fapply [_ mf ma] [_ mf ma mb])
  (bind [_ ma f])
  (apply-id [_ f a] [_ f a b]))

(defn inject [effects f]
  {:post [%]}                                               ; all effects lifted
  (get effects f))

(tests
  (def evaluator (reify Evaluator
                   (pure [_ c] c)
                   (fmap [_ f ma] (f ma))
                   (fapply [_ mf ma] (mf ma))
                   (fapply [_ mf ma mb] (mf ma mb))
                   (bind [_ ma f] (f ma))
                   (apply-id [_ f a] (f a))
                   (apply-id [_ f a b] (f a b))))

  (def effects {'println (fn [& args] (println `(println ~@args)))})

  (inject effects 'println) := _
  (try (inject effects 'inc) (catch AssertionError _ :crash)) := :crash)

(tests
  (fmap evaluator inc 1) := 2
  (fapply evaluator + 1 2) := 3
  (apply-id evaluator inc 1) := 2
  (apply-id evaluator + 1 2) := 3
  (pure evaluator 1) := 1
  (bind evaluator 1 inc) := 2
  )

(defn lift-1 [x]
  (m/match x
    (`unquote ?a) ?a
    ?a (pure evaluator ?a)))                                ; symbol not resolved

(tests
  (lift-1 '1) := 1
  (lift-1 '~1) := 1
  (lift-1 1) := 1                                           ; fyi

  (lift-1 '~inc) := 'inc                                    ; unresolved
  )

(defn run-aps [[f & xs :as form]]
  (cons @(resolve (lift-1 f)) #_(inject (lift-1 f))
    (map lift-1 xs)))

(tests
  ; test in identity monad
  (run-aps '(inc 1)) := [inc 1]
  (run-aps '(inc ~1)) := [inc 1]
  (run-aps '(~inc 1)) := [inc 1]
  (run-aps '(~inc ~1)) := [inc 1]
  )

(defn unquote-in-form? [form]
  (m/find form
    (m/scan (`unquote _))                                   ; (inc ~a b) but not (a (b ~c)) nor (inc a) nor ~a
    true))

(defn bind-symbol? [s]
  (and (symbol? s)
    (= \. (last (name s)))))

(tests
  (bind-symbol? 'foo.) := true
  (bind-symbol? 'foo) := false
  (bind-symbol? []) := false
  (bind-symbol? 1) := false)

(defn form? [x]
  (and (seq? x) ; yep, i bet you're surprised
    (not (empty? x))))

(defn interpret-1 [evaluator effects x]
  (println 'interpret-1 x)
  (cond (form? x)
        (let [[f & [a b :as args]] x
              mb (m/match x                                 ; order matters

                   ; bind injects effects
                   ((m/pred bind-symbol? ?f) (`unquote ?ma))            ; arity
                   (bind evaluator ?ma
                     (inject effects (symbol (apply str (butlast (name ?f))))))

                   ; functor (could collapse into applicative)
                   ((m/pred symbol? ?f) (`unquote ?v))
                   (fmap evaluator ?f ?v)

                   ; applicative
                   (m/pred unquote-in-form?)
                   (apply fapply evaluator (run-aps x))

                   ; don't apply unquotes, leave in place
                   (`unquote ?x)
                   ?x #_(if (symbol? ?x) (env ?x) ?x)

                   ; foreign clojure call. No macros or clojure special forms in this layer
                   _
                   (apply apply-id evaluator @(resolve f)
                     (map (comp deref resolve) args)))]
          mb)

        ;(bind-symbol? x) (inject effects (symbol (apply str (butlast (name x)))))

        ;(symbol? x) @(resolve x)

        () x))

(comment
  (m/match '(println. ~1)
    ((m/pred bind-symbol? ?f) (`unquote ?ma))
    (bind evaluator ?ma (inject (symbol (apply str (butlast (name ?f))))))
    _ ::error)

  )

(tests
  (interpret-1 evaluator effects '1) := 1
  (def x 42)
  (interpret-1 evaluator effects {'x 42} 'x) := 42
  (interpret-1 evaluator effects 'inc) := 'inc                                ; not resolved yet
  (interpret-1 evaluator effects 'println) := 'println
  (interpret-1 evaluator effects '(inc 1)) := 2                               ; apply nodes resolve
  (interpret-1 evaluator effects '(println 1)) := nil                         ; foreign side effect, not kosher
  ; 1                                                       ; side effects aren't intercepted

  "apply"
  (interpret-1 evaluator effects '(inc ~1)) := 2
  (interpret-1 evaluator effects '(~inc ~1)) := 2
  (interpret-1 evaluator effects '(~inc 1)) := 2
  (interpret-1 evaluator effects '(+ ~1 ~2)) := 3
  (interpret-1 evaluator effects '(+ ~1 2)) := 3
  (interpret-1 evaluator effects '(~+ ~1 2)) := 3
  (interpret-1 evaluator effects '(~+ 1 ~2)) := 3
  (try (interpret-1 evaluator effects '(~X ~1 2)) (catch Exception _ :crash)) := :crash

  "effects"
  (interpret-1 evaluator effects '(println ~1)) := nil
  ; 1 -- unmanaged

  (interpret-1 evaluator effects '(println ~1 2)) := nil
  ; 1 2 -- unmanaged

  (interpret-1 evaluator effects '(println. ~1)) := nil
  ; (clojure.core/println 1)                               ; supervised effect
  ;(interpret-1 '(println. ~1)) := nil                       ; invalid

  "clojure interop"
  (defn g [x y] (let [a x b a] (+ y b 100)))                ; clojure here
  (interpret-1 evaluator effects '(~g ~1 2)) := 103                           ; not clojure

  (try (interpret-1 evaluator effects '(let [] (~g ~1 2))) (catch Exception _ :UnsupportedOperation)) := :UnsupportedOperation
  ; -- Wrong number of args (2) passed to: clojure.core/let
  ; Can this be fixed with tools.analyser?
  )

(defmacro interpret [evaluator effects ast]
  `(postwalk (partial interpret-1 ~evaluator ~effects &env)
     (quote ~ast)))

(tests

  (interpret-1 evaluator effects '(println. ~1)) := nil
  (postwalk #(interpret-1 evaluator effects %) '(println. ~1)) := nil
  (postwalk #(doto % println) '(println. ~1))

  (postwalk #(interpret-1 evaluator effects %) '(println. ~(+ (inc 1) (inc 2)))) := nil
  ; (clojure.core/println 5)
  (postwalk #(interpret-1 evaluator effects %) '(pr-str ~(println. ~(inc 1)))) := "nil"

  ;(postwalk interpret-1 `(println. ~1)) := "nil"

  (macroexpand-1 '(interpret evaluator effects (pr-str ~(println. ~(inc 1)))))
  (interpret evaluator effects (pr-str ~(println. ~(inc 1))))
  ; (clojure.core/println 2)
  := "nil"

  "inject effects"
  ; {'println (fn [& args] (apply println '! args))}
  (postwalk #(interpret-1 evaluator effects %)
    '(println. ~(+ (inc 1) (inc 2))))
  ; ! 5
  := nil


  ; sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
  ; sequence :: Map (Flow a) -> Flow (Map a)

  ; traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
  ; traverse :: (a -> Flow b) -> Map a -> Flow (Map b)

  )
