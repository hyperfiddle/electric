(ns dustin.via.free
  (:require
    [clojure.walk :refer [postwalk]]
    [meander.epsilon :as m]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]))

; The Free Monad is an abstract DAG which is evaluated for effect
; by providing at runtime an interpreter for it.
;
; (~f. ~a ~b 3)
; ~ marks wrapped values
; . marks join nodes

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

(defprotocol Interpreter
  (fapply [_ mf ma] [_ mf ma mb])
  ;(apply-id [_ f a] [_ f a b])
  (join [_ mma])
  (pure [_ c]))                ; not actually needed

(def eval-id
  (reify Interpreter
    (fapply [_ mf ma] (mf ma))
    (fapply [_ mf ma mb] (mf ma mb))
    ;(apply-id [_ f a] (f a))
    ;(apply-id [_ f a b] (f a b))
    (join [_ mma] mma)
    (pure [_ c] c)))

(tests
  "Identity monad"
  (fapply eval-id inc 1) := 2
  (fapply eval-id + 1 2) := 3
  ;(apply-id eval-id inc 1) := 2
  ;(apply-id eval-id + 1 2) := 31
  (join eval-id 1) := 1
  (pure eval-id 1) := 1)

(defn my-resolve [effects s]
  {:post [%]}             ; unresolved effect
  (if (bind-symbol? s)
    (effects (symbol (apply str (butlast (name s)))))       ; managed
    (some-> (resolve s) deref)))                            ; foreign

(tests
  (def x 42)
  (def effects {'println (fn [& args] (println '! `(println ~@args)))})
  (my-resolve {} 'inc) := inc
  (my-resolve {} 'x) := 42
  (try (my-resolve {} 'undef) (catch AssertionError _ :crash)) := :crash
  (my-resolve {} 'println) := clojure.core/println
  (try (my-resolve {} 'println.) (catch AssertionError _ :crash)) := :crash
  (my-resolve effects 'println.) := (effects 'println))

(defn lift-and-resolve [interpreter effects x]
  ; ~f. -> (unquote f.)
  ; x is like 'a or '(clojure.core/unquote a)
  (m/match x
    (`unquote ?a) (if (symbol? ?a) (my-resolve effects ?a) ?a)
    ?a (pure interpreter (if (symbol? ?a) (my-resolve effects ?a) ?a))))

(tests
  (lift-and-resolve eval-id effects '~println.) := (effects 'println)
  (lift-and-resolve eval-id effects 'println.) := (effects 'println)
  (lift-and-resolve eval-id effects '~println) := println   ; foreign
  (lift-and-resolve eval-id effects '1) := 1
  (lift-and-resolve eval-id effects '~1) := 1
  (lift-and-resolve eval-id effects 1) := 1                 ; fyi
  )

; form is a quoted apply node, like '(~f. a ~b 1)
; resolve symbols during apply
; either resolve the effect or resolve the foreign fn

(defn bind-form? [x]
  (m/match x
    ((`unquote (m/pred bind-symbol? ?s)) & _) true
    ((m/pred bind-symbol? ?s) & _) true
    _ false))

(tests
  (bind-form? '(f x)) := false
  (bind-form? '(f. x)) := true
  (bind-form? '(~f x)) := false
  (bind-form? '(~f. x)) := true)

(defn run-apply [interpreter effects [f & [a b :as args] :as form]]
  (println 'run-apply form)
  (let [mas (cons (lift-and-resolve interpreter effects f)
                (map #(lift-and-resolve interpreter {} %) args))
        ;_ (println 'run-apply-mas mas)
        mb (apply fapply interpreter mas)] ; args are not effects
    (if (bind-form? f) (join interpreter mb) mb)))

(tests
  (run-apply eval-id {} '(inc x)) := 43
  (run-apply eval-id {} '(inc ~x)) := 43
  (run-apply eval-id {} '(~inc ~x)) := 43
  (run-apply eval-id {'inc dec} '(inc. ~x)) := 41
  (run-apply eval-id {'println println} '(~println. ~x)) := nil
  (try (run-apply eval-id {'inc inc} '(undef. ~x)) (catch Throwable _ :crash)) := :crash)

(defn interpret-1 [interpreter effects x]
  ;(println 'interpret-1 x)
  (m/match x

    (`unquote ?x) ?x ; leave in place to be interpreted during application

    (!xs ...) (run-apply interpreter effects x)

    _ x))

(tests
  "smoke screen"
  (interpret-1 eval-id {} 'x) := 'x                         ; delayed eval
  (interpret-1 eval-id {} '(inc x)) := 43
  (interpret-1 eval-id {} '(inc ~x)) := 43
  (interpret-1 eval-id {} '(~inc ~x)) := 43
  (interpret-1 eval-id {'inc dec} '(~inc. ~x)) := 41
  (interpret-1 eval-id {} '(inc 1)) := 2)

(tests
  ;(defn env [k] (get {'x 42} k k #_@(resolve k)))           ;
  ; symbols are foreign?

  (interpret-1 eval-id effects '1) := 1

  "Watch out! Symbols are resolved during application!"
  (interpret-1 eval-id effects 'x) := 'x #_42         ; !!!         ; values resolved in place
  (interpret-1 eval-id effects 'inc) := 'inc          ; effect not resolved yet
  (interpret-1 eval-id effects 'println) := 'println

  "Symbols resolved during application"
  (interpret-1 eval-id effects '(inc x)) := 43
  (interpret-1 eval-id effects '(inc 1)) := 2
  (interpret-1 eval-id effects '(println x)) := nil   ; foreign side effect, not kosher
  ; 42 -- side effects aren't intercepted

  "apply"
  (interpret-1 eval-id effects '(inc ~1)) := 2
  (interpret-1 eval-id effects '(~inc ~1)) := 2
  (interpret-1 eval-id effects '(~inc 1)) := 2
  (interpret-1 eval-id effects '(+ ~1 ~2)) := 3
  (interpret-1 eval-id effects '(+ ~1 2)) := 3
  (interpret-1 eval-id effects '(~+ ~1 2)) := 3
  (interpret-1 eval-id effects '(~+ 1 ~2)) := 3
  (try (interpret-1 eval-id effects '(~X ~1 2)) (catch Throwable _ :crash)) := :crash

  "effects"
  (interpret-1 eval-id effects '(println ~1)) := nil
  ; 1 -- unmanaged
  (interpret-1 eval-id effects '(println ~1 2)) := nil
  ; 1 2 -- unmanaged
  (interpret-1 eval-id effects '(println. ~1)) := nil
  ; (clojure.core/println 1)                               ; supervised effect

  "clojure interop"
  (defn g [x y] (let [a x b a] (+ y b 100)))
  (interpret-1 eval-id effects '(~g ~1 2)) := 103                           ; not clojure
  (try (interpret-1 eval-id effects '(let [] (~g ~1 2))) (catch Throwable _ :UnsupportedOperation)) := :UnsupportedOperation
  ; -- Wrong number of args (2) passed to: clojure.core/let
  ; Can this be fixed with tools.analyser?

  "AST traversals"
  (postwalk #(doto % println) '(println. ~1)) := '(println. (clojure.core/unquote 1))
  (postwalk #(interpret-1 eval-id effects %) '(println. ~1))
  ;! (clojure.core/println 1)
  := nil
  (postwalk #(interpret-1 eval-id effects %) '(println. ~(+ (inc 1) (inc 2))))
  ;! (clojure.core/println 5)
  := nil
  (postwalk #(interpret-1 eval-id effects %) '(pr-str ~(println. ~(inc 1))))
  ;! (clojure.core/println 2)
  := "nil"

  ; sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
  ; sequence :: Map (Flow a) -> Flow (Map a)

  ; traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
  ; traverse :: (a -> Flow b) -> Map a -> Flow (Map b)
  )

;(defn interpret [interpreter effects ast])

(defmacro interpret [interpreter effects ast]
  `(postwalk (partial interpret-1 ~interpreter ~effects)
     (quote ~ast)))

(tests
  (interpret eval-id {} (inc x)) := 43
  (interpret eval-id {} (inc 1)) := 2
  (interpret eval-id {} (~inc ~(inc ~x))) := 44
  (interpret eval-id effects (println. ~1)) := nil
  (interpret eval-id effects (pr-str ~(println. ~(inc 1)))) := "nil")
