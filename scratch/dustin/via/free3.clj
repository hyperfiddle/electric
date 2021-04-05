(ns dustin.via.free3
  "reverse polarity, no .
Effects are resolved from separate namespace? Nah"
  (:require
    [clojure.walk :refer [postwalk]]
    [meander.epsilon :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(defprotocol Interpreter
  ;(apply-id [_ as])
  (fapply [_ ms])
  (join [_ mma])
  (pure [_ c])
  (inject [M form])                                    ; inject
  (run [M ast])                                             ; traverse?
  (boot [M ast resolve! reject!] [M ast]))

(defn resolve' [M env x]
  {:post [(doto % (assert "unresolved symbol"))]}
  (assert (symbol? x) (str "resolve error, x: " (pr-str x) " is not a symbol"))
  (if (contains? env x)
    (pure M (env x)) (some-> (resolve x) deref)))

(defn lift-and-resolve [M effects x] #_(println 'lift-and-resolve x)
  (match x
    ('quote ?a) (pure M (lift-and-resolve M effects ?a))
    'quote      x
    ?a          (if (symbol? ?a) (resolve' M effects ?a) ?a)))

(deftype Eval-id [effects]
  Interpreter
  (fapply [_ ms] (apply (first ms) (rest ms)))
  (join [_ mma] mma)
  (pure [_ c] c)
  (inject [M x] (println 'inject x)
    (match x
      (`deref ?x) (join M ?x)
      (!xs ...) (let [mas (map #(lift-and-resolve M effects %) !xs)
                      mmb (fapply M mas)] (join M mmb))
      _ x))

  (run [M ast] (postwalk #(inject M %) ast))

  (boot [M ast resolve! reject!]
    ((m/reactor (run M ast)) resolve! reject!))

  (boot [M ast] (boot M ast #(println ::finished %) #(println ::crashed %))))

(def m-id (new Eval-id {}))

(tests
  "Identity monad"
  (fapply m-id [inc 1]) := 2
  (fapply m-id [+ 1 2]) := 3
  (join m-id 1) := 1
  (pure m-id 1) := 1)

(tests
  (def x 42)
  (def effects {'println (fn [& args] (println '! `(println ~@args)))})

  (tests "resolve"
    (resolve' {} 'inc) := inc
    (resolve' {} 'x) := 42
    (try (resolve' {} 'undef) (catch AssertionError _ :crash)) := :crash
    (resolve' {} 'println) := clojure.core/println)

  (tests "lift-and-resolve"
    (lift-and-resolve m-id {} 'println) := println
    (lift-and-resolve m-id {} ''println) := (pure m-id println)
    (lift-and-resolve m-id {} '1) := 1
    (lift-and-resolve m-id {} ''1) := (pure m-id 1)
    (lift-and-resolve m-id {} 1) := 1)                         ; fyi

  (tests
    (inject m-id '(inc x)) := 43
    (inject m-id '(inc 'x)) := 43
    ;(inject m-id '(@inc @x)) := 43
    ;(inject m-id '(inc. @x)) := 41
    ;(inject m-id '(@println. @x)) := nil
    (try (inject m-id '(undef x)) (catch Throwable _ :crash)) := :crash)

  (tests
    "smoke screen"
    (inject m-id x) := 42
    (inject m-id 'x) := 'x                             ; delayed eval
    (inject m-id '(inc x)) := 43
    (inject m-id '(inc x)) := 43
    ;(inject m-id '(@inc @x)) := 43
    ;(inject m-id '(@inc. @x)) := 41
    (inject m-id '(inc 1)) := 2
    (inject m-id '1) := 1)

  (tests
    "Watch out! Symbols are resolved during application!"
    (inject m-id 'x) := 'x #_42         ; !!!         ; values resolved in place
    (inject m-id 'inc) := 'inc          ; effect not resolved yet
    (inject m-id 'println) := 'println)

  (tests
    "Symbols resolved during application"
    (inject m-id '(inc x)) := 43
    (inject m-id '(inc 1)) := 2
    (inject m-id '(println x)) := nil   ; foreign side effect, not kosher
    ; 42 -- side effects aren't intercepted
    )

  ;(tests
  ;  "apply"
  ;  (inject m-id '(inc @1)) := 2
  ;  (inject m-id '(@inc @1)) := 2
  ;  (inject m-id '(@inc 1)) := 2
  ;  (inject m-id '(+ @1 @2)) := 3
  ;  (inject m-id '(+ @1 2)) := 3
  ;  (inject m-id '(@+ @1 2)) := 3
  ;  (inject m-id '(@+ 1 @2)) := 3
  ;  (try (inject m-id '(@X @1 2)) (catch Throwable _ :crash)) := :crash)

  ;(tests
  ;  "effects"
  ;  (inject m-id '(println 1)) := nil
  ;  ; 1 -- unmanaged
  ;  (inject m-id '(println 1 2)) := nil
  ;  ; 1 2 -- unmanaged
  ;  (inject m-id '(println 1)) := nil
  ;  ; (clojure.core 1)                               ; supervised effect
  ;  )

  ;(tests
  ;  "clojure interop"
  ;  (defn g [x y] (let [a x b a] (+ y b 100)))
  ;  (inject m-id '(@g @1 2)) := 103                           ; not clojure
  ;  (try (inject m-id '(let [] (@g @1 2))) (catch Throwable _ :UnsupportedOperation)) := :UnsupportedOperation
  ;  ; -- Wrong number of args (2) passed to: clojure.core/let
  ;  ; Can this be fixed with tools.analyser?
  ;  )

  ;(tests
  ;  "AST traversals"
  ;  (postwalk #(doto % println) '(println. @1)) := '(println. (clojure.core/deref 1))
  ;  (postwalk #(inject m-id %) '(println. @1))
  ;  ;! (clojure.core/println 1)
  ;  := nil
  ;  (postwalk #(inject m-id %) '(println. @(+ (inc 1) (inc 2))))
  ;  ;! (clojure.core/println 5)
  ;  := nil
  ;  (postwalk #(inject m-id %) '(pr-str @(println. @(inc 1))))
  ;  ;! (clojure.core/println 2)
  ;  := "nil")
  )

(tests
  (run m-id `~(inc x)) := 43
  (run m-id `~(inc 1)) := 2
  (run m-id (inc (inc x))) := 44
  (run m-id `(println ~1)) := nil
  (run m-id `(pr-str (println (inc 1)))) := "nil")

; Special forms and utilities for special forms
; (do not call from userland)

(defn if2 [test ma mb]
  (if test ma mb))

(defn bind [M ma eff]
  (join M (fapply M [(pure M #_kf eff) ma])))

;(defn bind-list [M seq-m-a eff] ; only used in foreach?
;  (map (fn [ma] (bind M ma eff)) seq-m-a))

;(defn extend-seq [M kf xs] ; List a -> List m a , but stabilized
;  ; request reactor reuse existing signal with same id
;  (map #(pure M (kf %) %) xs)
;  #_(map (fn [x] (m/signal! (kf x) (m/ap x))) xs))

(defn sequence' [M mas] ; list (m a) -> m (list a)
  (fapply M (cons (pure M list) mas))
  #_(m/signal! (apply m/latest list mas)))

;(defn foreach [M list-a eff]
;  (let [list-m-a (extend-seq M kf list-a)
;        list-m-b (map #(bind M % eff) list-m-a)
;        m-list-b (sequence M list-m-b)]
;    m-list-b))

; traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
(defn foreach [M list-a eff]
  (sequence' M (map eff list-a)))

; sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
; sequence :: Map (Flow a) -> Flow (Map a)
;
; traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
; traverse :: (a -> Flow b) -> Map a -> Flow (Map b)
;
; unsequence :: (Comonad w, Monad m) => w [a] -> [m a]
; unsequence = map return . extract
