(ns user.electric.electric-missionary-interop
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import (clojure.lang IFn IDeref)))


(hyperfiddle.rcf/enable!)

(tests
  "introduce Missionary signal to Electric program"
  ; Electric programs compile down to Missionary signals,
  ; so Electric has native interop with Missionary primitives.
  (def !x (atom 0))
  (with (e/run (tap (let [X (m/watch !x)]  ; X is a recipe for a signal that is derived from the atom
                    (new X))))             ; construct actual signal instance from the recipe with (new)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "dataflow diamond"
  (def !x (atom 0))
  (with (e/run (let [X (m/watch !x)     ; missionary flow recipes are like Haskell IO actions
                     x (new X)]         ; construct flow recipe once
                 (tap (+ x x))))
    % := 0
    (swap! !x inc)
    % := 2
    (swap! !x inc)
    % := 4))

(tests "broken dataflow diamond"
  (def !x (atom 0))
  (with (e/run (let [X (m/watch !x)]
                 (tap (+ (new X) (new X))))) ; bad - two separate watch instances on the same atom
    % := 0
    (swap! !x inc)                     ; each instance fires an event resulting in two propagation frames
    % := 1                             ; bad
    % := 2
    (swap! !x inc)
    % := 3                             ; bad
    % := 4))

(tests
  "Q: What can e/fn and e/defn take as arguments, must they be continuous flows?"

  "You can only call an Electric fn with an Electric flow. (Literals and foreign globals are auto-lifted by compiler)"
  (def !x (atom 0))
  (with (e/run
          (tap
            (new                       ; call Electric fn with new
              (e/fn [x y] (+ x y))
              (e/watch !x)             ; reactive value
              42)))                    ; reactive 42 (lifted constant)
    % := 42)

  "To call an Electric fn with a Missionary flow, first join the Missionary flow into Electric with (new)
  and then call the Electric function with the joined flow."
  (with (e/run
          (tap
            (new                        ; call Electric fn with new
              (e/fn [x y] (+ x y))
              (new                      ; join Missionary flow into Electric with new
                (m/watch !x))           ; Missionary watch (flow as a value, needs to be joined)
              42)))
    % := 42)

  "call an Electric function with a missionary flow from an eduction"
  (with (e/run
          (tap
            (new
              (e/fn [x y] (+ x y))
              (new                                          ; join Missionary flow to Electric
                (->> (m/watch !x) (m/reductions + 0)))      ; Missionary flow as a value, needs to be joined
              42)))
    % := 42))

(tests
  "Electric can join both discrete and continuous missionary flows, but they must have an initial value"
  (with (e/run
          (tap
            (let [<x (m/watch !x)                           ; continuous flow as a value
                  >y (m/eduction (map inc) <x)]             ; discrete flow as a value
              (new
                (e/fn [x y] (+ x y))
                (new <x)                                    ; join continuous flow value
                (new >y)))))                                ; join discrete flow value
    % := 1))

(defn sleep-emit [delays]
  (m/ap (let [n (m/?< (m/seed delays))]
          (m/? (m/sleep n n)))))

(tests
  "Joining a discrete flow without an initial value is undefined"
  (with (e/run
          (tap
            (let [>x (sleep-emit [10 20])]                  ; event at t=10 and t=20, nothing at t=0
              (new
                (e/fn [x] (inc x))
                (new >x)))))
    ; no result
    % := ::rcf/timeout))

(tests
  "Electric thunks compile to Missionary continuous flows (!!)
  therefore they can be passed to Missionary's API"
  (def !x (atom 0))
  (with (e/run
          (tap
            (let [x  (e/watch !x)                           ; an Electric signal (continuous)
                  X  (e/fn [] x)                            ; Normally in Electric we would name a e/fn with capital X, but in this example
                  <x (e/fn [] x)                            ; <x is an appropriate name since it has the same type as any other continuous flow value
                  >y (m/eduction (dedupe) <x)]              ; apply lifted Electric signal <x to Missionary API (implicit conversion to discrete flow, eduction is eager)
              (new >y))))                                   ; rejoin (implicit conversion to continuous flow)
    % := 0))

(tests
  "Therefore, Electric thunk is monadic lift, new is monadic join"
  (def !x (atom 0))
  (with (e/run
          (tap
            (let [x   (e/watch !x)                          ; x :: m a
                  <x  (e/fn [] x)                           ; <x :: m m a
                  <<x (e/fn [] <x)                          ; <<x :: m m m a
                  <x  (new <<x)                             ; new is join :: m m a -> m a -- remove one level of monadic structure
                  x   (new <x)]                             ; join
              x)))
    % := 0))

(tests
  "Putting it all together - dedupe with Missionary transducer"
  (def !x (atom 0))
  (with (e/run
          (let [x (e/watch !x)]
            (tap (->> (e/fn [] x)                             ; lift
                    (m/eduction (dedupe))                   ; pass lifted value to missionary
                    (new)))))                               ; join back
    % := 0
    (swap! !x inc)
    % := 1
    (swap! !x identity)
    ; % := 1 -- skipped by dedupe
    (swap! !x inc)
    % := 2))

(tests
  "crashing a missionary flow"
  (defn boom! [x] (throw (ex-info "boom" {})) x)
  (with (e/run (tap (try (new (m/eduction (map boom!) (e/fn [] 1)))
                       (catch Throwable t ::boom))))
    % := ::boom))

; raw missionary continuous flow - see https://github.com/leonoel/flow
(def <f (fn [n t]
          (n) ; continuous flows notify immediately                                              ; notify 42 is ready
          (reify
            IFn (invoke [_])            ; no resources to release
            IDeref (deref [this] 42)))) ; never notify again

(tests
  "raw Missionary flow called from Electric"
  (def !it (<f #(tap ::notify)
               #(tap ::terminate)))
  % := ::notify
  @!it := 42
  (!it))

(comment
  "Document surprising edge case: Electric cannot use new to join foreign missionary flow from cc/def global"
  (tests (with (e/run (tap (new <f))) % := 42)) ; -- compile time error
  ; Syntax error macroexpanding hyperfiddle.electric/local
  ; Cannot call `new` on <f
  ;
  ; Why?
  ; It collides with Clojure constructor syntax. At compile time, Electric must decide if (new X) should
  ; compile to a Clojure constructor call, or a flow join.
  ;
  ; Clojure core syntax:
  "Clojure constructor syntax is defined only on static classes"
  (tests (new java.lang.Long 1) := 1) ; ✅

  "clojure cannot construct a dynamic class with new"
  (tests (new (identity java.lang.Long) 1) :throws Exception) ; -- compile time error
  ; Syntax error (IllegalArgumentException) compiling new
  ; Unable to resolve classname: (identity java.lang.Long)

  "another way to try to construct a dynamic class with new"
  (let [Klass java.lang.Long] (new Klass 1)) ; -- compile time error
  ; Syntax error (IllegalArgumentException) compiling new
  ; Unable to resolve classname: Klass

  ; See also: https://stackoverflow.com/questions/9167457/in-clojure-how-to-use-a-java-class-dynamically
  ; todo - show that clojurescript has the same rule

  ; Therefore, Electric inverses Clojure syntax.
  ; In Clojure, dynamic class construction syntax is undefined.
  ; In Electric, dynamic class construction syntax is given meaning: join flow.
  ; By inversing Clojure syntax, this is guaranteed not to break any existing Clojure code that compiles.

  (with (e/run (new <f))) ; -- Electric compiles this to a Clojure constructor call
  ; Syntax error macroexpanding hyperfiddle.electric/local
  ; Cannot call `new` on >F
  )

(tests
  "workaround syntax gap by making static calls dynamic"
  (with (e/run (tap (new (identity <f)))) % := 42)
  (with (e/run (tap (let [>F <f] (new >F)))) % := 42))
