(ns user.photon-missionary-interop
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m])
  (:import hyperfiddle.photon.Failure
           (clojure.lang IFn IDeref)))


(hyperfiddle.rcf/enable!)

(tests
  "introduce Missionary signal to Photon program"
  ; Photon programs compile down to Missionary signals and therefore Photon has native interop with Missionary primitives.
  (def !x (atom 0))
  (with (p/run (! (let [X (m/watch !x)]                     ; X is a recipe for a signal that is derived from the atom
                    (new X))))                              ; construct actual signal instance from the recipe with (new)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "dataflow diamond"
  (def !x (atom 0))
  (with (p/run (let [X (m/watch !x)                         ; missionary flow recipes are like Haskell IO actions
                     x (new X)]                             ; construct flow recipe once
                 (! (+ x x))))
    % := 0
    (swap! !x inc)
    % := 2
    (swap! !x inc)
    % := 4))

(tests "broken dataflow diamond"
  (def !x (atom 0))
  (with (p/run (let [X (m/watch !x)]
                 (! (+ (new X) (new X)))))                  ; bad - two separate watch instances on the same atom
    % := 0
    (swap! !x inc)                                          ; each instance fires an event resulting in two propagation frames
    % := 1                                                  ; bad
    % := 2
    (swap! !x inc)
    % := 3                                                  ; bad
    % := 4))

(tests
  "Q: What can p/fn and p/defn take as arguments, must they be continuous flows?"

  "You can only call a photon fn with a photon flow. (Literals and foreign globals are auto-lifted by compiler)"
  (def !x (atom 0))
  (with (p/run
          (!
            (new                                            ; call photon fn with new
              (p/fn [x y] (+ x y))
              (p/watch !x)                                  ; Photon flow from atom
              42)))                                         ; Photon literals are lifted
    % := 42)

  "To call a photon fn with a missionary flow, first join the missionary flow into photon with (new)
  and then call the photon function with the joined flow."
  (with (p/run
          (!
            (new                                            ; call photon fn with new
              (p/fn [x y] (+ x y))
              (new                                          ; join missionary flow into Photon with new
                (m/watch !x))                               ; missionary watch (flow as a value, needs to be joined)
              42)))
    % := 42)

  "call a photon function with a missionary flow from an eduction"
  (with (p/run
          (!
            (new
              (p/fn [x y] (+ x y))
              (new                                          ; join missionary flow to Photon
                (->> (m/watch !x) (m/reductions + 0)))      ; missionary flow as a value, needs to be joined
              42)))
    % := 42))

(tests
  "Photon can join both discrete and continuous missionary flows, but they must have an initial value"
  (with (p/run
          (!
            (let [<x (m/watch !x)                           ; continuous flow as a value
                  >y (m/eduction (map inc) <x)]             ; discrete flow as a value
              (new
                (p/fn [x y] (+ x y))
                (new <x)                                    ; join continuous flow value
                (new >y)))))                                ; join discrete flow value
    % := 1))

(defn sleep-emit [delays]
  (m/ap (let [n (m/?< (m/seed delays))]
          (m/? (m/sleep n n)))))

(tests
  "Joining a discrete flow without an initial value is undefined"
  (with (p/run
          (!
            (let [>x (sleep-emit [10 20])]                  ; event at t=10 and t=20, nothing at t=0
              (new
                (p/fn [x] (inc x))
                (new >x)))))
    ; no result
    % := ::rcf/timeout))

(tests
  "Photon thunks compile to missionary continuous flows (!!)
  therefore they can be passed to Missionary's API"
  (def !x (atom 0))
  (with (p/run
          (!
            (let [x  (p/watch !x)                           ; a photon signal (continuous)
                  X  (p/fn [] x)                            ; Normally in Photon we would name a p/fn with capital X, but in this example
                  <x (p/fn [] x)                            ; <x is an appropriate name since it has the same type as any other continuous flow value
                  >y (m/eduction (dedupe) <x)]              ; apply lifted Photon signal <x to missionary API (implicit conversion to discrete flow, eduction is eager)
              (new >y))))                                   ; rejoin (implicit conversion to continuous flow)
    % := 0))

(tests
  "Therefore, photon thunk is monadic lift, new is monadic join"
  (def !x (atom 0))
  (with (p/run
          (!
            (let [x   (p/watch !x)                          ; x :: m a
                  <x  (p/fn [] x)                           ; <x :: m m a
                  <<x (p/fn [] <x)                          ; <<x :: m m m a
                  <x  (new <<x)                             ; new is join :: m m a -> m a -- remove one level of monadic structure
                  x   (new <x)]                             ; join
              x)))
    % := 0))

(tests
  "Putting it all together - dedupe with missionary transducer"
  (def !x (atom 0))
  (with (p/run
          (let [x (p/watch !x)]
            (! (->> (p/fn [] x)                             ; lift
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
  "crashing a missionary flow is fatal"
  (defn boom! [x] (throw (ex-info "boom" {})) x)
  (with (p/run (! (try (new (m/eduction (map boom!) (p/fn [] 1)))
                       (catch Throwable t ::boom))))
    ; % := :boom -- uncaught
    % := ::rcf/timeout))

(tests
  "inject Photon exception from missionary flow"
  (defn boom! [x] (Failure. (r/error "boom")))

  (with (p/run (! (try (new (m/eduction (map boom!) (p/fn [] 1)))
                       (catch Throwable t ::boom))))
    % := ::boom))


; raw missionary flow - see https://github.com/leonoel/flow
(def >F (fn [n t]
          (n)                                               ; notify 42 is ready
          (reify
            IFn (invoke [_])                                ; no resources to release
            IDeref (deref [this] 42))))                     ; never notify again

(tests
  "raw missionary flow called from photon"
  (def !it (>F #(! ::notify)
               #(! ::terminate)))
  % := ::notify
  @!it := 42
  (!it))

(tests
  ; (p/run (new >F)) -- fails due to syntax collision with Clojure new
  (with (p/run (! (new (identity >F))))                     ; workaround syntax gap
    % := 42))

(comment
  ; Explanation of Clojure syntax
  ; Clojure constructor syntax is defined only on static classes
  (new java.lang.Long 1)                                    ; works
  (new (identity java.lang.Long) 1)                         ; fails
  (let [Klass java.lang.Long] (new Klass 1))                ; fails
  ; therefore Photon can shadow the entire dynamic usage of new without
  ; colliding with Clojure functionality.
  ; That's how it works today. Todo improve the coverage with additional syntax.
  )
