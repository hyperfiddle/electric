(ns user.lang1
  "Photon language tutorial"
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hfdl.impl.util :as u]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [missionary.core :as m]))

(tests
  "smoke screen"
  (def dispose (run (! "hello world")))
  % := "hello world"
  (dispose)

  (def !x (atom 0))
  (def x (m/watch !x))
  (def dispose (run (! (let [x ~(m/watch !x)]
                         (+ x x)))))
  % := 0
  (swap! !x inc)
  % := 2
  (swap! !x inc)
  % := 4
  (dispose))

(tests
  "literals are lifted"
  (def dispose (r/run (! 1)))
  % := 1
  (dispose)

  (def dispose (r/run (! inc)))
  % := inc
  (dispose)

  "data literals lifted"
  (def dispose (r/run (! {:a 1})))
  % := {:a 1}
  (dispose)

  "globals lifted"
  (def a 1)
  (def dispose (r/run (! a)))
  % := 1
  (dispose)

  "clojure call"
  (def dispose (r/run (! (inc (inc 1)))))
  % := 3
  (dispose)

  "introduce a flow from foreign clojure call (e.g. entrypoint)"
  (def !x (atom 0))                                         ; atoms model variable inputs
  (def x (m/watch !x))                                      ; clojure flow derived from atom
  (def dispose (r/run (! ~x)))                              ; unquote foreign flow
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

; todo
;(tests
;  "reactive quote escapes to flow layer"
;  (def dispose
;    (r/run (! (let [x 1]
;                [(type x)
;                 (type #'x)]))))
;  % := [Long 'flow]
;  (dispose)
;
;  "special form for unquoting a quoted flow (monadic join)"
;  (def dispose (r/run (! (let [x #'1] ~x))))
;  % := 1
;  (dispose))

(tests
  "reactive addition (wrong way – two propagation frames, no sharing)"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def dispose (r/run (! (+ ~x ~x))))
  % := 0
  (swap! !x inc)
  % := 1
  % := 2
  (swap! !x inc)
  % := 3
  % := 4
  (dispose)

  "diamonds - let introduces shared nodes in the dag, no glitch"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def dispose (run (! (let [x ~(m/watch !x)]
                         (+ x x)))))
  % := 0
  (swap! !x inc)
  % := 2
  (swap! !x inc)
  % := 4
  (dispose))

(tests
  "reactive function call"
  (def !f (atom +)) (def f (m/watch !f))
  (def !x (atom 1)) (def x (m/watch !x))
  (def dispose (r/run (! (~f 0 ~x))))
  % := 1
  (swap! !x inc)
  % := 2
  (reset! !f -)
  % := -2
  (dispose))

(tests
  "foreign clojure collections. clojure.core/map is not incremental, the arguments are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (def dispose
    (r/run
      (! (let [f ~(m/watch !f)
               xs ~(m/watch !xs)]
           (clojure.core/map f xs)))))
  % := [2 3 4]
  (swap! !xs conj 4)
  % := [2 3 4 5]
  (reset! !f dec)
  % := [0 1 2 3]
  (dispose))

(tests
  "common core macros just work"
  (def dipsose
    (r/run
      (! (let [f ~(m/watch (atom inc))
               xs ~(m/watch (atom [1 2 3]))]
           (->> xs (map f))))))
  % := [2 3 4]
  (dispose)

  "destructuring"
  (def dispose
    (r/run (! (let [[a] ~(m/watch (atom [:a]))] a))))
  % := :a
  (dispose))

(tests
  "reactor termination"
  (def !x (atom 0))
  (def dispose
    (r/run (! ~(->> (m/watch !x) (m/eduction (take-while even?))))))
  % := 0
  (reset! !x 2)
  % := 2
  (reset! !x 1)
  % := ::rcf/timeout                                        ; never switched odd because it terminated
  (dispose))

(tests
  "reactive if"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !p (atom :p)) (def p (m/watch !p))
  (def !q (atom :q)) (def q (m/watch !q))
  (def dispose (r/run (! (if (odd? ~a) ~p ~q))))
  % := :p
  (swap! !a inc)
  % := :q
  (reset! !p :pp)
  (swap! !a inc)
  % := :pp
  (dispose))

(tests
  "reactive case"
  (def !a (atom 0)) (def a (m/watch !a))
  (def !p (atom :p)) (def p (m/watch !p))
  (def !q (atom :q)) (def q (m/watch !q))
  (def dispose (r/run (! (case ~a 0 ~p ~q))))
  % := :p
  (swap! !a inc)
  % := :q
  (reset! !q :qq)
  % := :qq
  (dispose))

(tests
  "reactive for"
  (def !xs (atom [1 2 3]))
  (def dispose (r/run (! (r/for [x ~(m/watch !xs)] (inc x)))))
  % := [2 3 4]
  (swap! !xs conj 4)
  % := [2 3 4 5]
  (dispose))

(tests
  "reactive for is differential (diff/patch)"
  (def !xs (atom [1 2 3]))
  (def dispose (r/run (! (r/for [x ~(m/watch !xs)] (! x)))))
  % := _                                                    ; its a race
  % := _
  % := _
  % := [1 2 3]
  (swap! !xs conj 4)
  % := 4
  % := [1 2 3 4]
  (swap! !xs pop)
  % := [1 2 3]
  (swap! !xs assoc 1 :b)
  % := :b
  % := [1 :b 3]
  (dispose))

(comment                                                    ; TODO
  "reactive for with keyfn"
  (def !xs (atom [{:id 1 :name "alice"} {:id 2 :name "bob"}]))
  (r/run (! (r/for :id [x ~(m/watch !xs)] (! x))))
  % := {:id 1 :name "alice"}
  % := {:id 1 :name "bob"}
  % := [{:id 1 :name "alice"} {:id 2 :name "bob"}]
  (swap! !xs assoc-in [0 :name] "ALICE")
  % := {:id 1 :name "ALICE"}
  % := [{:id 1 :name "ALICE"} {:id 2 :name "bob"}])

; node call (static dispatch)

; nested dags

(defnode g [x] x)                                           ; reactive fn (DAG). Compiler marks dag with meta
(defn f [x] x)                                              ; This var is not marked with meta

(tests
  "higher order dags"
  (def !x (atom 0))
  ~(r/run
     (let [ff (fn [x] x)                                    ; foreign clojure fns are useful, e.g. passing callbacks to DOM
           gg (node [x] x)                                  ; you almost always want this, not fn
           x ~(m/watch !x)]
       [(f x)                                               ; var marked
        (g x)                                               ; var says node
        (ff x)                                              ; Must assume interop, for compat with clojure macros
        (r/call gg x)                                       ; Must mark reactive-call
        (r/call (node [x] x) x)]))
  := [0 0 0 0 0])

(tests
  "reactive node closure"
  (def !x (atom 0))
  (def !y (atom 0))
  ~(r/run
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           _ 1
           _ ~(m/seed [1])
           f (node [needle] (+ y needle))                   ; constant signal
           g (if (odd? x) (node [needle] (+ y needle)) (node [needle] (+ y needle)))
           h ~(m/seed [(node [needle] (+ y needle))])]
       [(call f x)
        (call g x)
        (call h x)]))
  := [0 0 0])

(tests
  "reactive clojure.core/fn"
  ~(r/run
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           f (fn [needle] (+ y needle))]                    ; closure is rebuilt when flow changes
       ; (value is fully compatible with fn contract)
       ; the lambda is as variable as the var it closes over
       ; well defined. It's not allowed to use dataflow inside FN. Compiler can never reach it
       ; compiler will talk it to detect the free variables only
       (f x)))
  := [0 0])

; if we really want to be able to close over reactive values we
; need to solve the problem of dynamic extent. if a node closes over a
; reactive value and this value is destroyed due to a conditional switching,
; what happens ?
; In other words, there is a dag alive that needs X and X dies
; Should that dag be killed as well, or allowed to live with last known value of x, or undefined?

(tests
  "reactive closure over discarded var"
  (def !a (atom false))
  (def !b (atom 1))
  ~(r/run
     (call (let [!n (atom (node [] 0))]
             (when ~(m/watch !a)
               (let [x ~(m/watch !b)]
                 (reset! !n (node [] x))))
             ~(m/watch !n))))
  := 0
  (swap! !a not)
  := 1
  (swap! !a not)                                            ; watch !b is discarded
  := 'undefined)

(defnode fib [n]
  (case n
    0 0 1 1
    (+ (recur (- n 2))
       (recur (- n 1)))))

(tests
  "reactive recursion"
  (fib 3) := 5)

(declare pong)
(defnode ping [x] (case x 0 :done (pong (dec x))))
(defnode pong [x] (ping x))

(tests
  "mutual recursion"
  (ping 3) := :done)

(defnode boom [] (throw (Exception. "")))

(tests
  "exceptions"
  ; As a reference, see how regular Clojure exceptions are dynamic scoped
  (try
    (let [f (try (fn [] (/ 1 0))                            ; this exception will escape
                 (catch Exception _ :inner))]
      ; the lambda doesn't know it was constructed in a try/catch block
      (f))
    (catch Exception _ :outer))
  := :outer

  ~(r/run (r/try (boom) (r/catch Exception _))) := nil      ; reactive exception caught

  ~(r/run
     (r/try
       (let [f (r/try
                 (node [] (boom))                           ; reactive exception uncaught
                 (r/catch Exception _ :inner))]
         (f))
       (r/catch Exception _ :outer)))
  := :outer)

(defnode db)
(defnode foo [] db)

(tests
  "dynamic scope (note that try/catch has the same structure)"
  ~(r/run (r/with {db 0} (foo))) := 0                       ; binding available

  ~(r/run (r/with {db 1}
                  (let [n (r/with {db 0}
                                  (node [] (foo)))]         ; binding out of scope
                    (r/call n)))) := 1)

; first class node with static linking
; first class node with dynamic linking

(defnode foo)
(def ^:dynamic *db*)
(defn effect [] (println *db*))
(defnode bar [nf]
  [($ nf :x)
   (foo :x)
   (effect *db*)])

(tests
  (r/run (r/bind [*db* ~(m/seed 1)
                  (foo [x] *db*)]
                 (bar (node [x] *db*))))

  )

