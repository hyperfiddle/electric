(ns user.lang
  "Photon language tutorial"
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [missionary.core :as m]))


(tests
  "literals are lifted"
  (def dispose (r/run (! 1)))
  % := 1
  (dispose)

  "data literals"
  (def dispose (r/run (! {:a 1})))
  % := {:a 1}
  (dispose)

  "globals lifted"
  (def a 1)
  (def dispose (r/run (! a)))
  % := 1
  (dispose)

  (def dispose (r/run (! inc)))
  % := inc
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
  (def dispose
    (r/run (! (let [x ~(m/watch !x)]
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
  "lazy"
  (def dispose (r/run (! (if false (! :a) (! :b)))))
  % := :b
  % := :b
  (dispose))

(tests
  "lazy"
  (def dispose (r/run (! (if false (! :a) (! :b)))))
  % := :b
  % := :b
  (dispose))

(comment
  "control flow implemented with lazy"
  (defnode if2 [x a b] (get {true a false b} (boolean x)))
  (def dispose (r/run (! (if2 false (! :a) (! :b)))))
  ;% := :a
  % := :b
  % := :b
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

(comment
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
  #_(dispose))                                              ; broken dispose fixme

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

(tests
  "reactive do"
  (def !x (atom 0))
  (def dispose (r/run (do (! :a) (! ~(m/watch !x)))))
  ; do is not monadic sequence, we considered that
  ; It's an incremental computation so only rerun what changed in our opinion
  % := :a
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests
  "reactive doto"
  (defn MutableMap [] (new java.util.HashMap))
  (defn PutMap [!m k v] (.put !m k v))
  (defn Ref [] (new Object))
  (def !z (atom 0))
  (def !x (atom 0))
  (def dispose
    (r/run
      #_(doto (element "input")
          (set-attribute! "type" "text")
          (set-attribute! "value" x))
      (! (doto (MutableMap)                                 ; the doto is incrementalized
           (PutMap "a" (swap! !z inc))                      ; detect effect
           (PutMap "b" ~(m/watch !x))))))
  % := {"a" 1
        "b" 0}
  (swap! !x inc)
  % := ::rcf/timeout                                        ; no further sample, the map hasn't changed
  (dispose))

; node call (static dispatch)
(comment
  "defnode is defn for reactive fns"
  ; best example of this is hiccup incremental maintenance

  (defnode !')
  (defnode div [child] (!' child) [:div child])
  (defnode widget [x]
    (div [(div x) (div :a)]))

  (def !x (atom 0))
  (def dispose (r/run (! (r/bind [(!' [x] (rcf/! x))]
                                 (widget ~(m/watch !x))))))
  % := 0
  % := :a
  % := [[:div 0] [:div :a]]
  % := [:div [[:div 0] [:div :a]]]
  (swap! !x inc)
  % := 1
  ; no :a
  % := [[:div 1] [:div :a]]
  % := [:div [[:div 1] [:div :a]]]
  (dispose))

(tests
  "node call vs fn call"
  (defnode g [x] x)                                         ; reactive fn (DAG). Compiler marks dag with meta
  (defn f [x] x)                                            ; This var is not marked with meta
  (def !x (atom 0))
  (def dispose
    (r/run
      (! (let [x ~(m/watch !x)]
           [(f x)
            (g x)
            ]))))
  % := [0 0]
  (dispose))

(comment
  "higher order dags"
  (def !x (atom 0))

  (def dispose
    (r/run
      (! (let [ff (fn [x] x)                                ; foreign clojure fns are useful, e.g. passing callbacks to DOM
               gg (node [x] x)                              ; you almost always want this, not fn
               x ~(m/watch !x)]
           [(f x)                                           ; var marked
            (g x)                                           ; var says node
            (ff x)                                          ; Must assume interop, for compat with clojure macros
            ($ gg x)                                        ; Must mark reactive-call
            ($ (node [x] x) x)]))))
  % := [0 0 0 0 0]
  (dispose))

(comment
  "reactive node closure"
  (def !x (atom 0))
  (def !y (atom 0))
  (def dispose
    (r/run (! (let [x ~(m/watch !x)
                    y ~(m/watch !y)
                    _ 1
                    _ ~(m/seed [1])
                    f (node [needle] (+ y needle))          ; constant signal
                    g (if (odd? x) (node [needle] (+ y needle))
                                   (node [needle] (+ y needle)))
                    h ~(m/seed [(node [needle] (+ y needle))])]
                [($ f x)
                 ($ g x)
                 ($ h x)]))))
  % := [0 0 0]
  (dispose))

(comment
  "reactive clojure.core/fn"
  (def !x (atom 0))
  (def !y (atom 0))
  (def dispose
    (r/run
      (! (let [x ~(m/watch !x)
               y ~(m/watch !y)
               f (fn [needle] (+ y needle))]                ; closure is rebuilt when y changes
           ; (value is fully compatible with fn contract)
           ; the lambda is as variable as the var it closes over
           ; well defined. It's not allowed to use dataflow inside FN. Compiler can never reach it
           ; compiler will walk it to detect the free variables only
           (f x)))))
  % := 0
  (swap! !y inc)
  % := 1
  (swap! !x inc)
  % := 2
  (dispose))

; if we really want to be able to close over reactive values we
; need to solve the problem of dynamic extent. if a node closes over a
; reactive value and this value is destroyed due to a conditional switching,
; what happens ?
; In other words, there is a dag alive that needs X and X dies
; Should that dag be killed as well, or allowed to live with last known value of x, or undefined?

(comment
  "reactive closure over discarded var"
  (def !a (atom false))
  (def !b (atom 1))
  (def dispose
    (r/run
      (! ($                                                 ; call a closure from outside the extent of its parent
           (let [!n (atom (node [] 0))]
             (when ~(m/watch !a)
               (let [x ~(m/watch !b)]
                 (reset! !n (node [] x))))                  ; use mutation to escape the extent of the closure
             ~(m/watch !n))))))
  := 0
  (swap! !a not)
  := 1
  (swap! !a not)                                            ; watch !b is discarded
  := ::rcf/timeout)

(defnode fib [n]
  (case n
    0 0 1 1
    (+ (fib (- n 2))
       (fib (- n 1)))))

(tests
  "reactive recursion"
  (def !x (atom 5))
  (def dispose (r/run (! (fib ~(m/watch !x)))))
  % := 5
  (swap! !x inc)
  ; this will reuse the topmost frame, it is still naive though
  % := 8
  (dispose))

(defnode fib [n]
  (case n
    0 0 1 1
    ; these are not tail calls
    (+ (recur (- n 2))                                      ; non-tail call is legal in reactive clojure
       (recur (- n 1)))))

(comment
  "recur special form"
  (def !x (atom 5))
  (def dispose (r/run (! (fib ~(m/watch !x)))))
  % := 5
  (swap! !x inc)
  ; this will reuse the topmost frame, it is still naive though
  % := 8
  (dispose))

; todo loop recur

(tests
  "mutual recursion"
  (declare pong)
  (defnode ping [x] (case x 0 :done (pong (dec x))))
  (defnode pong [x] (ping x))
  (def dispose (r/run (! (ping 3))))
  % := :done
  (dispose))

(tests
  "For reference, Clojure exceptions have dynamic scope"
  (try
    (let [f (try (fn [] (/ 1 0))                            ; this exception will escape
                 (catch Exception _ ::inner))]
      ; the lambda doesn't know it was constructed in a try/catch block
      (f))
    (catch Exception _ ::outer))
  := ::outer)

(comment
  "reactive exceptions"
  (defnode boom [] (throw (ex-info "" {})))
  (def dispose
    (r/run (! (r/try
                (boom)
                (r/catch Exception _ ::inner)))))
  % := ::inner                                              ; reactive exception caught

  (def dispose
    (r/run (! (r/try
                (let [nf (r/try
                           (node [] (boom))                 ; reactive exception uncaught
                           (r/catch Exception _ ::inner))]
                  ($ nf))
                (r/catch Exception _ ::outer)))))
  := ::outer)

(comment
  "leo bind"
  (defnode foo)
  (def !x (atom 0))
  (def dispose (r/run (! (r/bind [(foo [] ~(m/watch !x))] (foo)))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(comment
  "cannot take value of a bind"
  (defnode nf)
  (def dispose
    (r/run (! (r/bind [(nf [] (boom))]
                      nf))))
  % := ::rcf/timeout                                        ; runtime error
  (dispose))

(comment
  "dynamic scope (note that try/catch has the same structure)"
  (def ^:dynamic db)
  (defnode foo [] db)
  (def dispose (r/run (! (r/binding [db ::inner] (foo)))))
  % := ::inner

  (def dispose (r/run (! (r/binding [db ::outer]
                                    (let [nf (r/binding [db ::inner]
                                                        (node [] (foo)))] ; binding out of scope
                                      ($ nf))))))
  % := ::outer
  (dispose))

(comment
  "reactive interop with clojure dynamic scope"
  ; motivating use case: (defnode hf-nav [kf e] (kf (d/entity *db* e)))
  ; we think this is well defined but dangerous because
  ; each and every down-scope function call will react on this implicit global
  ; which can be catastrophic to performance
  (def ^:dynamic *db*)
  ; all reactive bindings are captured and attached to both of these calls
  ; only reactive bindings must be translated, not clojure bindings
  (defn not-query [] (inc 1))                               ; reacts on implicit global !!
  (defn query [] (inc *db*))
  (def !x (atom 0))
  (def dispose (r/run (! (r/binding [*db* ~(m/watch !x)] (query)))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))
