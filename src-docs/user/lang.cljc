(ns user.lang
  "Photon language tutorial"
  (:require [hfdl.lang :as r]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [missionary.core :as m]))

;(defmacro with-disposal [task & body]
;  `(let [dispose# ~task]
;     ~@body
;     (dispose#)))
;
;(tests
;  "hello world"
;  (with-disposal
;    (r/run (! "hello world"))
;    % := "hello world"))

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

(tests
  "reactive quote escapes to flow layer"
  (def dispose
    (r/run (! (let [x 1]
                [(type x)
                 (if (fn? #'x) ::fn)]))))
  % := [java.lang.Long ::fn]
  (dispose)

  "special form for unquoting a quoted flow (monadic join)"
  (def dispose (r/run (! (let [x #'1] ~x))))
  % := 1
  (dispose))

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
  (def dispose
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

(comment
  "reactor termination"
  ; Leo says: pending question. (The test does pass)
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
  "reactive def")

(tests
  "reactive fn"
  ; Leo thinks fn definition is not right, easy fix
  ; reactive quote works
  (def dispose (r/run (! (r/$ (r/fn [x] (inc x)) 1))))
  % := 2
  (dispose))

(tests
  "reactive def fn"
  (r/def f (r/fn [x] (inc x)))
  (def dispose (r/run (! (r/$ f 1))))
  % := 2
  (dispose))

(tests
  "reactive defn"
  (r/defn f [x] (inc x))
  (def dispose (r/run (! (r/$ f 1))))
  % := 2
  (dispose))

(comment
  ; TBD, negotiating with Leo
  ; Leo: by eager we mean, when inside the if2 body, a and b
  ; are already evaluated (the flow has been run but not sampled)
  "control flow implemented with lazy"
  (r/defn if2 [x a b] ~(get {true #'a false #'b} (boolean x)))
  (def dispose (r/run (! (r/$ if2 false (! :a) (! :b)))))
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

(tests
  "def"
  ; concurrency issue, a problem with def redefinition
  (r/def foo 1)
  (def dispose (r/run (! foo)))
  % := 1
  (dispose))

(tests
  "binding"
  (r/def foo 1)
  (def dispose (r/run (! (binding [foo 2] foo))))
  % := 2
  (dispose))

(tests
  "quote captures lexical scope"
  ; Dustin: controversial
  ; Leo suggests renaming to "thunk" to match the behavior
  ; thunk is a closure with no args
  (def dispose (r/run (! ~(let [a 1] #'a))))
  % := 1
  (dispose))

(tests
  "join captures dynamic scope"
  (r/def foo 1)
  (def dispose (r/run (! (let [q #'foo] (binding [foo 2] ~q)))))
  % := 2
  (dispose))

(tests
  "if with bindings"
  (def !a (atom true))
  (r/def foo 1)
  (def dispose (r/run (! (binding [foo 2] (if ~(m/watch !a) foo (- foo))))))
  % := 2
  (swap! !a not)
  % := -2
  (dispose))

(tests
  "if with unwinding binding"
  (def !a (atom true))
  (r/def foo 1)
  (def dispose (r/run (! ~(binding [foo 2] #'(if ~(m/watch !a) foo (- foo))))))
  % := 1
  (swap! !a not)
  % := -1
  (dispose))

(tests
  ; unstable
  "internal def"
  (def !a (atom 0))
  (r/def foo 1)
  (r/def bar 2)
  (def dispose (r/run (! ~((def bar) #'[foo bar] (m/watch !a)))))
  % := [1 0]
  (dispose))

(tests
  "reactive for"
  (def !xs (atom [1 2 3]))
  (def dispose (r/run (! (r/for [x ~(m/watch !xs)] (inc x)))))
  % := []                                                   ;; TODO
  % := [2 3 4]
  (swap! !xs conj 4)
  % := [2 3 4 5]
  #_(dispose))

(tests
  "reactive for is differential (diff/patch)"
  (def !xs (atom [1 2 3]))
  (def dispose (r/run (! (r/for [x ~(m/watch !xs)] (! x)))))
  % := []                                                   ;; TODO
  (hash-set % % %) := #{1 2 3}                              ; concurrent, order undefined
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

(tests
  "Reactive for with bindings"

  (def !items (atom ["a"]))
  (r/def foo 0)
  (r/run (binding [foo 1]
           (r/for [item ~(m/watch !items)]
             (! foo)
             item)))

  % := 1
  (swap! !items conj "b")
  % := 1 ; If 0 -> foo’s binding vanished
  )


(tests
  "reactive for with keyfn"
  (def !xs (atom [{:id 1 :name "alice"} {:id 2 :name "bob"}]))
  (r/run (! (r/for-by :id [x ~(m/watch !xs)] (! x))))
  % := []                                                   ;; TODO
  (hash-set % %) := #{{:id 1 :name "alice"} {:id 2 :name "bob"}}
  % := [{:id 1 :name "alice"} {:id 2 :name "bob"}]
  (swap! !xs assoc-in [0 :name] "ALICE")
  % := {:id 1 :name "ALICE"}
  % := [{:id 1 :name "ALICE"} {:id 2 :name "bob"}])

(comment
  (r/run (do :a :b))
  )

(tests
  "reactive do"
  ; What is the intent of do?
  ; When you write do, you always want to perform effects
  ; Todo, resolve controversy - how lazy is it?
  (def !x (atom 0))
  (def dispose (r/run (! (do (! :a) (! ~(m/watch !x))))))
  ; do is not monadic sequence, we considered that
  ; It's an incremental computation so only rerun what changed in our opinion
  % := :a
  % := 0
  % := 0
  (swap! !x inc)
  % := 1
  % := 1
  (dispose))

(tests
  "do forces evaluation (introduces eagerness)"
  ; Current behavior - do stmts are sampled eagerly, as fast as possible
  (def !a (atom 0))
  (def !b (atom 0))
  (r/run (! @(doto !b (reset! (! ~(m/watch !a))))))
  % := 0
  % := 0
  (swap! !a inc)
  ; the ref !b doesn't change, so we don't see 1 again
  % := 1)

(comment
  "entrypoint forces evaluation (introduces eagerness)" ; desired behavior, we think
  ; Alternative - do stmts are sampled (for effect) when result is sampled

  (def !a (atom 0))
  (def !b (atom 0))
  (r/run (! @(doto !b (reset! (! ~(m/watch !a))))))
  % := 0
  % := 0
  (swap! !a inc)
  % := 1
  % := 1)

(rcf/set-timeout! 4000)

(tests
  "do stmts run in parallel, not sequence.
  In other words, `do` is sequenceA or sequenceM"
  (def x (m/ap (m/? (m/sleep 1000 :a))))
  (def y (m/ap (m/? (m/sleep 1000 :b))))
  (def z (m/ap (m/? (m/sleep 1000 :c))))
  (def dispose
    (r/run

      (! (do ~x ~y ~z))

      ))
  % := :c
  ; and took 1 seconds
  (dispose))

; first way (do a b) is same as (let [_ a] b) - true in clojure. Problem here is do stmts (a) are never sampled which is
; never what you want
;
; second way (do a b) is the same as (case a b). a and b are sequenced. problem is b is not constructed until a is
; available (maybe bug). This is what we have today, may not disqualify the current behavior
;
; third way (do a b) is same as ({} a b); both are constructed at the same time and we need both to be available for the
; do expression to be available. whenever a changes, the expr changes.

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
  % := {"a" 1 "b" 0}
  (swap! !x inc)
  ;% := ::rcf/timeout       ; old design no further sample, the map hasn't changed
  % := {"a" 1 "b" 1} ; alternative (desired) design will sample again
  (dispose))

; node call (static dispatch)
(tests
  "reactive defn"
  ; best example of this is hiccup incremental maintenance

  (r/def !')
  (r/defn div [child] (!' child) [:div child])
  (r/defn widget [x]
    (r/$ div [(r/$ div x) (r/$ div :a)]))

  (def !x (atom 0))
  (def dispose (r/run (! (r/binding [!' ! #_(r/fn [x] (! x))] ; careful at repl, ! only defined in test context
                           (r/$ widget ~(m/watch !x))))))
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
  (r/def g (r/fn [x] x))                                      ; reactive fn (DAG). Compiler marks dag with meta
  (defn f [x] x)                                            ; This var is not marked with meta
  (def !x (atom 0))
  (def dispose
    (r/run
      (! (let [x ~(m/watch !x)]
           [(f x) (r/$ g x)]))))
  % := [0 0]
  (dispose))

(tests
  "higher order dags"
  (def !x (atom 0))
  (defn f [x] x)
  (r/def g (r/fn [x] x))
  (def dispose
    (r/run
      (! (let [ff #_(fn [x] x) identity                     ; foreign clojure fns are useful, e.g. passing callbacks to DOM
               gg (r/fn [x] x)                              ; you almost always want this, not fn
               x ~(m/watch !x)]
           [(f x)                                           ; var marked
            (r/$ g x)                                           ; var says node
            (ff x)                                          ; Must assume interop, for compat with clojure macros
            (r/$ gg x)                                        ; Must mark reactive-call
            (r/$ (r/fn [x] x) x)]))))
  % := [0 0 0 0 0]
  (dispose))

(tests
  "former bug"
  (def !x (atom 0))
  (def !y (atom 0))
  (def dispose (r/run (! (let [y ~(m/watch !y)]
                           (if (odd? ~(m/watch !x))
                             (r/fn [x] (+ y x))
                             (r/fn [x] (+ y x)))))))
  % := _
  (dispose))

(tests
  "reactive node closure"
  (def !x (atom 0))
  (def !y (atom 0))
  (def dispose
    (r/run (! (let [x ~(m/watch !x)
                    y ~(m/watch !y)
                    f (r/fn [x] (+ y x))          ; constant signal
                    g (if (odd? x) (r/fn [x] (+ y x))
                                   (r/fn [x] (+ y x)))
                    h ~(m/seed [(r/fn [x] (+ y x))])]
                [(r/$ f x)
                 (r/$ g x)
                 (r/$ h x)]))))
  % := [0 0 0]
  (dispose))

(comment
  ; todo implement fn
  "reactive clojure.core/fn"
  (def !x (atom 0))
  (def !y (atom 0))
  (def dispose
    (r/run
      (! (let [x ~(m/watch !x)
               y ~(m/watch !y)
               ; rebuild clojure closure when y updates
               f (fn [needle] (+ y needle))]
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
           (let [!n (atom (r/fn [] 0))]
             (when ~(m/watch !a)
               (let [x ~(m/watch !b)]
                 (reset! !n (r/fn [] x))))                  ; use mutation to escape the extent of the closure
             ~(m/watch !n))))))
  := 0
  (swap! !a not)
  := 1
  (swap! !a not)                                            ; watch !b is discarded
  := ::rcf/timeout)

(comment
  "reactive recursion"
  (r/defn fib [n]
    ; todo, recursion doesn't work yet
    (case n
      0 0 1 1
      (+ (r/$ fib (- n 2))                                        ; self recur
         (r/$ fib (- n 1)))))
  (def !x (atom 5))
  (def dispose (r/run (! (fib ~(m/watch !x)))))
  % := 5
  (swap! !x inc)
  ; this will reuse the topmost frame, it is still naive though
  % := 8
  (dispose))

(comment
  "recur special form"
  (r/defn fib' [n]
    (case n
      0 0 1 1
      (+ (recur (- n 2)) ; todo
         (recur (- n 1)))))
  (def !x (atom 5))
  (def dispose (r/run (! (fib' ~(m/watch !x)))))
  % := 5
  (swap! !x inc)
  ; this will reuse the topmost frame, it is still naive though
  % := 8
  (dispose))

; todo loop recur

(comment
  "mutual recursion"
  (declare pong)
  (r/defn ping [x] (case x 0 :done (r/$ pong (dec x))))
  ; can static call infer $ here? Leo needs to think
  (r/defn pong [x] (r/$ ping x))
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

(tests
  "reactive exceptions"
  (r/defn boom [] (throw (ex-info "" {})))
  (def dispose
    (r/run (! (try (r/$ boom) (catch Exception _ ::inner)))))
  % := ::inner                                              ; reactive exception caught

  (def dispose
    (r/run (! (try
                (let [nf (try
                           (r/fn [] (r/$ boom))             ; reactive exception uncaught
                           (catch Exception _ ::inner))]
                  (r/$ nf))
                (catch Exception _ ::outer)))))
  % := ::outer)

(tests
  "leo bind"
  (r/def foo)
  (r/def foo')
  (def !x (atom 0))
  (def dispose (r/run (! (let [x ~(m/watch !x)]
                           (binding [foo #'(inc x)
                                     foo' (r/fn [] (inc x))]
                             [~foo (r/$ foo')])))))            ; omg
  % := [1 1]
  (swap! !x inc)
  % := [2 2]
  (dispose))

; dumb test
;(comment
;  "can take value of bind (previously couldn't)"
;  (r/def nf)
;  (def dispose
;    (r/run (! (r/binding [nf 1] nf))))
;  % := 1                                        ; runtime error
;  (dispose))

(tests
  "dynamic scope (note that try/catch has the same structure)"
  (r/def db)
  (r/defn foo [] db)
  (def dispose (r/run (! (binding [db ::inner] (r/$ foo)))))
  % := ::inner
  (dispose)

  (def dispose (r/run (! (binding [db ::outer]
                           (let [nf (binding [db ::inner]
                                      (r/fn [] (r/$ foo)))]     ; binding out of scope
                             (r/$ nf))))))
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

; unquote is for introducing flow transformers – a special form
(comment
  "flow transformers"
  ; :: (a -> m b) -> (a -> m b)
  ; widgets take a flow and return a new flow.
  )

(tests
  "lazy parameters. Flows are not run unless sampled"
  (def dispose (r/run (r/$ (r/fn [_]) (! :boom))))
  % := ::rcf/timeout
  (dispose)

  (r/def foo :boom)
  (def dispose (r/run (let [_ (! :boom)])))
  % := ::rcf/timeout
  (dispose))

(tests
  (r/run2 {} (! ~@~@1))
  % := 1)

(tests
  (r/def foo nil)
  (r/run2 {} (! (binding [foo 1] ~@~@foo)))
  % := 1)

(tests
  (r/def foo nil)
  (r/run2 {} (! (binding [foo 1] ~@~#'~@foo)))
  % := 1)

(tests
  (r/def foo nil)
  (r/def bar #'~@foo)
  (r/run2 {} (! (binding [foo 1] ~@~bar)))
  % := 1)

(tests
  "reactive pending states"
  ;~(m/reductions {} hfdl.impl.runtime/pending m/none)
  (def dispose (r/run (! (try true (catch hfdl.impl.runtime/Pending _ ::pending)))))
  % := true)

(tests
  (r/run2 {} (! (try ~@1 (catch hfdl.impl.runtime/Pending _ ::pending))))
  % := ::pending    ; Use try/catch to intercept special pending state
  % := 1)

(tests
  (r/run2 {} (! (try [(! 1) (! ~@2)]
                     (catch hfdl.impl.runtime/Pending _
                       ::pending))))
  % := 1
  % := ::pending
  ; do not see 1 again
  % := 2
  % := [1 2])

;(tests
;  (r/run2 {} (! (try (dom/div)                              ; must be cleaned up by pending state - in dom layer. todo
;                     (dom/div ~@1)
;                     (catch hfdl.impl.runtime/Pending _
;                       (dom/div ::pending)))))
;  % := ::pending
;  % := 1)

;(tests
;  (r/run2 {} (! (try [~@(d/q) ~@(d/entity)]
;                     (catch hfdl.impl.runtime/Pending _
;                       ::pending))))
;  % := ::pending
;  % := 1)

(comment
  "photon binding transfer"
  ; Guidance: distribution should not impact the evaluated result of the expr
  (tests
    (r/defn expr [x] x)
    (r/run2 {} (! ~@(r/$ expr 1)))
    % := 1)

  (tests
    (r/def expr #'(let [x %0] x))
    (r/run2 {} (! ~@(binding [%0 1] ~expr)))                ; no binding transfer
    % := 1)

  (tests
    (r/def expr #'(let [x %0] x))
    (r/run2 {} (! (binding [%0 1] ~@~expr)))                ; binding transfer
    % := 1))

(tests
  (def !x (atom 0))
  (r/run
    (let [x ~(m/watch !x)]
      (when (even? x) (! x))))
  % := 0
  (swap! !x inc)
  (swap! !x inc)
  % := 2
  )

(comment
  "clojure metadata"
  ; is this problematic?
  ; leo says if transit encodes this then it will work for free
  ; thus keep clojure core behavior wrt metas
  (r/run2 {}
    (let [x (with-meta {} {:foo 1})]
      ; works with explicit do
      ; crashes currently
      (! (meta x))
      (! (meta ~@x))
      (! (meta ~@~@x))))
  % := {:foo 1}
  % := {:foo 1}
  % := {:foo 1})

(tests
  "reactive metadata"
  (def !x (atom 0))
  (r/run (! (meta (let [x (with-meta [] {:foo ~(m/watch !x)})] x))))
  % := {:foo 0}
  (swap! !x inc)
  % := {:foo 1})

(tests
  "regression: cancel on reactive quote"
  (def !x (atom 42))
  (r/def foo ~(m/watch !x))

  ; prove that if we pass this fn a reactive quote,
  ; then it will fail to cancel properly. The switch will cancel
  ; the quote then await termination which never happens.
  (defn x [>a] (m/ap (m/?< (m/seed [1 2]))
                     (m/?< >a)))

  ; To repro the bug the >a must just be a reactive var

  (r/run (! ~(x #'foo)))
  % := 42
  % := 42  ; bug is timeout
  )

(tests
  ""
  (def !x (atom 42))
  (r/def foo ~(m/watch !x))

  ; prove that if we pass this fn a reactive quote,
  ; then it will fail to cancel properly. The switch will cancel
  ; the quote then await termination which never happens.
  (defn x [>a] (m/ap (m/?< (m/seed [1 2]))
                     (m/?< >a)))

  ; To repro the bug the >a must just be a reactive var

  (r/run (! ~(x (let [x foo] #'x))))
  % := 42
  % := 42  ; bug is timeout
  )

(tests
  (let [foo (m/ap (m/? (m/sleep 10 :foo)))]
    (r/run (! ~~#'(let [a ~foo] #'a)))
    % := :foo))

(tests
  (def !x (atom 0))
  (r/run (! (try (-> ~(m/watch !x)
                   (doto (-> even? (when-not (throw (ex-info "odd" {})))))
                   (/ 2))
                 (catch Exception e (ex-message e)))))
  % := 0
  (swap! !x inc)
  % := "odd"
  (swap! !x inc)
  % := 1)

(tests
  (def !x (atom 0))
  (def !f (atom "hello"))
  (def e (ex-info "error" {}))
  (r/run
    (! (try (when-not (even? ~(m/watch !x)) (throw e))
            (finally (! ~(m/watch !f))))))
  % := "hello"
  % := nil
  (swap! !x inc)
  (reset! !f "world")
  % := "world"
  (swap! !x inc)
  % := nil)
