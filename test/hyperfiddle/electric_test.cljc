(ns hyperfiddle.electric-test
  (:require
   [contrib.cljs-target :refer [do-browser]]
   [clojure.core :as cc]
   [hyperfiddle.electric :as e]
   #?(:clj [hyperfiddle.electric.impl.expand :as expand])
   [hyperfiddle.electric.impl.lang :as lang]
   [hyperfiddle.electric.impl.runtime :as r]
   [hyperfiddle.electric.impl.io :as electric-io]
   [hyperfiddle.electric.impl.ir-utils :as ir-utils]
   [hyperfiddle.electric-local-def :as l]
   #?(:cljs [hyperfiddle.goog-calls-test])
   #?(:cljs [hyperfiddle.js-calls-test])
   [hyperfiddle.rcf :as rcf :refer [% tap with tests]]
   [missionary.core :as m]
   [clojure.test :as t]
   [clojure.string :as str]
   [contrib.assert :as ca])
  (:import [missionary Cancelled]
           [hyperfiddle.electric Pending Failure]
           #?(:clj [clojure.lang ExceptionInfo Compiler$CompilerException])))

(tests "hello world"
  (l/def Msg "Hello")
  (with ((l/local (tap Msg)) tap tap)
    % := "Hello"))

(tests "node using a node"
  (l/def Hello "Hello")
  (l/def Greeting (str Hello " " Hello "!"))
  (with ((l/local (tap Greeting)) tap tap)
    % := "Hello Hello!"))

(tests "binding"
  (l/def B 1)
  (l/def BB [B (binding [B 2] B)])
  (with ((l/local (tap BB)) tap tap)
    % := [1 2]))

(tests "new on electric node"
  (l/def Closure (::lang/closure 1))
  (l/def NewNode (new Closure))
  (with ((l/local (tap NewNode)) tap tap)
    % := 1))

(tests "closure reading node"
  (l/def Readme 1)
  (with ((l/local (tap (new (::lang/closure Readme)))) tap tap)
    % := 1))

(tests "inject"
  (l/def NonInjected 1)
  (l/def ToInject 2)
  (with ((l/local (tap (new ((::lang/inject ToInject) (::lang/closure [NonInjected ToInject]) (m/watch (atom 0)))))) tap tap)
    % := [1 0]))

(tests "try/finally"
  (with ((l/local (try (tap 1) (catch Pending _) (finally (tap 2)))) tap tap)
    [% %] := [1 2]))

(tests "try/catch"
  (def boom (ex-info "boom" {}))
  (with ((l/local (try (throw boom) (catch ExceptionInfo e (tap e)))) tap tap)
    % := boom))

(tests "transfer"
  (l/def Transfer (e/server 1))
  (with ((l/local (try (tap Transfer) (catch Pending _))) tap tap)
    % := 1))

(tests "new on local electric closure"
  (with ((l/local (let [x (::lang/closure 1)] (tap (new x)))) tap tap)
    % := 1))

(defrecord Point [x y])
(tests "new on class"
  (with ((l/local (tap (new Point 1 2))) tap tap)
    % := (Point. 1 2)))

(tests "new on missionary flow"
  (with ((l/local (tap (new (m/ap 1)))) tap tap)
    % := 1))

(tests "if"
  (with ((l/local (tap (if true :yes :no))) tap tap)
    % := :yes))

(tests "case"
  (with ((l/local (tap (case 1 1 1 2 2))) tap tap)
    % := 1))

(tests "quote"
  (with ((l/local (tap 'foo)) tap tap)
    % := 'foo))

#?(:cljs
   (tests "js*"
     (with ((l/local (tap (js* "~{}+1" 1))) tap tap)
       % := 2)))

(tests "clj fn"
  (with ((l/local (let [x 1] (tap (#(inc x))))) tap tap)
    % := 2))

#?(:clj
   (tests "."
     (with ((l/local (tap (. java.time.Instant EPOCH))) tap tap)
       % := java.time.Instant/EPOCH)))

(tests "loop/recur"
  (with ((l/local (tap (loop [x 1] (if (odd? x) (recur (dec x)) x)))) tap tap)
    % := 0))

(tests "def"
  (with ((l/local (def DEFD 1)) tap tap))
  DEFD := 1)

;;; MAIN ELECTRIC TEST SUITE

(tests "hello world"
  (with ((l/local (tap "hello world")) tap tap)
    % := "hello world"))

(tests "literals are lifted"
  (with ((l/local (tap 1)) tap tap)
    % := 1))

(tests "data literals"
  (with ((l/local (tap {:a 1})) tap tap)
    % := {:a 1}))

(tests "globals lifted"
  (def a 1)
  (with ((l/local (tap a)) tap tap)
    % := 1))

(tests
  (with ((l/local (tap inc)) tap tap)
    % := inc))

(tests "clojure call"
  (with ((l/local (tap (inc (inc 1)))) tap tap)
    % := 3))

(tests "introduce foreign atom"
  (def !x (atom 0))
  (with ((l/local (tap (e/watch !x))) tap tap)                           ; clojure flow derived from atom
    % := 0
    (swap! !x inc)
    % := 1))

(tests "l/def body is an Electric expressions"
  (def !x (atom 0))
  (l/def x (e/watch !x))                                   ; just don't use it from Clojure
  (with ((l/local (tap x)) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "l/def defines a singleton"
  (def !x (atom 0))
  (defn incer [atm] (m/cp (swap! atm inc)))
  (l/def X (new (incer !x)))
  (with ((l/local (tap [X X])) tap tap)
    % := [1 1]))

(tests "introduce foreign missionary signal"
  (def !x (atom 0))                                         ; atoms model variable inputs
  (with ((l/local (tap (new (m/watch !x)))) tap tap)                      ; clojure flow derived from atom
    % := 0
    (swap! !x inc)
    % := 1))

(comment "can't boot flow from Clojure global due to syntax collision with Clojure new"
  (def !x (atom 0))
  (def X (m/watch !x))
  (with ((l/local (tap (new X))) tap tap)                                 ; unsupported
    % := #?(:clj 0 :cljs _)
    (swap! !x inc)
    % := 1))

(tests "however, CAN boot flow from a Electric global"
  (def !x (atom 0))
  (l/def X (m/watch !x))
  (with ((l/local (tap (X.))) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "reactive closures - call them with (new)"
  (with ((l/local (tap (let [x 1, F (e/fn [] x)] ; capitalized
                         [(number? x) (new F)]))) tap tap) ; construct/flatmap
    % := [true 1]))

(tests "dataflow diamond - let introduces shared nodes in the dag"
  (def !x (atom 0))
  (with ((l/local (tap (let [x (e/watch !x)] (+ x x)))) tap tap)
    % := 0
    (swap! !x inc)
    % := 2
    (swap! !x inc)
    % := 4))

(tests "broken dataflow diamond (two propagation frames - bad)"
  (def !x (atom 0))
  (with ((l/local (tap (let [X (m/watch !x)] ; recipe for flow
                         (+ (X.) (X.))))) tap tap)                        ; bad - construct flow twice
    % := 0
    (swap! !x inc)
    % := 1                                                  ; glitch due to two watch events,
    % := 2                                                  ; causing two propagation frames
    (swap! !x inc)
    % := 3
    % := 4))

(tests "reactive function call"
  (def !f (atom +))
  (def !x2 (atom 1))
  (with ((l/local (tap ((e/watch !f) 0 (e/watch !x2)))) tap tap)
    % := 1
    (swap! !x2 inc)
    % := 2
    (reset! !f -)
    % := -2))

(tests "l/def without initial value"
  (def !x (atom 0))
  (l/def X_136)
  (with ((l/local (tap (binding [X_136 (m/watch !x)] (X_136.)))) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "l/def with initial value"
  (def !x (atom 0))
  (l/def X_146 (m/watch !x))
  (with ((l/local (tap (X_146.))) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "foreign clojure collections. clojure.core/map is not incremental, the arguments are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (with
    ((l/local
       (tap (let [f (new (m/watch !f))
                  xs (new (m/watch !xs))]
              (clojure.core/map f xs)))) tap tap)
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]
    (reset! !f dec)
    % := [0 1 2 3]))

(tests "common core macros just work"
  (with
    ((l/local
       (tap (let [f (new (m/watch (atom inc)))
                  xs (new (m/watch (atom [1 2 3])))]
              (->> xs (map f))))) tap tap)
    % := [2 3 4]))

(comment #_rcf/tests "reactor termination"
  ; Leo says: pending question. (The test does pass)
  (def !x (atom 0))
  (with ((l/local (tap (new (->> (m/watch !x) (m/eduction (take-while even?)))))) tap tap)
    % := 0
    (reset! !x 2)
    % := 2
    (reset! !x 1)
    % := ::rcf/timeout))  ; never switched odd because it terminated

(tests "reactive if"
  (def !a (atom 1))
  (def !p (atom :p))
  (def !q (atom :q))
  (with ((l/local (tap (if (odd? (new (m/watch !a))) (new (m/watch !p)) (new (m/watch !q))))) tap tap)
    % := :p
    (swap! !a inc)
    % := :q
    (reset! !p :pp)
    (swap! !a inc)
    % := :pp))

(tests "lazy"
  (with ((l/local (tap (if false (tap :a) (tap :b)))) tap tap)
    % := :b
    % := :b))

(tests "reactive fn"
  (with ((l/local (tap (new (e/fn [x] (inc x)) 1))) tap tap)
    % := 2))

(l/def F2 (e/fn [x] (inc x)))
(tests "reactive def fn"
  (with ((l/local (tap (F2. 1))) tap tap)
    % := 2))

(l/defn My-inc [x] (inc x))
(tests "reactive defn"
  (with ((l/local (tap (My-inc. 1))) tap tap)
    % := 2))

(tests "control flow implemented with lazy signals"
  (l/defn If2 [x a b]                                       ; Key question - how lazy are the parameters?
    (->> (boolean x)
         (get {true (e/fn [] a)
               false (e/fn [] b)})
         (new)))

  (def !x (atom false))
  (def !a (atom :a))
  (def !b (atom :b))
  (with ((l/local (let [x (e/watch !x)
                        a (tap (e/watch !a)) ; lazy
                        b (tap (e/watch !b))] ; lazy
                    (tap (If2. x a b)))) tap tap)
    % := :a
    % := :b
    % := :b
    (swap! !x not)
    % := :a))

(tests "imperative let behavior (with clojure.core compat) sequences effects in let order, breaking lazy if"
  (def !x (atom false))
  (def !a (atom :a))
  (def !b (atom :b))
  (with ((l/local (let [x (e/watch !x)
                        a (tap (e/watch !a))
                        b (tap (e/watch !b))]
                    (tap (if x a b)))) tap tap)
    % := :a    ; too eager, but desirable default for CC compat
    % := :b
    % := :b
    (swap! !x not)
    ;% := :a
    ;% := :b
    % := :a))

(tests "reactive case"
  (def !a (atom 0))
  (def !p (atom :p))
  (def !q (atom :q))
  (with ((l/local (tap (case (e/watch !a)
                         0 (e/watch !p)
                         (e/watch !q)))) tap tap)
    % := :p
    (swap! !a inc)
    % := :q
    (reset! !q :qq)
    % := :qq))

(tests "symbols in electric"
  (with ((l/local (tap 'x)) tap tap)
    % := 'x))

(tests "symbols in electric"
  (with ((l/local (tap '[x])) tap tap)
    % := '[x]))

(tests "case on symbols"
  (def !x (atom 'foo))
  (with ((l/local (tap (case (e/watch !x) foo 1 2))) tap tap)
    % := 1))

(tests "case on symbols"
  (def !x (atom 'foo))
  (with ((l/local (tap (case '[a b] [a b] 1 2))) tap tap)
    % := 1))

(tests "case with no matching clause"
  (with ((l/local (try (case 2 1 1)
                       (catch #?(:clj IllegalArgumentException :cljs js/Error) e (tap [:right (ex-message e)]))
                       (catch #?(:clj Throwable :cljs :default) e (tap [:wrong e])))) tap tap))
  % := [:right "No matching clause: 2"])

(l/def my-var 1)
(tests "def"
  (with ((l/local (tap my-var)) tap tap)
    % := 1))

(tests "binding"
  (l/def foo 1)
  (with ((l/local (tap (binding [foo 2] foo))) tap tap)
    % := 2))

(tests "binding - fn"
  (l/def foo)
  (with ((l/local (binding [foo (partial tap)] (foo 1))) tap tap)
    % := 1))

(tests "binding - e/fn"
  (l/def foo)
  (with ((l/local (binding [foo (e/fn [x] (tap x))] (foo. 1))) tap tap)
    % := 1))

(tests "lexical closure"
  (with ((l/local (tap (new (let [a 1] (e/fn [] a))))) tap tap)
    % := 1))

(tests "join captures dynamic scope"
  (l/def foo 1)
  (with ((l/local (let [Q (e/fn [] foo)]
                    (binding [foo 2]
                      (tap (Q.))))) tap tap)
    % := 2))

(tests "if with bindings"
  (def !a (atom true))
  (l/def foo 1)
  (with ((l/local (tap (binding [foo 2] (if (e/watch !a) foo (- foo))))) tap tap)
    % := 2
    (swap! !a not)
    % := -2))

(l/def foo4 1)
(tests "if with unwinding binding"
  (def !a (atom true))
  (with ((l/local (tap (new (binding [foo4 2] (e/fn [] (if (new (m/watch !a)) foo4 (- foo4))))))) tap tap)
    % := 1
    (swap! !a not)
    % := -1))

(l/def foo 1)
(l/def bar 2)

(tests "inject"
  (def !a (atom 0))
  (with ((l/local (tap (new ((::lang/inject bar) (e/fn [] [foo bar]) (m/watch !a))))) tap tap)
    % := [1 0]))

(tests "reactive for"
  (def !xs (atom [1 2 3]))
  (with ((l/local (tap (e/for [x (e/watch !xs)] (inc x)))) tap tap)
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]))

(tests "reactive for is differential (diff/patch)"
  (def !xs (atom [1 2 3]))
  (with ((l/local (tap (e/for [x (new (m/watch !xs))] (tap x)))) tap tap)
    (hash-set % % %) := #{1 2 3}                            ; concurrent, order undefined
    % := [1 2 3]
    (swap! !xs conj 4)
    % := 4
    % := [1 2 3 4]
    (swap! !xs pop)
    % := [1 2 3]  ;; TODO glitch here
    (swap! !xs assoc 1 :b)
    % := :b
    % := [1 :b 3]))

(l/def foo 0)
(tests "Reactive for with bindings"
  (def !items (atom ["a"]))
  (with ((l/local (binding [foo 1]
                    (e/for [item (e/watch !items)]
                      (tap foo)
                      item))) tap tap)

    % := 1
    (swap! !items conj "b")
    % := 1)) ; If 0 -> foo’s binding vanished

(tests "reactive for with keyfn"
  (def !xs (atom [{:id 1 :name "alice"} {:id 2 :name "bob"}]))
  (with ((l/local (tap (e/for-by :id [x (e/watch !xs)] (tap x)))) tap tap)
    (hash-set % %) := #{{:id 1 :name "alice"} {:id 2 :name "bob"}}
    % := [{:id 1 :name "alice"} {:id 2 :name "bob"}]
    (swap! !xs assoc-in [0 :name] "ALICE")
    % := {:id 1 :name "ALICE"}
    % := [{:id 1 :name "ALICE"} {:id 2 :name "bob"}]))

(tests "reactive do (this is changing soon)"
  ; see: https://www.notion.so/hyperfiddle/What-is-do-let-and-implement-ed781cc5645d4e83aa90b04e31988754
  ; current behavior is not compatible with cc/let
  (def !x (atom 0))
  (with ((l/local (tap (do (tap :a) (tap (e/watch !x))))) tap tap)
    ; Currently, do is not monadic sequence.
    ; It's an incremental computation so only rerun what changed in our opinion
    % := :a
    % := 0
    % := 0
    (swap! !x inc)
    ; no :a
    % := 1
    % := 1))

(tests "do forces evaluation (introduces eagerness)"
  ; Current behavior - do stmts are sampled eagerly, as fast as possible
  (def !a (atom 0))
  (def !b (atom 0))
  (with ((l/local (tap @(doto !b (reset! (tap (new (m/watch !a))))))) tap tap)
    % := 0
    % := 0
    (swap! !a inc)
    ; the ref !b doesn't change, so we don't see 1 again
    % := 1))

(comment "entrypoint forces evaluation (introduces eagerness)" ; desired behavior, we think
  ; Alternative - do stmts are sampled (for effect) when result is sampled

  (def !a (atom 0))
  (def !b (atom 0))
  ((l/local (tap @(doto !b (reset! (tap (new (m/watch !a))))))) tap tap)
  % := 0
  % := 0
  (swap! !a inc)
  % := 1
  % := 1)

#?(:clj (defn slow-identity [x] (Thread/sleep 30) x))

#?(:clj
   (tests
     (with ((l/local (try
                                        ; This test asserts that these run concurrently.
                                        ; If they block, the final tap would exceed the RCF timeout
                       (tap (e/offload #(slow-identity 1)))
                       (tap (e/offload #(slow-identity 2)))
                       (tap (e/offload #(slow-identity 3)))
                       (tap (e/offload #(slow-identity 4)))
                       (catch Pending _ (tap ::pending)))) tap tap) ; never see pending if thread is blocked
       % := ::pending
       (set [% % % %]) := #{3 1 2 4}))) ; concurrent sleeps race

#?(:clj
   (tests "reactive doto"
     (defn MutableMap [] (new java.util.HashMap))
     (defn PutMap [!m k v] (.put !m k v))
     (defn Ref [] (new Object))
     (def !z (atom 0))
     (def !xx (atom 0))
     (with ((l/local
              #_(doto (element "input")
                  (set-attribute! "type" "text")
                  (set-attribute! "value" x))
              (tap (doto (MutableMap)            ; the doto is incrementalized
                     (PutMap "a" (swap! !z inc)) ; detect effect
                     (PutMap "b" (tap (e/watch !xx)))))) tap tap)
       % := 0, % := {"a" 1 "b" 0}
       (swap! !xx inc)
       % := 1))) ; mutable map is clojure.core/=, therefore skipped

(l/def trace!)
(l/defn Div [child] (trace! child) [:div child])
(l/defn Widget [x] (Div. [(Div. x) (Div. :a)]))

(tests "reactive defn"
                                        ; best example of this is hiccup incremental maintenance
    (def !x (atom 0))
    (with ((l/local (tap (binding [trace! tap] (Widget. (e/watch !x))))) tap tap)
      % := 0
      % := :a
      % := [[:div 0] [:div :a]]
      % := [:div [[:div 0] [:div :a]]]
      (swap! !x inc)
      % := 1
                                        ; no :a
      % := [[:div 1] [:div :a]]
      % := [:div [[:div 1] [:div :a]]]))

(l/def G (e/fn [x] x))                                      ; reactive fn (DAG). Compiler marks dag with meta
(tests "node call vs fn call"
  (defn f [x] x)                                            ; This var is not marked with meta
  (def !x (atom 0))
  (with ((l/local (tap (let [x (e/watch !x)] [(f x) (G. x)]))) tap tap)
    % := [0 0]))

(l/def G (e/fn [x] x))
(tests "higher order dags"
  (def !x (atom 0))
  (defn f [x] x)
  (with
    ((l/local
       (tap (let [ff (fn [x] x) ; foreign clojure fns are sometimes useful, e.g. DOM callbacks
                  Gg (e/fn [x] x) ; but you almost always want reactive lambda, not cc/fn
                  x (e/watch !x)]
              [(f x)       ; var marked
               (G. x)      ; var says node
               (ff x)      ; Must assume interop, for compat with clojure macros
               (Gg. x)     ; Must mark reactive-call
               (new (e/fn [x] x) x)]))) tap tap)
    % := [0 0 0 0 0]))

(tests "reactive closures"
  (def !x (atom 1))
  (def !y (atom 10))
  (l/def x (e/watch !x))
  (l/def y (e/watch !y))
  (with ((l/local (tap (new (if (odd? x)
                              (e/fn [x] (* y x))
                              (e/fn [x] (* y x)))
                         x))) tap tap)
    % := 10
    (swap! !x inc)
    % := 20
    (swap! !x inc)
    % := 30
    (swap! !y inc)
    % := 33
    (swap! !y inc)
    % := 36))

(tests "reactive closures 2"
  (def !x (atom 0))
  (def !y (atom 0))
  (with
    ((l/local (tap (let [x (e/watch !x)
                         y (e/watch !y)
                         F (e/fn [x] (+ y x)) ; constant signal
                         G (if (odd? x) (e/fn [x] (+ y x))
                               (e/fn [x] (+ y x)))
                         H (new (m/seed [(e/fn [x] (+ y x))]))]
                     [(F. x)
                      (G. x)
                      (H. x)]))) tap tap)
    % := [0 0 0]))

(tests "reactive clojure.core/fn"
  (def !x (atom 0))
  (def !y (atom 0))
  (with
    ((l/local
       (tap (let [x (e/watch !x)
                  y (e/watch !y)
                                        ; rebuild Clojure closure f when y updates
                  f (fn [needle] (+ y needle))]
                                        ; (value is fully compatible with fn contract)
                                        ; the lambda is as variable as the var it closes over
                                        ; well defined. It's not allowed to use dataflow inside FN. Compiler can never reach it
                                        ; compiler will walk it to detect the free variables only
              (f x)))) tap tap)
    % := 0
    (swap! !y inc)
    % := 1
    (swap! !x inc)
    % := 2))

(tests "For reference, Clojure exceptions have dynamic scope"
  (try (let [f (try (fn [] (throw (ex-info "boom" {}))) ; this exception will escape
                 (catch #?(:clj Exception, :cljs :default) _ ::inner))]
      ; the lambda doesn't know it was constructed in a try/catch block
      (f))
    (catch #?(:clj Exception, :cljs :default) _ ::outer))
  := ::outer)

(tests "Reactor crashes on uncaught exceptions"
  (def !x (atom true))
  (with ((l/local (tap (assert (e/watch !x)))) tap tap)
    % := nil                            ; assert returns nil or throws
    (swap! !x not)                      ; will crash the reactor
    ;; TODO in old tests an ex-info comes out, why? Is this a FailureInfo?
    (ex-message %) := "Assert failed: (e/watch !x)"
    (swap! !x not)                      ; reactor will not come back.
    (tap ::nope), % := ::nope))

(l/defn Boom [] (assert false))
(tests "reactive exceptions"
  (with ((l/local (tap (try
                         (Boom.)
                         (catch #?(:clj AssertionError, :cljs js/Error) e
                           e)))) tap tap)
    #?(:clj  (instance? AssertionError %)
       :cljs (instance? js/Error %)) := true))

(tests
  (with ((l/local (tap (try (let [Nf (try (e/fn [] (Boom.)) ; reactive exception uncaught
                                          (catch #?(:clj AssertionError, :cljs :default) _ ::inner))]
                              (Nf.))
                            (catch #?(:clj AssertionError, :cljs :default) _ ::outer)))) tap tap)
    % := ::outer))

(l/def inner)
(l/def Outer (e/fn [] inner))

(tests "dynamic scope (note that try/catch has the same structure)"
  (with ((l/local (tap (binding [inner ::inner] (Outer.)))) tap tap)
    % := ::inner))

(tests "dynamic scope (note that try/catch has the same structure)"
  (with ((l/local (tap (binding [inner ::outer]
                         (let [Nf (binding [inner ::inner]
                                    (e/fn [] (Outer.)))] ; binding out of scope
                           (Nf.))))) tap tap)
    % := ::outer))

(tests "lazy parameters. Flows are not run unless sampled"
  (with ((l/local (new (e/fn [_]) (tap :boom))) tap tap)
    % := :boom))

(tests "lazy parameters. Flows are not run unless sampled"
  (with ((l/local (let [_ (tap :bang)])) tap tap)                 ; todo, cc/let should sequence effects for cc compat
    % := :bang))

(tests "client/server transfer"
                                        ; Pending state is an error state.
                                        ; Pending errors will crash the reactor if not caugh
  (with ((l/local (try (tap (e/server (e/client 1))) (catch Pending _))) tap tap)
    % := 1))

(l/def foo nil)
(tests
  (with ((l/local (try (tap (binding [foo 1] (e/server (e/client foo))))
                       (catch Pending _))) tap tap)
    % := 1))

(l/def foo nil)
(tests
  (with ((l/local (try (tap (binding [foo 1] (e/server (new (e/fn [] (e/client foo))))))
                       (catch Pending _))) tap tap)
    % := 1))

(l/def foo1 nil)
(l/def Bar1 (e/fn [] (e/client foo1)))
(tests
  (with ((l/local (try (tap (binding [foo1 1] (e/server (Bar1.))))
                       (catch Pending _))) tap tap)
    % := 1))

(tests "reactive pending states"
  ;~(m/reductions {} hyperfiddle.electric.impl.runtime/pending m/none)
  (with ((l/local (tap (try true (catch Pending _ ::pending)))) tap tap)
    % := true))

(tests
  (with ((l/local (tap (try (e/server 1) (catch Pending _ ::pending)))) tap tap)
    % := ::pending    ; Use try/catch to intercept special pending state
    % := 1))

(tests
  (with ((l/local (tap (try [(tap 1) (tap (e/server 2))] (catch Pending _ ::pending)))) tap tap)
    % := 1
    % := ::pending
                                        ; do not see 1 again
    % := 2
    % := [1 2]))

(tests "the same exception is thrown from two places!"
  (l/defn InputController1 [tap controlled-value]
    (try controlled-value (catch Pending _ (tap :pending-inner))))

  (with ((l/local (try
                    (InputController1. tap (throw (Pending.)))
                    (catch Pending _ (tap :pending-outer)))) tap tap))
  % := :pending-inner
  % := :pending-outer)

(tests "object lifecycle"
  (def !x (atom 0))
  (let [hook (fn [mount! unmount!]
               (m/observe (fn [!]
                            (mount!)
                            (! nil)
                            #(unmount!))))
        dispose!
        ((l/local (tap
                    (let [x (e/watch !x)]
                      (when (even? x)
                        (new (e/fn [x]
                               (new (hook (partial tap 'mount) (partial tap 'unmount)))
                               x)
                          x))))) tap tap)]

    % := 'mount
    % := 0
    (swap! !x inc)
    (hash-set % %) := '#{unmount nil}       ;; should ordering matter here ?
    (swap! !x inc)
    % := 'mount
    % := 2
    (dispose!)
    % := 'unmount))

(tests "object lifecycle 3"
  (defn observer [x]
    (fn mount [f]
      (f (tap [:up x]))
      (fn unmount [] (tap [:down x]))))

  (def !state (atom [1]))
  (with ((l/local (e/for [x (e/watch !state)] (new (m/observe (observer x))))) tap tap)
    % := [:up 1]
    (swap! !state conj 2)
    % := [:up 2]
    (reset! !state [3])
    (hash-set % % %) := #{[:up 3] [:down 1] [:down 2]})
  % := [:down 3])

(tests "object lifecycle 3 with pending state"
  (def !state (atom [1]))

  (defn observer [tap x]
    (fn mount [f]
      (tap [::mount x])
      (f nil)
      (fn unmount [] (tap [::unmount x]))))

  (let [dispose ((l/local (try
                            (e/for [x (e/watch !state)] ; pending state should not trash e/for branches
                              (new (m/observe (observer tap x)))) ; depends on x, which is pending
                            (catch Pending _))) tap tap)]
    % := [::mount 1]
    (reset! !state [2])
    (hash-set % %) := #{[::mount 2] [::unmount 1]}
    (reset! !state (Failure. (Pending.))) ; simulate pending state, we cannot use e/server due to distributed glitch
    % := [::unmount 2]                    ; FAIL e/for unmounted the branch
    (reset! !state [2])
    % := [::mount 2]                    ; branch is back up
    (dispose)
    % := [::unmount 2]))

(l/def x2 1)
(tests "object lifecycle 4"
  (def !input (atom [1 2]))
  (defn up-down [x trace!] (m/observe (fn [!] (trace! :up) (! x) #(trace! :down))))

  (with ((l/local
           (tap (e/for [id (new (m/watch !input))]
                  (binding [x2 (do id x2)]
                    (new (up-down x2 tap)))))) tap tap)
    [% %] := [:up :up]
    % := [1 1]
    (swap! !input pop)
    % := :down
    % := [1])
  % := :down)

(tests "reactive metadata"
  (def !x (atom 0))
  (with ((l/local (tap (meta (let [x (with-meta [] {:foo (e/watch !x)})] x)))) tap tap)
    % := {:foo 0}
    (swap! !x inc)
    (tap ::hi) % := ::hi))

(tests "undefined continuous flow, flow is not defined for the first 10ms"
  (let [flow (m/ap (m/? (m/sleep 10 :foo)))]
    (with ((l/local (tap (new (new (e/fn [] (let [a (new flow)] (e/fn [] a))))))) tap tap)
      (ex-message %) := "Undefined continuous flow.")))

(tests
  (def !x (atom 0))
  (with ((l/local (tap (try (-> (e/watch !x)
                              (doto (-> even? (when-not (throw (ex-info "odd" {})))))
                              (/ 2))
                            (catch #?(:clj Exception, :cljs :default) e (ex-message e))))) tap tap)
    % := 0
    (swap! !x inc)
    % := "odd"
    (swap! !x inc)
    % := 1))

(tests
  (def !x (atom 0))
  (def !f (atom "hello"))
  (def e (ex-info "error" {}))
  (with ((l/local
           (tap (try (if (even? (e/watch !x)) :ok (throw e))
                     (catch #?(:clj Throwable, :cljs :default) _ :caugh)
                     (finally (tap (e/watch !f)))))) tap tap)
   % := "hello"
   % := :ok
   (swap! !x inc)
   % := :caugh
   (reset! !f "world")
   % := "world"
   (swap! !x inc)
   % := :ok))

(l/def unbound1)
(l/def unbound2)
(tests
  (with ((l/local (tap (new (e/fn [] (binding [unbound1 1 unbound2 2] (+ unbound1 unbound2)))))) tap tap)
   % := 3))

#?(:clj
(tests
  "understand how Clojure handles unbound vars"
  ; In Clojure,
  ; Is unbound var defined or undefined behavior?
  ; What does it mean in CLJS? No vars in cljs.
  (def ^:dynamic y_964)
  (bound? #'y_964) := false
  (.isBound #'y_964) := false
  (def unbound (clojure.lang.Var$Unbound. #'y_964))
  (instance? clojure.lang.Var$Unbound unbound) := true

  ; leaking unbounded value
  (instance? clojure.lang.Var$Unbound y_964) := true

  ; not an error in clojure
  (try y_964 (catch Exception e nil))
  (instance? clojure.lang.Var$Unbound *1) := true)
)

(tests "In Electric, accessing an unbound var throws a userland exception"
  ;; An unbound var is either:
  ;; - an uninitialized p/def,
  ;; - an unsatisfied reactive fn parameter (reactive fn called with too few arguments).
  (l/def x)
  (with ((l/local x) prn tap)
    (ex-message %) := "Unbound electric var `hyperfiddle.electric-test/x`"))

(tests "Initial p/def binding is readily available in p/run"
 (def !x (atom 0))
 (l/def X (m/watch !x))
 (with ((l/local (tap (X.))) tap tap)
       % := 0
       (swap! !x inc)
       % := 1))

#?(:clj
   (tests ; GG: IDE doc on hover support
     "Vars created with p/def have the same metas as created with cc/def"
     (l/def Documented "p/def" :init)
     (select-keys (meta (var Documented)) [:name :doc])
     := {:name 'Documented
         :doc  "p/def"}))

#?(:clj
   (tests ; GG: IDE doc on hover support
    "Vars created with p/defn have the same metas as created with cc/defn"
    (l/defn Documented "doc" [a b c])
    (select-keys (meta (var Documented)) [:name :doc :arglists])
    := {:name 'Documented
        :doc  "doc"
        :arglists '([a b c])}))

(tests "pentagram of death - via Kenny Tilton"
  ; Key elements:
  ;  - two dependency chains from some property P leading back to one property X; and
  ;  - branching code in the derivation of P that will not travel the second dependency chain until a
  ;    certain condition is met; and
  ;  - by chance, propagation reaches P on the the first path before it reaches some intermediate property
  ;    I on the second dependency chain.
  ; The consequence is P updating once and reading (for the first time) property I, which has not yet been
  ; updated hence is inconsistent with the new value of X. This inconsistency is temporary (hence the name
  ; "glitch") because I will be updated soon enough and P will achieve consistency with X, but if one's
  ; reactive engine dispatches side effects off state change -- possible trouble.
  (def !aa (atom 1))
  (def !a7 (atom 7))
  (with
    ((l/local
       (let [aa  (e/watch !aa)
             a7  (e/watch !a7)
             a70 (* 10 a7)
             bb  aa
             cc  (* 10 aa)
             dd  (if (even? bb)
                   (* 10 cc)
                   42)]
         (tap (+ a70 bb (* 10000 dd))))) tap tap)
    % := 420071
    (swap! !aa inc)
    % := 2000072
    (swap! !aa inc)
    % := 420073))

(tests "pentagram of death reduced"
  ; the essence of the problem is:
  ; 1. if/case switch/change the DAG (imagine a railroad switch between two train tracks)
  ; 2. to have a conditional where the predicate and the consequent have a common dependency
  (def !x (atom 1))
  (with ((l/local (tap (let [p       (e/watch !x)
                             q       (tap (str p))
                             control (- p)]
                         (case control -1 p -2 q q)))) tap tap)
    % := "1"                                                ; cc/let sequences effects
    % := 1                                                  ; cross
    (swap! !x inc)
    % := "2"                                                ; q first touched
    % := "2"))

(tests "for with literal input"
  (with ((l/local (tap (e/for [x [1 2 3]] (tap x)))) tap tap)
    (hash-set % % %) := #{1 2 3}
    % := [1 2 3]))

(tests "for with literal input, nested"
  (def !x (atom 0))
  (with ((l/local (tap (when (even? (e/watch !x))
                         (e/for [x [1 2 3]]
                           (tap x))))) tap tap)
    (hash-set % % %) := #{1 2 3}
    % := [1 2 3]
    (swap! !x inc)
    % := nil))

(tests "nested closure"
  (def !x (atom 0))
  (with ((l/local (tap (new (let [x (e/watch !x)]
                              (if (even? x)
                                (e/fn [] :even)
                                (e/fn [] :odd)))))) tap tap)
    % := :even
    (swap! !x inc)
    % := :odd))

(tests "simultaneous add and remove in a for with a nested hook"
  (def !xs (atom [1]))
  (defn hook
    ([x] (tap [x]))
    ([x y] (tap [x y])))
  (with
    ((l/local
       (tap (new (e/hook hook 0
                   (e/fn []
                     (e/for [x (e/watch !xs)]
                       (new (e/hook hook x
                              (e/fn [] (str x)))))))))) tap tap)
    % := [1 nil]
    % := ["1"]
    (reset! !xs [2])
    % := [2 nil]
    % := ["2"]
    % := [1] ;; unmount on next frame ???
    )
  % := [2]
  % := [0])

(tests
  (def !t (atom true))
  (with ((l/local
           (tap (try (let [t (e/watch !t)]
                       (when t t (e/server t)))
                     (catch Pending _ :pending)
                     #_(catch Cancelled _ :cancelled)))) tap tap)
    % := :pending
    % := true
    (swap! !t not)
    % := nil))

(tests
  (def !state (atom true))
  (with ((l/local (when (e/watch !state) (tap :touch))) tap tap)
    % := :touch
    (reset! !state true)
    (tap ::nope) % := ::nope))

(tests "e/for in a conditional"
  (def !state (atom true))
  (with ((l/local (tap (if (e/watch !state) 1 (e/for [_ []])))) tap tap)
    % := 1
    (swap! !state not)
    % := []
    (swap! !state not)
    % := 1)
  )


(comment          ; we are not sure if this test has value. It is not minimized.
  (tests "Hack for e/for in a conditional. Passes by accident" ; PASS
    (def !state (atom true))
    (with ((l/local (tap (if (e/watch !state) 1 (try (e/for [_ []]) (catch Throwable t (throw t)))))) tap tap)
      % := 1
      (swap! !state not)
      % := []
      (swap! !state not)
      % := 1)))

(tests "Nested e/for with transfer"
  (def !state (atom [1]))
  (l/def state (e/watch !state))
  (with ((l/local (try (e/for [x (e/server state)]
                         (e/for [y (e/server state)]
                           (tap [x y])))
                       (catch Cancelled _)
                       (catch Pending _))) tap tap)
    % := [1 1]
    (reset! !state [3])
    % := [3 3]))

(tests
  "Static call"
  (with ((l/local (tap (Math/abs -1))) tap tap)
    % := 1))

#?(:clj
   (tests "Dot syntax works (clj only)"
     (with ((l/local (tap (. Math abs -1))) tap tap)
       % := 1)))

(tests "Sequential destructuring"
  (with ((l/local (tap (let [[x y & zs :as coll] [:a :b :c :d]] [x y zs coll]))) tap tap)
    % := [:a :b '(:c :d) [:a :b :c :d]]))

(tests "Associative destructuring"
  (with ((l/local (tap (let [{:keys [a ns/b d]
                              :as m
                              :or {d 4}}
                             {:a 1, :ns/b 2 :c 3}] [a b d m]))) tap tap)
    % := [1 2 4 {:a 1, :ns/b 2, :c 3}]))

(tests "Associative destructuring with various keys"
  (with ((l/local (tap (let [{:keys    [a]
                              :ns/keys [b]
                              :syms    [c]
                              :ns/syms [d]
                              :strs    [e]}
                             {:a 1, :ns/b 2, 'c 3, 'ns/d 4, "e" 5}]
                         [a b c d e]))) tap tap)
    % := [1 2 3 4 5]))

(tests "fn destructuring"
  (with ((l/local
           (try
             (tap (e/client ((fn [{:keys [a] ::keys [b]}] [::client a b]) {:a 1 ::b 2})))
             (tap (e/server ((fn [{:keys [a] ::keys [b]}] [::server a b]) {:a 1 ::b 2})))
             (catch Pending _))) tap tap))
    % := [::client 1 2]
    % := [::server 1 2])

(tests
  (def !xs (atom [false]))
  (with
    ((l/local
       (tap (try (e/for [x (e/watch !xs)]
                   (assert x))
                 (catch #?(:clj Error :cljs js/Error) _ :error)))) tap tap)
    % := :error
    (reset! !xs [])
    % := []))

(tests "All Pending instances are equal"
  (= (Pending.) (Pending.)) := true)

(tests
  "Failure instances are equal if the errors they convey are equal"
  (= (Failure. (Pending.)) (Failure. (Pending.))) := true

  (let [err (ex-info "error" {})]
    (= err err) := true
    (= (Failure. err) (Failure. err)) := true
    (= (ex-info "a" {}) (ex-info "a" {})) := false
    (= (Failure. (ex-info "err" {})) (Failure. (ex-info "err" {}))) := false))

(tests          ; temporary test because p/run does not serilize to transit.
  "Electric transit layer serializes unserializable values to nil"
  (electric-io/decode (electric-io/encode 1)) := 1
  (electric-io/decode (electric-io/encode (type 1))) := nil)

;; HACK sequences cljs async tests. Symptomatic of an RCF issue.
;; Ticket: https://www.notion.so/hyperfiddle/cljs-test-suite-can-produce-false-failures-0b3799f6d2104d698eb6a956b6c51e48
#?(:cljs (t/use-fixtures :each {:after #(t/async done (js/setTimeout done 1))}))

(tests
  (def !x (atom true))
  (with ((l/local
           (try
             (let [x (e/watch !x)]
                                        ; check eager network does not beat the switch
               (tap (if x (e/server [:server x]) [:client x])))
             (catch Pending _))) tap tap)
    % := [:server true]
    (swap! !x not)
                                        ; the remote tap on the switch has been removed
    % := [:client false]))

(tests
  (def !x (atom true))
  (l/def x (e/server (e/watch !x)))
  (with ((l/local
           (try
             (if (e/server x) ; to be consistent, client should see x first and switch
               (e/server (tap x)) ; but test shows that the server sees x change before client
               (e/server x))
             (catch Pending _))) tap tap)
   % := true
   (swap! !x not)
   % := false #_ ::rcf/timeout)
  ; we have to choose: consistency or less latency?
  ; current behavior - Dustin likes, Leo does not like
  )

;; https://www.notion.so/hyperfiddle/distribution-glitch-stale-local-cache-of-remote-value-should-be-invalidated-pending-47f5e425d6cf43fd9a37981c9d80d2af
(tests "glitch - stale local cache of remote value should be invalidated/pending"
  (def !x (atom 0))
  (def dispose ((l/local (tap (try (let [x (new (m/watch !x))]
                                     ;; pending or both equal
                                     [x (e/server x)])
                                   (catch Pending _ ::pending)))) tap tap))
  % := ::pending
  % := [0 0]
  (swap! !x inc)
  % := ::pending
  % := [1 1]
  (dispose))

(comment
  ; https://www.notion.so/hyperfiddle/p-fn-transfer-d43869c673574390b186ccb4df824b39
  ((l/local
     (e/server
       (let [Foo (e/fn [] (type 1))]
         (tap (Foo.))
         (tap (e/client (Foo.)))))) tap tap)
  % := "class java.lang.Long"
  % := "class #object[Number]"

  ; implications - all ~e/fns~ neutral electric expressions are compiled for both peers, including
  ; the parts that don't make sense, because you don't know in advance which peer will
  ; run which function

  ; costs:
  ; increases size of compiler artifacts
  ; increases compile times
  )

(tests
  (with ((l/local (try (e/server
                         (let [foo 1]
                           (tap foo)
                           (tap (e/client foo))))
                       (catch Pending _))) tap tap)
    % := 1
    % := 1))

(tests "Today, bindings fail to transfer, resulting in unbound var exception. This will be fixed"
                                        ; https://www.notion.so/hyperfiddle/photon-binding-transfer-unification-of-client-server-binding-7e56d9329d224433a1ee3057e96541d1
  (l/def foo)
  (with ((l/local (try
                    (e/server
                      (binding [foo 1]
                        (tap foo)
                        (tap (e/client foo))))
                    (catch Pending _)
                    (catch #?(:clj Error :cljs js/Error) e
                      (tap e)))) tap tap)
    % := 1
                                        ; % := 1 -- target future behavior
    (type %) := #?(:clj Error :cljs js/Error)))

(tests "static method call"
  (with ((l/local (tap (Math/max 2 1))) tap tap)
    % := 2))

(tests "static method call in e/server"
  (with ((l/local (try (tap (e/server (Math/max 2 1)))
                       (catch Pending _))) tap tap)
    % := 2))

(tests "static method call in e/client"
  (with ((l/local (try (tap (e/server (subvec (vec (range 10))
                                        (Math/min 1 1)
                                        (Math/min 3 3))))
                       (catch Pending _))) tap tap)
    % := [1 2]))

(tests "Inline cc/fn support"
  (def !state (atom 0))
  (l/def global)
  (with ((l/local (let [state (e/watch !state)
                        local [:local state]
                        f     (binding [global [:global state]]
                                (fn ([a] [a local hyperfiddle.electric-test/global])
                                  ([a b] [a b local global])
                                  ([a b & cs] [a b cs local global])))]
                    (tap (f state))
                    (tap (f state :b))
                    (tap (f state :b :c :d)))) tap tap)
    % := [0 [:local 0] [:global 0]]
    % := [0 :b [:local 0] [:global 0]]
    % := [0 :b '(:c :d) [:local 0] [:global 0]]
    (swap! !state inc)
    % := [1 [:local 1] [:global 1]]
    % := [1 :b [:local 1] [:global 1]]
    % := [1 :b '(:c :d) [:local 1] [:global 1]]))

(tests "cc/fn lexical bindings are untouched"
  (with ((l/local (let [a 1
                        b 2
                        f (fn [a] (let [b 3] [a b]))]
                    (tap (f 2)))) tap tap)
    % := [2 3]))

(tests "Inline cc/fn shorthand support"
  (with ((l/local (tap (#(inc %) 1))) tap tap)
    % := 2))

(tests "inline m/observe support"
  (let [!state (atom 0)]
    (with ((l/local (let [state     (e/watch !state)
                          lifecycle (m/observe (fn [push]
                                                 (tap :up)
                                                 (push state)
                                                 #(tap :down)))
                          val       (new lifecycle)]
                      (tap val))) tap tap)
      % := :up
      % := 0
      (swap! !state inc)
      % := :down
      % := :up
      % := 1)
    % := :down))

(tests "Inline letfn support"
  (with ((l/local (tap (letfn [(descent  [x] (cond (pos? x) (dec x)
                                                   (neg? x) (inc x)
                                                   :else    x))
                               (is-even? [x] (if (zero? x) true  (is-odd?  (descent x))))
                               (is-odd?  [x] (if (zero? x) false (is-even? (descent x))))]
                         (tap [(is-even? 0) (is-even? 1) (is-even? 2) (is-even? -2)])
                         (tap [(is-odd?  0) (is-odd?  2) (is-odd?  3) (is-odd? -3)])))) tap tap)
    % := [true false true true]
    % := [false false true true]
    % := [false false true true]))

(tests
  (with ((l/local (try (letfn [(foo [])]
                         (tap (e/watch (atom 1))))
                       (catch Throwable t (prn t)))) tap tap)
    % := 1))

(tests "Inline letfn support"
  (def !state (atom 0))
  (l/def global)
  (with ((l/local (let [state (e/watch !state)
                        local [:local state]]
                    (binding [global [:global state]]
                      (letfn [(f ([a] [a local hyperfiddle.electric-test/global])
                                ([a b] [a b local global])
                                ([a b & cs] [a b cs local global]))]
                        (tap (f state))
                        (tap (f state :b))
                        (tap (f state :b :c :d)))))) tap tap)
    % := [0 [:local 0] [:global 0]]
    % := [0 :b [:local 0] [:global 0]]
    % := [0 :b '(:c :d) [:local 0] [:global 0]]
    (swap! !state inc)
    % := [1 [:local 1] [:global 1]]
    % := [1 :b [:local 1] [:global 1]]
    % := [1 :b '(:c :d) [:local 1] [:global 1]]))

#?(:clj
   (tests "e/fn is undefined in clojure-land"
     (tap (try (lang/analyze {} `(fn [] (e/fn []))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric/fn) inside a Clojure function"))

#?(:clj
   (tests "e/client is undefined in clojure-land"
     (tap (try (lang/analyze {} `(fn [] (e/client []))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric/client) inside a Clojure function"))

#?(:clj
   (tests "e/server is undefined in clojure-land"
     (tap (try (lang/analyze {} `(fn [] (e/server []))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric/server) inside a Clojure function"))

#?(:clj
   (tests "e/server is undefined in clojure-land"
     (tap (try (lang/analyze {} `(fn [] (e/watch (atom :nomatter)))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric/watch) inside a Clojure function"))

(tests "cycle"
  (with ((l/local
           (let [!F (atom (e/fn [] 0))]
             (tap (new (new (m/watch !F))))
             (let [y 1] (reset! !F (e/fn [] y))))) tap tap)
    % := 0
    % := 1))

#?(:clj ; test broken in cljs, not sure why
   (tests "loop/recur"
     (l/defn fib [n] (loop [n n] (if (<= n 2) 1 (+ (recur (dec n)) (recur (- n 2))))))
     (with ((l/local (tap (e/for [i (range 1 11)] (fib. i)))) tap tap)
       % := [1 1 2 3 5 8 13 21 34 55])))

;; currently broken https://www.notion.so/hyperfiddle/cr-macro-internal-mutation-violates-photon-purity-requirement-119c18755ddd466384beb15f1e2317c5
#_
(tests
  "inline m/cp support"
  (let [!state (atom 0)]
    (with (p/run (let [state (p/watch !state)]
                   (tap (new (m/cp state)))))
      % := 0
      (swap! !state inc)
      % := 1))

  "inline m/ap support"
  (let [!state (atom [1])]
    (with (p/run (let [coll (p/watch !state)]
                   (tap (new (m/ap (tap (m/?< (m/seed coll))))))))
      % := 1
      % := 1
      (swap! !state conj 2)
      % := 1
      % := 2
      % := 2)))

(tests "letfn body is electric"
  (l/def z 3)
  (def !x (atom 4))
  (with ((l/local (let [y 2] (letfn [(f [x] (g x)) (g [x] [x y z])] (tap (f (e/watch !x)))))) tap tap)
    % := [4 2 3]
    (swap! !x inc)
    % := [5 2 3]))

;; currently broken https://www.notion.so/hyperfiddle/cr-macro-internal-mutation-violates-photon-purity-requirement-119c18755ddd466384beb15f1e2317c5
#_
(tests
  "inline m/sp support"
  (let [!state (atom 0)]
    (with (p/run (let [val  (p/watch !state)
                       task (m/sp val)]
                   (tap (new (m/relieve {} (m/reductions {} :init (m/ap (m/? task))))))))
      % := 0
      (swap! !state inc)
      % := 1
      )))

#?(:clj
   (tests "set!"
     (def !y (atom 8))
     (with ((l/local (let [pt (java.awt.Point. 1 2)
                           y (e/watch !y)]
                       (set! (.-y pt) y)
                       ;; calling (.-y pt) doesn't work, it's deduped
                       (tap [y pt]))) tap tap)
       % := [8 (java.awt.Point. 1 8)]
       (swap! !y inc)
       % := [9 (java.awt.Point. 1 9)])))

#?(:cljs
   (do-browser
     (tests "set!"
       ;; https://www.notion.so/hyperfiddle/RCF-implicit-do-rewrite-rule-does-not-account-for-let-bindings-61b1ad82771c407198c1f678683bf443
       (defn bypass-rcf-bug [[href a]] [href (str/replace (.-href a) #".*/" "")])
       (def !href (atom "href1"))
       (with ((l/local (let [a (.createElement js/document "a")
                             href (e/watch !href)]
                         (set! (.-href a) href)
                         (tap [href a]))) tap tap)
         (bypass-rcf-bug %) := ["href1" "href1"]
         (reset! !href "href2")
         (bypass-rcf-bug %) := ["href2" "href2"]))))

#?(:clj (tests "set! with electric value"
          (with ((l/local (tap (let [pt (java.awt.Point. 1 2)]
                                 (set! (.-y pt) (new (e/fn [] 0)))))) tap tap)
            % := 0)))

#?(:cljs (tests "set! with electric value"
           (with ((l/local (tap (let [o (js/Object.)]
                                  (set! (.-x o) (new (e/fn [] 0)))))) tap tap)
             % := 0)))

(tests "e/fn arity check"
  (with ((l/local (try (new (e/fn [x y z] (throw (ex-info "nope" {}))) 100 200 300 400)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called <unnamed-efn> with 4 arguments but it only supports 3"))

(l/defn ThreeThrow [_ _ _] (throw (ex-info "nope")))
(tests "e/fn arity check"
  (with ((l/local (try (new ThreeThrow 100 200 300 400)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called ThreeThrow with 4 arguments but it only supports 3"))

(tests "e/fn arity check"
  (with ((l/local (try (new (e/fn Named [x y] (throw (ex-info "nope" {}))) 100)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called Named with 1 argument but it only supports 2"))

(tests "Partial application"
  (with ((l/local
           (tap (new (e/partial 0 (e/fn [] :a)) ))
           (tap (new (e/partial 1 (e/fn [a] a) :a)))
           (tap (new (e/partial 2 (e/fn [a b] [a b]) :a) :b))
           (tap (new (e/partial 4 (e/fn [a b c d] [a b c d]) :a :b) :c :d))) tap tap)
    % := :a
    % := :a
    % := [:a :b]
    % := [:a :b :c :d]))

(l/def Factorial-gen (e/fn [Rec]
                       (e/fn [n]
                         (if (zero? n)
                           1
                           (* n (new Rec (dec n)))))))

(l/def Y "Y-Combinator"
  (e/fn [f]
    (new
      (e/fn [x] (new x x))
      (e/fn [x] (new f (e/fn [y] (new (new x x) y)))))))

(tests "Y-Combinator"
  (let [!n (atom 5)]
    (with ((l/local (tap (new (Y. Factorial-gen) (e/watch !n)))) tap tap)
      % := 120
      (reset! !n 20)
      % := 2432902008176640000)))

(tests "clojure def inside electric code"
  (def !x (atom 0))
  (with ((l/local (def --foo (tap (e/watch !x)))) tap tap)
                    % := 0, --foo := 0
    (swap! !x inc)  % := 1, --foo := 1))

(tests "catch handlers are work skipped"
  (def !x (atom 0))
  (with ((l/local (try (e/watch !x)
                  (throw (ex-info "hy" {}))
                  (catch ExceptionInfo e (tap e))
                  (catch Cancelled _ (tap :cancelled)))) tap tap)
   (ex-message %) := "hy"      ; exception tapped by `ExceptionInfo` catch block
   (swap! !x inc))              ; same exception, so work skipped
  % := :cancelled)

(tests "pendings don't enter cc/fn's"
  (with ((l/local (try (let [v (new (m/observe (fn [!] (! r/pending) (def ! !) #(do))))]
                         (#(tap [:v %]) v))
                       (catch Pending _ (tap :pending))
                       (catch #?(:clj Throwable :cljs :default) e (prn [(type e) (ex-message e)])))) tap tap)
    % := :pending
    (! 1)
    % := [:v 1]))

(tests "catch code reacts to changes"
  (def !x (atom 0))
  (with ((l/local (tap (try (throw (ex-info "boom" {}))
                            (catch Throwable _ (e/watch !x))))) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "Electric dynamic scope is available in cc/fn"
  (l/def ^:dynamic dynfoo 1)
  (with ((l/local
           (try
             ((fn []
                (tap dynfoo)))
             (binding [dynfoo 2]
               ((fn [] (tap dynfoo))))
             (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
    % := 1
    % := 2))

#?(:clj ; fail to compile in cljs: `Can't set! local var or non-mutable field` (foo177584 is not dynamic)
   (tests "l/def are not dynamic by default in cc/fn"
     (l/def foo177584 1)
     (with ((l/local
              (try
                ((fn [] (binding [foo177584 2] (tap foo177584)))) ; foo177584 is not ^:dynamic
                (catch #?(:clj Throwable, :cljs js/Error) t (tap (ex-message t))))) tap tap)
       % := "Can't dynamically bind non-dynamic var: hyperfiddle.electric-test/foo177584")))

(tests "Injecting an l/def binding in cc/fn respects dynamic scope rules"
  (l/def ^:dynamic dynfoo 1)
  (with ((l/local
           (try
             (tap dynfoo)               ; electric dynamic context
             (binding [dynfoo 2]        ; rebound in electic context
               ((fn []
                  (tap dynfoo)          ; injected dynamic context
                  (binding [dynfoo 3]   ; rebound in clojure context
                    (tap dynfoo)        ; read clojure context
                    )))
               (tap dynfoo))            ; cc/fn scope doesn't alter electric scope
             (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
    % := 1
    % := 2
    % := 3
    % := 2))

(tests "In Clojure, unqualified names first resolves to lexical scope"
  (def ^:dynamic foo 1)
  foo := 1 ; no lexical binding shadowing -> resolve to foo var
  (let [foo 2] ; lexical shadowing
    foo := 2   ; resolve to lexical scope
    (binding [#?(:clj foo, :cljs hyperfiddle.electric-test/foo) 3] ; always rebind var in clojure. Cljs requires fully qualified name.
      foo := 2 ; unqualified name resolves to lexical scope
      hyperfiddle.electric-test/foo := 3))) ; qualified name resolves to the var

#?(:clj
   (tests "cc/fn args shadow l/def injections"
     (l/def ^:dynamic dynfoo 1)
     (with ((l/local
              (try
                (tap dynfoo)            ; electric dynamic context
                ((fn [dynfoo]           ; dynvar shadowed by argument
                   (tap dynfoo)
                   (binding [dynfoo 2]  ; rebinds the vars
                     (tap dynfoo)  ; still resolves to argument in lexical scope
                     (tap hyperfiddle.electric-test/dynfoo)))
                 :argument)
                (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
       % := 1
       % := :argument
       % := :argument
       % := 2)))

#?(:clj
   (tests "Injected lexical scope respects precedence over injected dynamic scope"
     (l/def ^:dynamic dynfoo 1)
     (with ((l/local
              (try
                (tap dynfoo)
                (let [dynfoo :shadowed]
                  ((fn []
                     (tap dynfoo)
                     (binding [dynfoo 2]
                       (tap dynfoo)
                       (tap hyperfiddle.electric-test/dynfoo)))
                   ))
                (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
       % := 1
       % := :shadowed
       % := :shadowed
       % := 2)))

#?(:clj
   (tests "Shadowing injected dynamic scope in cc context respects clojure shadowing rules"
     (l/def ^:dynamic dynfoo 1)
     (with ((l/local
              (try
                (tap dynfoo)
                ((fn []
                   (tap dynfoo)
                   (let [dynfoo :shadowed]
                     (tap dynfoo)
                     (binding [dynfoo 2]
                       (tap dynfoo)
                       (tap hyperfiddle.electric-test/dynfoo)))))
                (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
       % := 1
       % := 1
       % := :shadowed
       % := :shadowed
       % := 2)))

(tests "snapshot"
  (def flow (e/-snapshot (m/observe (fn [!] (def ! !) #()))))
  "1 2 -> 1"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (! 1),         % := :notified, @it := 1
  (! 2)
  (it),           % := :terminated
  "Pending 1 2 -> Pending 1"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (! r/pending), % := :notified, @it := r/pending
  (! 1),         % := :notified, @it := 1
  (! 2)
  (it),           % := :terminated
  "Pending Pending 1 2 -> Pending Pending 1"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (! r/pending), % := :notified, @it := r/pending
  (! r/pending), % := :notified, @it := r/pending
  (! 1),         % := :notified, @it := 1
  (! 2)
  (it),           % := :terminated
  "ex-info 1 2 -> ex-info"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (def boom (Failure. (ex-info "boom" {})))
  (! boom),      % := :notified, @it := boom
  (! 1)
  (! 2)
  (it),           % := :terminated
  "1 Pending 2 -> 1"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (! 1),         % := :notified, @it := 1
  (! r/pending)
  (! 2)
  (it),           % := :terminated
  "Pending ex-info 1 -> Pending ex-info"
  (def it (flow #(tap :notified) #(tap :terminated)))
  (def boom (Failure. (ex-info "boom" {})))
  (! r/pending), % := :notified, @it := r/pending
  (! boom),      % := :notified, @it := boom
  (! 1)
  (it),           % := :terminated

  (tap ::done), % := ::done, (println " ok"))

(tests "for-event"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (with ((l/local (tap (try (e/for-event [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
                              (let [!v (atom :pending)]
                                (swap! !resolvers assoc e !v)
                                (try (let [v (e/watch !v)]
                                       (case v
                                         :pending (throw (Pending.))
                                         :caught (throw (ex-info "caught" {}))
                                         :uncaught (throw (ex-info "uncaught" {}))
                                         #_else v))
                                     (catch Pending _ :pending)
                                     (catch #?(:clj Throwable :cljs :default) e
                                       (case (ex-message e)
                                         "caught" (reduced nil)
                                         #_else (throw e))))))
                            (catch #?(:clj Throwable :cljs :default) e [(type e) (ex-message e)])))) tap tap)
    #_init                 % := []
    (@! 0),                % := [:pending]
    (@! 1),                % := [:pending :pending]
    (!! 1 (reduced nil)),  % := [:pending nil], % := [:pending]
    (!! 0 (reduced true)), % := [nil], % := []
    (@! 2),                % := [:pending]
    (!! 2 :caught),        % := [nil], % := []
    (@! 99),               % := [:pending]
    (!! 99 :uncaught),     % := [ExceptionInfo "uncaught"]
    (!! 99 :alive),        % := [:alive]
    (!! 99 (reduced nil)), % := [nil], % := []))

(tests "for-event-pending"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/local (tap (e/for-event-pending [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
                         (let [!v (atom :pending)]
                           (swap! !resolvers assoc e !v)
                           (let [v (e/watch !v)]
                             (case v
                               :pending (throw (Pending.))
                               :fail (throw fail)
                               #_else v)))))) tap tap)
    #_init        % := [::e/init]
    (@! 0),       % := [::e/pending e/pending]
    (@! 1)        ;; work skipped
    (!! 1 nil)    ;; work skipped, 0 still pending
    (!! 0 false)  % := [::e/ok false]
    (@! 2),       % := [::e/pending e/pending]
    (!! 2 :fail), % := [::e/failed fail]))

(tests "for-event-pending-switch"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/local (tap (e/for-event-pending-switch [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
                         (let [!v (atom :pending)]
                           (swap! !resolvers assoc e !v)
                           (e/on-unmount #(tap [:unmounted e]))
                           (let [v (e/watch !v)]
                             (case v
                               :pending (throw (Pending.))
                               :fail (throw fail)
                               #_else v)))))) tap tap)

    #_init                             % := [::e/init]
    (@! 0),                            % := [::e/pending e/pending]
    (@! 1),       % := [:unmounted 0]
    (@! 2),       % := [:unmounted 1]
    (!! 2 nil),   % := [:unmounted 2], % := [::e/ok nil]
    (@! 3),                            % := [::e/pending e/pending]
    (!! 3 :fail), % := [:unmounted 3], % := [::e/failed fail]))

(tests "do-event"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (with ((l/local (tap (try (e/do-event [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
                              (tap [:mount e])
                              (let [!v (atom :pending)]
                                (swap! !resolvers assoc e !v)
                                (try (let [v (e/watch !v)]
                                       (case v
                                         :pending (throw (Pending.))
                                         :caught (throw (ex-info "caught" {}))
                                         :uncaught (throw (ex-info "uncaught" {}))
                                         #_else v))
                                     (catch Pending _ :pending)
                                     (catch #?(:clj Throwable :cljs :default) e
                                       (case (ex-message e)
                                         "caught" (reduced nil)
                                         #_else (throw e))))))
                            (catch #?(:clj Throwable :cljs :default) e [(type e) (ex-message e)])))) tap tap)
    #_init                   % := nil
    (@! 0), % := [:mount 0], % := :pending
    (@! 1)                              ; skipped, previous still running
    (!! 0 (reduced false)),  % := nil
    (@! 2), % := [:mount 2], % := :pending
    (!! 2 :caught),          % := nil
    (@! 9), % := [:mount 9], % := :pending
    (!! 9 :uncaught),        % := [ExceptionInfo "uncaught"]
    (!! 9 :alive),           % := :alive
    (!! 9 (reduced true)),   % := nil))

(tests "do-event-pending"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/local (tap (e/do-event-pending [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
                         (tap [:mount e])
                         (let [!v (atom :pending)]
                           (swap! !resolvers assoc e !v)
                           (let [v (e/watch !v)]
                             (case v
                               :pending (throw (Pending.))
                               :fail (throw fail)
                               #_else v)))))) tap tap)
    #_init                   % := [::e/init]
    (@! 0), % := [:mount 0], % := [::e/pending e/pending]
    (@! 1)        ;; skipped
    (!! 0 false)             % := [::e/ok false]
    (@! 2), % := [:mount 2], % := [::e/pending e/pending]
    (!! 2 :fail),            % := [::e/failed fail]))

#?(:clj
   (tests "e/offload starts Pending"
     (def dfv (m/dfv))
     (with ((l/local (tap (try (e/offload #(m/? dfv))
                               (catch Pending ex ex)
                               (catch Throwable ex (prn ex))))) tap tap)
       % := e/pending
       (dfv 1)
       % := 1)))

#?(:clj
   (tests "e/offload doesn't throw Pending subsequently"
     (def !dfv (atom (m/dfv)))
     (with ((l/local (tap (try (let [dfv (e/watch !dfv)]
                                 (e/offload #(m/? dfv)))
                               (catch Pending ex ex)
                               (catch Throwable ex (prn ex))))) tap tap)
       % := e/pending
       (@!dfv 1)
       % := 1
       (reset! !dfv (m/dfv))
       (@!dfv 2)
       % := 2)))

#?(:clj
    (tests "e/offload on overlap uses latest value and discards previous"
      (def d1 (m/dfv))
      (def !dfv (atom d1))
      (with ((l/local (try (let [dfv (e/watch !dfv)]
                             (tap (e/offload #(m/? dfv))))
                           (catch Pending _)
                           (catch Throwable ex (prn [(type ex) (ex-message ex)])))) tap tap)

        (def d2 (reset! !dfv (m/dfv)))
        (d2 2)
        % := 2
        (d1 1))))

#?(:clj
   (tests "e/offload thunk is running on another thread"
     (defn get-thread [] (Thread/currentThread))
     (with ((l/local (try (tap (e/offload get-thread))
                          (catch Pending _)
                          (catch Throwable ex (prn ex)))) tap tap)
       (count (hash-set % (get-thread))) := 2)))

#?(:cljs
   (do-browser
     (tests "goog module calls don't trigger warnings"
       ;; this includes a goog test namespace, so if there are warnings the CI will blow up.
       ;; The blow up is configured as a shadow build hook in `hyperfiddle.browser-test-setup`
       (with ((l/local (tap (try (hyperfiddle.goog-calls-test/Main.) :ok
                                 (catch :default ex (ex-message ex))))) tap tap)
         % := :ok))))

(tests
  (with ((l/local (tap (try (new nil) (catch #?(:clj Throwable :cljs :default) e e)))) tap tap)
    (ex-message %) := "called `new` on nil"))

(tests
  (with ((l/local (tap (try (e/watch :foo) (throw (ex-info "nope" {}))
                            (catch ExceptionInfo e e)))) tap tap)
    (str/includes? (ex-message %) ":foo") := true))

(tests "l/def initialized to `nil` works in cc/fn"
  (l/def foo nil)
  (with ((l/local (binding [foo "foo"] (let [f foo] (#(tap [f foo]))))) tap tap)
    % := ["foo" "foo"]))

(tests "e/fn varargs"
  (with ((l/single+ {} (new (e/fn [x & xs] (tap [x xs])) 1 2 3 4)) tap tap)
    % := [1 [2 3 4]]))
(tests "e/fn varargs recursion with recur"
  (with ((l/single+ {} (new (e/fn [x & xs] (tap [x xs])) 1 2 3 4)) tap tap)
    % := [1 [2 3 4]]))
(tests "e/fn varargs recur is arity-checked"
  (with ((l/single+ {} (tap (try (new (e/fn [x & xs] (recur)) 1 2 3)
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You `recur`d in <unnamed-efn> with 0 arguments but it has 2 positional arguments"))

(l/defn MapVararg [& {:keys [x] :or {x 1} :as mp}] [x mp])
(tests "map vararg with no args is nil"
  (with ((l/single+ {} (tap (MapVararg.))) tap tap)
    % := [1 nil]))
(tests "map vararg with kw args"
  (with ((l/single+ {} (tap (MapVararg. :x 2))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with map arg"
  (with ((l/single+ {} (tap (MapVararg. {:x 2}))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with mixture"
  (with ((l/single+ {} (tap (MapVararg. :y 3 {:x 2}))) tap tap)
    % := [2 {:x 2, :y 3}]))
(tests "map vararg trailing map takes precedence"
  (with ((l/single+ {} (tap (MapVararg. :x 3 {:x 2}))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with positional arguments"
  (with ((l/single+ {} (tap (new (e/fn [a & {:keys [x]}] [a x]) 1 :x 2))) tap tap)
    % := [1 2]))

(tests "e/fn recur is arity checked"
  (with ((l/local (tap (try (new (e/fn X [x] (recur x x)) 1)
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You `recur`d in X with 2 arguments but it has 1 positional argument"))

(l/defn Zero [])
(l/defn One [x] x)
(l/defn Two [x y] [x y])
(l/defn VarArgs [x & xs] [x xs])
(tests "(new One 1)"
       (with ((l/local (tap (new One 1))) tap tap)
         % := 1))
(tests "(new VarArgs 1 2 3)"
       (with ((l/local (tap (new VarArgs 1 2 3))) tap tap)
         % := [1 [2 3]]))
(tests "varargs arity is checked"
       (with ((l/local (tap (try (new VarArgs)
                                 (catch ExceptionInfo e e)))) tap tap)
         (ex-message %) := "You called VarArgs with 0 arguments but it only supports 1"))

#?(:clj
   (tests "e/apply"
     (try (macroexpand '(e/apply Zero))
          (catch Compiler$CompilerException e
            (ex-message (ex-cause e))
            := "Assert failed: hyperfiddle.electric/apply takes and Electric function and at least one argument. Given 0.\n(not (empty? args))"))))
(tests "e/apply"
  (with ((l/single+ {} (tap (e/apply VarArgs [1 2 3]))) tap tap)
    % := [1 [2 3]]))
(tests "e/apply"
  (with ((l/single+ {} (tap (e/apply Two 1 [2]))) tap tap)
    % := [1 2]))
(tests "e/apply"
  (with ((l/single+ {} (tap (e/apply Two [1 2]))) tap tap)
    % := [1 2]))
(tests "e/apply"
  (with ((l/single+ {} (tap (e/apply Two [1 (inc 1)]))) tap tap)
    % := [1 2]))
(tests "e/apply"
  (with ((l/single+ {} (tap (try (e/apply Two [1 2 3]) (throw (ex-info "boo" {}))
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You called Two with 3 arguments but it only supports 2"))

(tests "multi-arity e/fn"
  (with ((l/local (tap (new (e/fn ([_] :one) ([_ _] :two)) 1))) tap tap)
    % := :one))
(tests "multi-arity e/fn"
  (with ((l/local (tap (new (e/fn ([_] :one) ([_ _] :two)) 1 2))) tap tap)
    % := :two))
(tests "multi-arity e/fn"
  (with ((l/local (tap (new (e/fn ([_]) ([_ & xs] (mapv inc xs))) 1 2 3 4))) tap tap)
    % := [3 4 5]))
(tests "multi-arity e/fn"
  (with ((l/single+ {} (tap (e/apply (e/fn ([_] :one) ([_ _] :two)) 1 [2]))) tap tap)
    % := :two))
(tests "multi-arity e/fn"
  (with ((l/single+ {} (tap (e/apply (e/fn ([_]) ([_ & xs] (mapv inc xs))) 1 2 [3 4]))) tap tap)
    % := [3 4 5]))

(tests "self-recur by name, e/fn"
  (with ((l/local (tap (new (e/fn fib [n] (case n 0 0 1 1 (+ (fib. (- n 1)) (fib. (- n 2))))) 6))) tap tap)
    % := 8))
(tests "self-recur by name, l/defn"
  (l/defn Fib [n] (case n 0 0 1 1 (+ (Fib. (- n 1)) (Fib. (- n 2)))))
  (with ((l/local (tap (Fib. 7))) tap tap)
    % := 13))
(tests "self-recur by name, e/fn thunk"
  (def !x (atom 2))
  (with ((l/local (new (e/fn X [] (if (pos-int? (tap (swap! !x dec))) (X.) (tap :done))))) tap tap)
    % := 1
    % := 0
    % := :done))
(tests "self-recur by name, to different arity"
  (with ((l/local (tap (new (e/fn X ([] (X. 0)) ([n] (inc n)))))) tap tap)
    % := 1))
(tests "self-recur by name, varargs"
  (with ((l/local (new (e/fn Chomp [& xs] (if (tap (seq xs)) (Chomp.) (tap :done))) 0 1 2)) tap tap)
    % := [0 1 2]
    % := nil
    % := :done))

#?(:clj
   (tests "e/fn multi-arity mistakes"
     (binding [expand/*electric* true]
       (try (expand/all {} '(e/fn Named ([x] x) ([y] y)))
            (catch Throwable e (tap e)))
       (ex-message (ex-cause %)) := "Conflicting arity definitions in Named: [x] and [y]"

       (try (expand/all {} '(e/fn Named ([x] x) ([& ys] ys)))
            (catch Throwable e (tap e)))
       (ex-message (ex-cause %)) := "Conflicting arity definitions in Named: [x] and [& ys]"

       (try (expand/all {} '(e/fn ([x & ys] x) ([x y & zs] ys)))
            (catch Throwable e (tap e)))
       (ex-message (ex-cause %)) := "Conflicting arity definitions: [x & ys] and [x y & zs]")))

#?(:cljs
   (tests "#js"
     (def !x (atom 0))
     (with ((l/local (let [x (e/watch !x)]
                       (tap #js {:x x})
                       (tap #js [:x x]))) tap tap)
       (.-x %) := 0
       (aget % 1) := 0
       (swap! !x inc)
       (.-x %) := 1
       (aget % 1) := 1)))

#?(:clj
   (tests "jvm interop"
     (with ((l/local
              (let [f (java.io.File. "src")
                    pt (java.awt.Point. 1 2)]
                (tap [(.getName f)                            ; instance method
                      (.-x pt)                                ; field access
                      (java.awt.geom.Point2D/distance 0 0 1 0) ; static method
                      ]))) tap tap)
       % := ["src" 1 1.0])))

#?(:cljs
   (tests "js interop"
     (with ((l/local
              (let [^js o #js {:a 1 :aPlus (fn [n] (inc n))}]
                (tap [(.aPlus o 1)      ; instance method
                      (.-a o)           ; field access
                      ]))) tap tap)
       % := [2 1])))

#?(:clj
   (tests "we capture invalid calls"
     (binding [expand/*electric* true]
       (try (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server) '(jjj 1))
            (throw (Throwable. "shouldn't"))
            (catch ExceptionInfo e
              (ex-message e) := "in: (jjj 1)\nI cannot resolve `jjj`, maybe it's defined only on the client?\nIf `jjj` is supposed to be a macro, you might need to :refer it in the :require-macros clause."
              (:form (ex-data e)) := 'jjj))

       "in cc/fn"
       (try (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server) '(fn [] (jjj 1)))
            (throw (Throwable. "shouldn't"))
            (catch ExceptionInfo e
              (ex-message e) := "in: (jjj 1)\nI cannot resolve `jjj`, maybe it's defined only on the client?\nIf `jjj` is supposed to be a macro, you might need to :refer it in the :require-macros clause."
              (:form (ex-data e)) := 'jjj))

       "named cc/fn"
       (try (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server) '(fn foo [] (jjj 1)))
            (throw (Throwable. "shouldn't"))
            (catch ExceptionInfo e
              (ex-message e) := "in: (jjj 1)\nI cannot resolve `jjj`, maybe it's defined only on the client?\nIf `jjj` is supposed to be a macro, you might need to :refer it in the :require-macros clause."
              (:form (ex-data e)) := 'jjj))

       "in letfn"
       (try (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server) '(letfn [(foo [] (jjj 1))]))
            (throw (Throwable. "shouldn't"))
            (catch ExceptionInfo e
              (ex-message e) := "in: (jjj 1)\nI cannot resolve `jjj`, maybe it's defined only on the client?\nIf `jjj` is supposed to be a macro, you might need to :refer it in the :require-macros clause."
              (:form (ex-data e)) := 'jjj))

       "arbitrary symbols"
       (try (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server)
              '(let [x js/document.body]))
            (catch ExceptionInfo e
              (ex-message e) := "in: (let* [x js/document.body])\nI cannot resolve `js/document.body`, maybe it's defined only on the client?\nIf `js/document.body` is supposed to be a macro, you might need to :refer it in the :require-macros clause."
              (:form (ex-data e)) := 'js/document.body))

       "clj static field works"
       (lang/analyze (assoc (l/->local-config {}) ::lang/current :server ::lang/me :server) 'clojure.lang.PersistentArrayMap/EMPTY))))

(tests "e/server e/client body"
  (with ((l/local (tap (e/client 1 2))) tap tap)
    % := 2))

(defn signify [node] (symbol (str/replace (str node) #"_hf_.*" "")))

#?(:clj
   (tests "we keep node order"
     (l/def A 1)
     (l/def B 2)
     (l/def C 3)
     ;; (require '[hyperfiddle.electric.impl.ir-utils :as ir-utils])

     (->> (lang/analyze (assoc (l/->local-config {}) ::lang/current :client ::lang/me :client)
            '[A (e/server B) C])
       ;; ir-utils/unwrite
       r/find-nodes (mapv signify))
     :=
     (->> (lang/analyze (assoc (l/->local-config {}) ::lang/current :client ::lang/me :server)
            '[A (e/server B) C])
       r/find-nodes (mapv signify))))

#?(:clj
   (tests "l/def marks the namespace"
     (l/def Foo 1)
     (-> *ns* meta ::lang/has-edef?) := true))

#?(:clj
   (tests "cljs macroexpansion regression"
     (binding [expand/*electric* true]
       (-> (expand/all {::lang/peers {:server :clj, :client :cljs}, ::lang/current :client, ::lang/me :server, :ns 'hyperfiddle.electric-test}
             '(e/fn Foo []))
         first) := ::lang/closure)))

(tests "set literal"
  (def !v (atom 1))
  (with ((l/local (tap #{(e/watch !v)})) tap tap)
    % := #{1}
    (swap! !v inc)
    % := #{2}))

(tests "calling an electric defn in a clojure defn as a clojure defn"
  (l/defn ElectricFn [] 1)
  (defn clj-fn2 [] (inc (ElectricFn)))
  (try (clj-fn2) (throw (ex-info "unreachable" {}))
       (catch ExceptionInfo e (ex-message e) := "I'm an electric value and you called me outside of electric.")))

(tests "let over e/def"
  (let [x 1] (l/def XX [x x]))
  (with ((l/local (tap XX)) tap tap)
    % := [1 1]))

#?(:clj
   (tests "::lang/only filters e/def compilation"
     (l/def ^{::lang/only #{:server}} ServerOnly 1)
     (some? (find-var `ServerOnly_hf_server_server)) := true
     (some? (find-var `ServerOnly_hf_client_server)) := true
     (not (find-var `ServerOnly_hf_server_client)) := true
     (not (find-var `ServerOnly_hf_client_client)) := true))

(deftype FieldAccess [x])
(tests "non-static first arg to . or .. works"
  (with ((l/local (tap (.. (FieldAccess. 1) -x))) tap tap)
    % := 1))

(tests "lexical first arg to . or .. works"
  (with ((l/local (let [fa (FieldAccess. 1)] (tap (.. fa -x)))) tap tap)
    % := 1))

(tests "()"
  (with ((l/local+ {} (tap ())) tap tap)
    % := ()))

(tests "(#())"
  (with ((l/local+ {} (tap (#()))) tap tap)
    % := ()))

(tests "((fn []))"
  (with ((l/local+ {} (tap ((fn [])))) tap tap)
    % := nil))

(tests "::lang/non-causal removes causality in `let`"
  (l/defn ^::lang/non-causal NonCausalLet [tap]
    (let [_ (tap 1)] (tap 2)))
  (with ((l/local (NonCausalLet. tap)) tap tap)
    ;; % := 1
    % := 2))

(tests "::lang/non-causal removes causality in `binding`"
  (l/def NonCausalEDef)
  (l/defn ^::lang/non-causal NonCausalBinding [tap]
    (binding [NonCausalEDef (tap 1)] (tap 2)))
  (with ((l/local (NonCausalBinding. tap)) tap tap)
    ;; % := 1
    % := 2))

(tests "binding in interop fn"
  (with ((l/local (tap ((fn [] (binding [*out* nil] 1))))) tap tap)
    % := 1))

(tests "output slot regression"
  (l/def xxx (e/server :x))
  (l/def yyy (e/server :y))
  (with ((l/local (e/client (try (tap xxx) (tap yyy) (catch Pending _)))) tap tap)
    % := :x
    % := :y))
