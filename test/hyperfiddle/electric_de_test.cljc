(ns hyperfiddle.electric-de-test
  (:require [hyperfiddle.rcf :as rcf :refer [tap with % tests]]
            [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.electric-local-def-de :as l]
            [hyperfiddle.electric.impl.io :as electric-io]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [contrib.cljs-target :refer [do-browser]]
            #?(:cljs [hyperfiddle.goog-calls-test-de])
            #?(:cljs [hyperfiddle.js-calls-test-de])
            [clojure.string :as str]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.electric-de-test :refer [skip failing]]))
  (:import [hyperfiddle.electric Pending Failure]
           [missionary Cancelled]
           #?(:clj [clojure.lang ExceptionInfo])))

(defmacro skip {:style/indent 0} [& _body] `(print "-"))

(defmacro failing {:style/indent 0} [& body] nil)

(tests "call on local electric ctor"
  (with ((l/single {} (let [x (e/fn [] 1)] (tap ($ x)))) tap tap)
    % := 1))

(defrecord Point [x y])
(tests "new on class"
  (with ((l/single {} (tap (new Point 1 2))) tap tap)
    % := (Point. 1 2)))

;; TODO `m/ap` has `try` in expansion
(tests "new on missionary flow"
  (with ((l/single {} (tap (e/input (m/ap 1)))) tap tap)
    % := 1))

(tests "join missionary flow"
  (def flow (m/ap 1))
  (with ((l/single {} (tap (e/input flow))) tap tap)
    % := 1))

(tests "if"
  (with ((l/single {} (tap (if true :yes :no))) tap tap)
    % := :yes))

(tests "case"
  (with ((l/single {} (tap (case 1  1 1, 2 2))) tap tap)
    % := 1))

(tests "case"
  (with ((l/single {} (tap (case 1  1 1, 2 2, #_else nil))) tap tap)
    % := 1))

(tests "quote"
  (with ((l/single {} (tap 'foo)) tap tap)
    % := 'foo))

#?(:cljs
   (tests "js*"
     (with ((l/single {} (tap (js* "~{}+1" 1))) tap tap)
       % := 2)))

(tests "clj fn"
  (with ((l/single {} (let [x 1] (tap (#(inc x))))) tap tap)
    % := 2))

#?(:clj
   (tests "."
     (with ((l/single {} (tap (. java.time.Instant EPOCH))) tap tap)
       % := java.time.Instant/EPOCH)))

(tests "loop/recur"
  (with ((l/single {} (tap (loop [x 1] (if (odd? x) (recur (dec x)) x)))) tap tap)
    % := 0))

(tests "def"
  (with ((l/single {} (def DEFD 1)) tap tap))
  DEFD := 1)

;;; MAIN ELECTRIC TEST SUITE

(tests "hello world"
       (with ((l/single {} (tap "hello world")) tap tap)
         % := "hello world"))

(tests "literals are lifted"
  (with ((l/single {} (tap 1)) tap tap)
    % := 1))

(tests "data literals"
  (with ((l/single {} (tap {:a 1})) tap tap)
    % := {:a 1}))

(tests "globals lifted"
  (def a 1)
  (with ((l/single {} (tap a)) tap tap)
    % := 1))

(tests
  (with ((l/single {} (tap inc)) tap tap)
    % := inc))

(tests "clojure call"
  (with ((l/single {} (tap (inc (inc 1)))) tap tap)
    % := 3))

(tests "introduce foreign atom"
  (def !x (atom 0))
  (with ((l/single {} (tap (e/watch !x))) tap tap)                           ; clojure flow derived from atom
    % := 0
    (swap! !x inc)
    % := 1))

(tests "introduce foreign missionary signal"
  (def !x (atom 0))                                         ; atoms model variable inputs
  (with ((l/single {} (tap (e/input (m/watch !x)))) tap tap)                      ; clojure flow derived from atom
    % := 0
    (swap! !x inc)
    % := 1))

(tests "reactive closures - call them with $"
  (with ((l/single {} (tap (let [x 1, F (e/fn [] x)] [(number? x) ($ F)]))) tap tap)
    % := [true 1]))

(tests "dataflow diamond - let introduces shared nodes in the dag"
  (def !x (atom 0))
  (with ((l/single {} (tap (let [x (e/watch !x)] (+ x x)))) tap tap)
    % := 0
    (swap! !x inc)
    % := 2
    (swap! !x inc)
    % := 4))

(tests "broken dataflow diamond (two propagation frames - bad)"
  (def !x (atom 0))
  (with ((l/single {} (tap (let [X (m/watch !x)] ; recipe for flow
                             (+ (e/input X) (e/input X))))) tap tap)                        ; bad - construct flow twice
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
  (with ((l/single {} (tap ((e/watch !f) 0 (e/watch !x2)))) tap tap)
    % := 1
    (swap! !x2 inc)
    % := 2
    (reset! !f -)
    % := -2))

(tests "foreign clojure collections. clojure.core/map is not incremental, the arguments are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (with
    ((l/single {} (tap (let [f (e/watch !f)
                             xs (e/watch !xs)]
                         (clojure.core/map f xs)))) tap tap)
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]
    (reset! !f dec)
    % := [0 1 2 3]))

(tests "common core macros just work"
  (with
    ((l/single {} (tap (let [f (e/watch (atom inc))
                             xs (e/watch (atom [1 2 3]))]
                         (->> xs (map f))))) tap tap)
    % := [2 3 4]))

(tests "reactive if"
  (def !a (atom 1))
  (def !p (atom :p))
  (def !q (atom :q))
  (with ((l/single {} (tap (if (odd? (e/watch !a)) (e/watch !p) (e/watch !q)))) tap tap)
    % := :p
    (swap! !a inc)
    % := :q
    (reset! !p :pp)
    (swap! !a inc)
    % := :pp))

(tests "lazy"
  (with ((l/single {} (tap (if false (tap :a) (tap :b)))) tap tap)
    % := :b
    % := :b))

(tests "reactive fn"
  (with ((l/single {} (tap ($ (e/fn [x] (inc x)) 1))) tap tap)
    % := 2))

(e/defn My-inc [x] (inc x))
(tests "reactive defn"
  (with ((l/single {} (tap ($ My-inc 1))) tap tap)
    % := 2))

(e/defn If2 [x a b]                                       ; Key question - how lazy are the parameters?
  (->> (boolean x)
    (get {true (e/fn [] a)
          false (e/fn [] b)})
    ($)))

(def !branch (atom false))
(tests "control flow implemented with lazy signals"
  (with ((l/single {} (let [x (e/watch !branch)
                            a (tap :a) ; lazy
                            b (tap :b)] ; lazy
                        (tap ($ If2 x a b)))) tap tap)
    % := :b
    % := :b
    (swap! !branch not)
    % := :a
    % := :a
    (swap! !branch not)
    % := :b))

(tests "lazy let"
  (def !x (atom false))
  (def !a (atom :a))
  (def !b (atom :b))
  (with ((l/single {} (let [x (e/watch !x)
                            a (tap (e/watch !a))
                            b (tap (e/watch !b))]
                        (if x a b))) tap tap)
    % := :b
    (swap! !x not)
    % := :a))

(tests "reactive case"
  (def !a (atom 0))
  (def !p (atom :p))
  (def !q (atom :q))
  (with ((l/single {} (tap (case (e/watch !a)
                         0 (e/watch !p)
                         (e/watch !q)))) tap tap)
    % := :p
    (swap! !a inc)
    % := :q
    (reset! !q :qq)
    % := :qq))

(tests "symbols in electric"
  (with ((l/single {} (tap 'x)) tap tap)
    % := 'x))

(tests "symbols in electric"
  (with ((l/single {} (tap '[x])) tap tap)
    % := '[x]))

(tests "case on symbols"
  (def !x (atom 'foo))
  (with ((l/single {} (tap (case (e/watch !x) foo 1 2))) tap tap)
    % := 1))

(tests "case on vector"
  (with ((l/single {} (tap (case '[a b] [a b] 1 2))) tap tap)
    % := 1))

(tests "case with list"
  (def !x (atom 'foo))
  (with ((l/single {} (tap (case 'a (a b) 1 2))) tap tap)
    % := 1))

;; TODO `try` and `case` default
(skip "case with no matching clause"
  (with ((l/single {} (try (case 2 1 1)
                       (catch #?(:clj IllegalArgumentException :cljs js/Error) e (tap [:right (ex-message e)]))
                       (catch #?(:clj Throwable :cljs :default) e (tap [:wrong e])))) tap tap))
  % := [:right "No matching clause: 2"])

(tests "binding"
  (def foo 1)
  (with ((l/single {} (tap (binding [foo 2] foo))) tap tap)
    % := 2))

(tests "binding - fn"
  (def foo)
  (with ((l/single {} (binding [foo (partial tap)] (foo 1))) tap tap)
    % := 1))

(tests "binding - e/fn"
  (def foo)
  (with ((l/single {} (binding [foo (e/fn [x] (tap x))] ($ foo 1))) tap tap)
    % := 1))

(tests "lexical closure"
  (with ((l/single {} (tap ($ (let [a 1] (e/fn [] a))))) tap tap)
    % := 1))

(tests "join captures dynamic scope"
  (def foo 1)
  (with ((l/single {} (let [Q (e/fn [] foo)]
                    (binding [foo 2]
                      (tap ($ Q))))) tap tap)
    % := 2))

(tests "if with bindings"
  (def !a (atom true))
  (def foo 1)
  (with ((l/single {} (tap (binding [foo 2] (if (e/watch !a) foo (- foo))))) tap tap)
    % := 2
    (swap! !a not)
    % := -2))

(def foo4 1)
(tests "if with unwinding binding"
  (def !a (atom true))
  (with ((l/single {} (tap ($ (binding [foo4 2] (e/fn [] (if (e/watch !a) foo4 (- foo4))))))) tap tap)
    % := 1
    (swap! !a not)
    % := -1))

(def foo 1)
(def bar 2)

(tests "reactive for"
  (def !xs (atom [1 2 3]))
  (with ((l/single {} (tap (e/for-by identity [x (e/watch !xs)] (inc x)))) tap tap)
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]))

(tests "reactive for is differential (diff/patch)"
       (def !xs (atom [1 2 3]))
       (with ((l/single {} (tap (e/for-by identity [x (e/watch !xs)] (tap x)))) tap tap)
         (hash-set % % %) := #{1 2 3}   ; concurrent, order undefined
         % := [1 2 3]
         (swap! !xs conj 4)
         % := 4
         % := [1 2 3 4]
         (swap! !xs pop)
         % := [1 2 3] ;; TODO glitch here
         (swap! !xs assoc 1 :b)
         % := :b
         % := [1 :b 3]))

(def foo 0)
(tests "Reactive for with bindings"
  (def !items (atom ["a"]))
  (with ((l/single {} (binding [foo 1]
                    (e/for-by identity [item (e/watch !items)]
                      (tap foo)
                      item))) tap tap)

    % := 1
    (swap! !items conj "b")
    % := 1)) ; If 0 -> foo’s binding vanished

(tests "reactive for with keyfn"
  (def !xs (atom [{:id 1 :name "alice"} {:id 2 :name "bob"}]))
  (with ((l/single {} (tap (e/for-by :id [x (e/watch !xs)] (tap x)))) tap tap)
    (hash-set % %) := #{{:id 1 :name "alice"} {:id 2 :name "bob"}}
    % := [{:id 1 :name "alice"} {:id 2 :name "bob"}]
    (swap! !xs assoc-in [0 :name] "ALICE")
    % := {:id 1 :name "ALICE"}
    % := [{:id 1 :name "ALICE"} {:id 2 :name "bob"}]))

(tests "reactive do"
  (def !x (atom 0))
  (with ((l/single {} (tap (do (tap :a) (tap (e/watch !x))))) tap tap)
    ; Currently, do is not monadic sequence.
    ; It's an incremental computation so only rerun what changed in our opinion
    (hash-set % %) := #{:a 0}
    % := 0
    (swap! !x inc)
    ; no :a
    % := 1
    % := 1))

(tests "do forces evaluation (introduces eagerness)"
  ; Current behavior - do stmts are sampled eagerly, as fast as possible
  (def !a (atom 0))
  (def !b (atom 0))
  (with ((l/single {} (tap @(doto !b (reset! (tap (e/watch !a)))))) tap tap)
    % := 0
    % := 0
    (swap! !a inc)
    ; the ref !b doesn't change, so we don't see 1 again
    % := 1))

(comment "entrypoint forces evaluation (introduces eagerness)" ; desired behavior, we think
  ; Alternative - do stmts are sampled (for effect) when result is sampled

  (def !a (atom 0))
  (def !b (atom 0))
  ((l/single {} (tap @(doto !b (reset! (tap (new (m/watch !a))))))) tap tap)
  % := 0
  % := 0
  (swap! !a inc)
  % := 1
  % := 1)

#?(:clj (defn slow-identity [x] (Thread/sleep 30) x))

;; TODO try
#?(:clj
   (skip
     (with ((l/single {} (try
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
     (with ((l/single {}
              #_(doto (element "input")
                  (set-attribute! "type" "text")
                  (set-attribute! "value" x))
              (tap (doto (MutableMap)            ; the doto is incrementalized
                     (PutMap "a" (swap! !z inc)) ; detect effect
                     (PutMap "b" (tap (e/watch !xx)))))) tap tap)
       % := 0, % := {"a" 1 "b" 0}
       (swap! !xx inc)
       % := 1))) ; mutable map is clojure.core/=, therefore skipped

(def trace!)
(e/defn Div [child] (trace! child) [:div child])
(e/defn Widget [x] ($ Div [($ Div x) ($ Div :a)]))

(tests "reactive defn"
                                        ; best example of this is hiccup incremental maintenance
    (def !x (atom 0))
    (with ((l/single {} (tap (binding [trace! tap] ($ Widget (e/watch !x))))) tap tap)
      % := 0
      % := :a
      % := [[:div 0] [:div :a]]
      % := [:div [[:div 0] [:div :a]]]
      (swap! !x inc)
      % := 1
                                        ; no :a
      % := [[:div 1] [:div :a]]
      % := [:div [[:div 1] [:div :a]]]))

(e/defn G [x] x)                                      ; reactive fn (DAG). Compiler marks dag with meta
(tests "node call vs fn call"
  (defn f [x] x)                                            ; This var is not marked with meta
  (def !x (atom 0))
  (with ((l/single {} (tap (let [x (e/watch !x)] [(f x) ($ G x)]))) tap tap)
    % := [0 0]))

(e/defn G [x] x)
(tests "higher order dags"
  (def !x (atom 0))
  (defn f [x] x)
  (with
    ((l/single {}
       (tap (let [ff (fn [x] x) ; foreign clojure fns are sometimes useful, e.g. DOM callbacks
                  Gg (e/fn [x] x) ; but you almost always want reactive lambda, not cc/fn
                  x (e/watch !x)]
              [(f x)       ; var marked
               ($ G x)      ; var says node
               (ff x)      ; Must assume interop, for compat with clojure macros
               ($ Gg x)     ; Must mark reactive-call
               ($ (e/fn [x] x) x)]))) tap tap)
    % := [0 0 0 0 0]))

(tests "reactive closures"
  (def !x (atom 1))
  (def !y (atom 10))
  (with ((l/single {}
           (let [x (e/watch !x), y (e/watch !y)]
             (tap ($ (if (odd? x)
                       (e/fn [x] (* y x))
                       (e/fn [x] (* y x)))
                    x)))) tap tap)
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
          ((l/single {} (tap (let [x (e/watch !x)
                                   y (e/watch !y)
                                   F (e/fn [x] (+ y x)) ; constant signal
                                   G (if (odd? x) (e/fn [x] (+ y x))
                                         (e/fn [x] (+ y x)))
                                   H (e/input (m/seed [(e/fn [x] (+ y x))]))]
                               [($ F x)
                                ($ G x)
                                ($ H x)]))) tap tap)
          % := [0 0 0]))

(tests "reactive clojure.core/fn"
  (def !x (atom 0))
  (def !y (atom 0))
  (with
    ((l/single {}
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

;; TODO throw
(skip "Reactor crashes on uncaugh exceptions"
  (def !x (atom true))
  (with ((l/single {} (tap (assert (e/watch !x)))) tap tap)
    % := nil                            ; assert returns nil or throws
    (swap! !x not)                      ; will crash the reactor
    ;; TODO in old tests an ex-info comes out, why? Is this a FailureInfo?
    (ex-message %) := "Assert failed: (e/watch !x)"
    (swap! !x not)                      ; reactor will not come back.
    (tap ::nope), % := ::nope))

;; TODO try/catch/throw
;; (l/defn Boom [] (assert false))
(skip "reactive exceptions"
  (with ((l/single {} (tap (try
                         (Boom.)
                         (catch #?(:clj AssertionError, :cljs js/Error) e
                           e)))) tap tap)
    #?(:clj  (instance? AssertionError %)
       :cljs (instance? js/Error %)) := true))

;; TODO try/catch/throw
(skip
  (with ((l/single {} (tap (try (let [Nf (try (e/fn [] (Boom.)) ; reactive exception uncaught
                                          (catch #?(:clj AssertionError, :cljs :default) _ ::inner))]
                              (Nf.))
                            (catch #?(:clj AssertionError, :cljs :default) _ ::outer)))) tap tap)
    % := ::outer))

(def inner)
(e/defn Outer [] inner)

(tests "dynamic scope (note that try/catch has the same structure)"
  (with ((l/single {} (tap (binding [inner ::inner] ($ Outer)))) tap tap)
    % := ::inner))

(tests "dynamic scope (note that try/catch has the same structure)"
  (with ((l/single {} (tap (binding [inner ::outer]
                         (let [Nf (binding [inner ::inner]
                                    (e/fn [] ($ Outer)))] ; binding out of scope
                           ($ Nf))))) tap tap)
    % := ::outer))

(tests "lazy parameters. Flows are not run unless sampled"
  (with ((l/single {} [($ (e/fn [_]) (tap :not)) (tap :boom)]) tap tap)
    % := :boom))

(tests "lazy parameters. Flows are not run unless sampled"
       (with ((l/single {} (let [_ (tap :not)] (tap :bang))) tap tap)
    % := :bang))

;; TODO network
(skip "client/server transfer"
                                        ; Pending state is an error state.
                                        ; Pending errors will crash the reactor if not caugh
  (with ((l/single {} (try (tap (e/server (e/client 1))) (catch Pending _))) tap tap)
    % := 1))

(tests "client/server transfer"
       (with ((l/local {} (tap (e/server 1))) tap tap)
         % := 1))

;; TODO network
(def foo nil)
(skip
  (with ((l/single {} (try (tap (binding [foo 1] (e/server (e/client foo))))
                           (catch Pending _))) tap tap)
    % := 1))

(tests
  (with ((l/single {} (tap (binding [foo 1] (e/server (e/client foo))))) tap tap)
    % := 1))

;; TODO network
(def foo nil)
(skip
  (with ((l/single {} (try (tap (binding [foo 1] (e/server (new (e/fn [] (e/client foo))))))
                       (catch Pending _))) tap tap)
    % := 1))

(tests
  (with ((l/local {} (tap (binding [foo 1] (e/server ($ (e/fn [] (e/client foo))))))) tap tap)
    % := 1))

;; TODO try/catch
(def foo1 nil)
(def Bar1)
(skip
  (with ((l/single {} (try (tap (binding [foo1 1] (e/server (Bar1.))))
                       (catch Pending _))) tap tap)
    % := 1))

(tests
  (with ((l/local {} (tap (binding [Bar1 (e/fn [] (e/client foo1)), foo1 1] (e/server ($ Bar1))))) tap tap)
    % := 1))

;; TODO try/catch
(skip "reactive pending states"
  ;~(m/reductions {} hyperfiddle.electric.impl.runtime/pending m/none)
  (with ((l/single {} (tap (try true (catch Pending _ ::pending)))) tap tap)
    % := true))

;; TODO try/catch
(skip
  (with ((l/single {} (tap (try (e/server 1) (catch Pending _ ::pending)))) tap tap)
    % := ::pending    ; Use try/catch to intercept special pending state
    % := 1))

;; TODO try/catch
(skip
  (with ((l/single {} (tap (try [(tap 1) (tap (e/server 2))] (catch Pending _ ::pending)))) tap tap)
    % := 1
    % := ::pending
                                        ; do not see 1 again
    % := 2
    % := [1 2]))

;; TODO try/catch
(skip "the same exception is thrown from two places!"
  (l/defn InputController1 [tap controlled-value]
    (try controlled-value (catch Pending _ (tap :pending-inner))))

  (with ((l/single {} (try
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
        ((l/single {} (tap
                    (let [x (e/watch !x)]
                      (when (even? x)
                        ($ (e/fn [x]
                               (e/input (hook (partial tap 'mount) (partial tap 'unmount)))
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
  (with ((l/single {} (e/for-by identity [x (e/watch !state)] (e/input (m/observe (observer x))))) tap tap)
    % := [:up 1]
    (swap! !state conj 2)
    % := [:up 2]
    (reset! !state [3])
    (hash-set % % %) := #{[:up 3] [:down 1] [:down 2]})
  % := [:down 3])

;; TODO try/catch
(skip "object lifecycle 3 with pending state"
  (def !state (atom [1]))

  (defn observer [tap x]
    (fn mount [f]
      (tap [::mount x])
      (f nil)
      (fn unmount [] (tap [::unmount x]))))

  (let [dispose ((l/single {} (try
                            (e/for-by identity [x (e/watch !state)] ; pending state should not trash e/for branches
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

(def x2 1)
(tests "object lifecycle 4"
  (def !input (atom [1 2]))
  (defn up-down [x trace!] (m/observe (fn [!] (trace! :up) (! x) #(trace! :down))))

  (with ((l/single {}
           (tap (e/for-by identity [id (e/watch !input)]
                  (binding [x2 (do id x2)]
                    (e/input (up-down x2 tap)))))) tap tap)
    [% %] := [:up :up]
    % := [1 1]
    (swap! !input pop)
    % := [1]
    % := :down)
  % := :down)

(tests "reactive metadata"
  (def !x (atom 0))
  (with ((l/single {} (tap (meta (let [x (with-meta [] {:foo (e/watch !x)})] x)))) tap tap)
    % := {:foo 0}
    (swap! !x inc)
    (tap ::hi) % := ::hi))

;; TODO shows Cannot invoke \"java.lang.Character.charValue()\" because \"x\" is null
(skip "undefined continuous flow, flow is not defined for the first 10ms"
  (let [flow (m/ap (m/? (m/sleep 10 :foo)))]
    (with ((l/single {} (tap ($ ($ (e/fn [] (let [a (e/input flow)] (e/fn [] a))))))) tap tap)
      (ex-message %) := "Undefined continuous flow.")))

;; TODO try/catch
(skip
  (def !x (atom 0))
  (with ((l/single {} (tap (try (-> (e/watch !x)
                              (doto (-> even? (when-not (throw (ex-info "odd" {})))))
                              (/ 2))
                            (catch #?(:clj Exception, :cljs :default) e (ex-message e))))) tap tap)
    % := 0
    (swap! !x inc)
    % := "odd"
    (swap! !x inc)
    % := 1))

;; TODO try/catch
(skip
  (def !x (atom 0))
  (def !f (atom "hello"))
  (def e (ex-info "error" {}))
  (with ((l/single {}
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

(def unbound1)
(def unbound2)
(tests
  (with ((l/single {} (tap ($ (e/fn [] (binding [unbound1 1 unbound2 2] (+ unbound1 unbound2)))))) tap tap)
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

;; TODO try/catch
(skip "In Electric, accessing an unbound var throws a userland exception"
  ;; An unbound var is either:
  ;; - an uninitialized p/def,
  ;; - an unsatisfied reactive fn parameter (reactive fn called with too few arguments).
  (def x)
  (with ((l/single {} x) prn tap)
    (ex-message %) := "Unbound electric var `hyperfiddle.electric-test/x`"))

#?(:clj
   (tests ; GG: IDE doc on hover support
    "Vars created with e/defn have the same metas as created with cc/defn"
    (e/defn Documented "doc" [a b c])
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
    ((l/single {}
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
  ;; the essence of the problem is:
  ;; 1. if/case switch/change the DAG (imagine a railroad switch between two train tracks)
  ;; 2. to have a conditional where the predicate and the consequent have a common dependency
  (def !x (atom 1))
  (with ((l/single {} (tap (let [p (e/watch !x)
                                 q (tap (str p))
                                 control (- p)]
                             (case control -1 p -2 q q)))) tap tap)
    % := 1                              ; cross
    (swap! !x inc)
    % := "2"                            ; q first touched
    % := "2"))

(tests "for with literal input"
  (with ((l/single {} (tap (e/for-by identity [x [1 2 3]] (tap x)))) tap tap)
    (hash-set % % %) := #{1 2 3}
    % := [1 2 3]))

(tests "for with literal input, nested"
  (def !x (atom 0))
  (with ((l/single {} (tap (when (even? (e/watch !x))
                         (e/for-by identity [x [1 2 3]]
                           (tap x))))) tap tap)
    (hash-set % % %) := #{1 2 3}
    % := [1 2 3]
    (swap! !x inc)
    % := nil))

(tests "nested closure"
  (def !x (atom 0))
  (with ((l/single {} (tap ($ (let [x (e/watch !x)]
                              (if (even? x)
                                (e/fn [] :even)
                                (e/fn [] :odd)))))) tap tap)
    % := :even
    (swap! !x inc)
    % := :odd))

;; TODO e/hook?
(skip "simultaneous add and remove in a for with a nested hook"
  (def !xs (atom [1]))
  (defn hook
    ([x] (tap [x]))
    ([x y] (tap [x y])))
  (with
    ((l/single {}
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

;; TODO try/catch
(skip
  (def !t (atom true))
  (with ((l/single {}
           (tap (try (let [t (e/watch !t)]
                       (when t t (e/server t)))
                     (catch Pending _ :pending)
                     #_(catch Cancelled _ :cancelled)))) tap tap)
    % := :pending
    % := true
    (swap! !t not)
    % := nil))

(def !t (atom true))
(tests
  (reset! !t true)
  (with ((l/single {} (tap (let [t (e/watch !t)] (when t t (e/server t))))) tap tap)
    % := true
    (swap! !t not)
    % := nil))

(def !state1 (atom true))
(tests
  (reset! !state1 true)
  (with ((l/single {} (when (e/watch !state1) (tap :touch))) tap tap)
    % := :touch
    (reset! !state1 true)
    (tap ::nope) % := ::nope))

(def !state2 (atom true))
(tests "e/for in a conditional"
  (reset! !state2 true)
  (with ((l/single {} (tap (if (e/watch !state2) 1 (e/for-by identity [_ []])))) tap tap)
    % := 1
    (swap! !state2 not)
    % := []
    (swap! !state2 not)
    % := 1)
  )


(comment          ; we are not sure if this test has value. It is not minimized.
  (tests "Hack for e/for in a conditional. Passes by accident" ; PASS
    (def !state (atom true))
    (with ((l/single {} (tap (if (e/watch !state) 1 (try (e/for [_ []]) (catch Throwable t (throw t)))))) tap tap)
      % := 1
      (swap! !state not)
      % := []
      (swap! !state not)
      % := 1)))

;; TODO transfer try/catch
(skip "Nested e/for with transfer"
  (def !state (atom [1]))
  (l/def state (e/watch !state))
  (with ((l/single {} (try (e/for [x (e/server state)]
                         (e/for [y (e/server state)]
                           (tap [x y])))
                       (catch Cancelled _)
                       (catch Pending _))) tap tap)
    % := [1 1]
    (reset! !state [3])
    % := [3 3]))

(def !state (atom [1]))
(def state)
(tests
  (reset! !state [1])
  "Nested e/for with transfer"
  (with ((l/local {} (binding [state (e/watch !state)]
                        (e/for-by identity [x (e/server state)]
                          (e/for-by identity [y (e/server state)]
                            (tap [x y]))))) tap tap)
    % := [1 1]
    (reset! !state [3])
    % := [3 3]))

(tests
  "Static call"
  (with ((l/single {} (tap (Math/abs -1))) tap tap)
    % := 1))

#?(:clj
   (tests "Dot syntax works (clj only)"
     (with ((l/single {} (tap (. Math abs -1))) tap tap)
       % := 1)))

(tests "Sequential destructuring"
  (with ((l/single {} (tap (let [[x y & zs :as coll] [:a :b :c :d]] [x y zs coll]))) tap tap)
    % := [:a :b '(:c :d) [:a :b :c :d]]))

(tests "Associative destructuring"
       (with ((l/single {} (tap (let [{:keys [a ns/b d]
                                       :as m
                                       :or {d 4}}
                                      {:a 1, :ns/b 2 :c 3}] [a b d m]))) tap tap)
         % := [1 2 4 {:a 1, :ns/b 2, :c 3}]))

(tests "Associative destructuring with various keys"
  (with ((l/single {} (tap (let [{:keys    [a]
                              :ns/keys [b]
                              :syms    [c]
                              :ns/syms [d]
                              :strs    [e]}
                             {:a 1, :ns/b 2, 'c 3, 'ns/d 4, "e" 5}]
                         [a b c d e]))) tap tap)
    % := [1 2 3 4 5]))

;; TODO transfer try/catch
(skip "fn destructuring"
  (with ((l/single {}
           (try
             (tap (e/client ((fn [{:keys [a] ::keys [b]}] [::client a b]) {:a 1 ::b 2})))
             (tap (e/server ((fn [{:keys [a] ::keys [b]}] [::server a b]) {:a 1 ::b 2})))
             (catch Pending _))) tap tap))
    % := [::client 1 2]
    % := [::server 1 2])

(tests "fn destructuring"
  (with ((l/local {}
           (tap (e/client ((fn [{:keys [a] ::keys [b]}] [::client a b]) {:a 1 ::b 2})))
           (tap (e/server ((fn [{:keys [a] ::keys [b]}] [::server a b]) {:a 1 ::b 2})))) tap tap)
    % := [::client 1 2]
    % := [::server 1 2]))

;; TODO try/catch
(skip
  (def !xs (atom [false]))
  (with
    ((l/single {}
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
;; #?(:cljs (t/use-fixtures :each {:after #(t/async done (js/setTimeout done 1))}))

;; TODO transfer try/catch
(skip
  (def !x (atom true))
  (with ((l/single {}
           (try
             (let [x (e/watch !x)]
                                        ; check eager network does not beat the switch
               (tap (if x (e/server [:server x]) [:client x])))
             (catch Pending _))) tap tap)
    % := [:server true]
    (swap! !x not)
                                        ; the remote tap on the switch has been removed
    % := [:client false]))

(def !x (atom true))
(tests
  (reset! !x true)
  (with ((l/local {} (let [x (e/watch !x)]
                        (tap (if x (e/server [:server x]) [:client x])))) tap tap)
    % := [:server true]
    (swap! !x not)
                                        ; the remote tap on the switch has been removed
    % := [:client false]))

;; TODO transfer try/catch
(skip
  (def !x (atom true))
  (l/def x (e/server (e/watch !x)))
  (with ((l/single {}
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

(def !x (atom true))
(tests
  (reset! !x true)
  (with ((l/local {}
           (let [x (e/watch !x)]
             (if (e/server x)
               (e/server (tap x))
               (e/server x)))) tap tap)
   % := true
   (swap! !x not)
   % := ::rcf/timeout))

;; TODO transfer try/catch
;; https://www.notion.so/hyperfiddle/distribution-glitch-stale-local-cache-of-remote-value-should-be-invalidated-pending-47f5e425d6cf43fd9a37981c9d80d2af
(skip "glitch - stale local cache of remote value should be invalidated/pending"
  (def !x (atom 0))
  (def dispose ((l/single {} (tap (try (let [x (new (m/watch !x))]
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
  ((l/single {}
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

;; TODO transfer try/catch
(skip
  (with ((l/single {} (try (e/server
                         (let [foo 1]
                           (tap foo)
                           (tap (e/client foo))))
                       (catch Pending _))) tap tap)
    % := 1
    % := 1))

(tests
  (with ((l/local {} (e/server
                        (let [foo 1]
                          (tap foo)
                          (tap (e/client foo))))) tap tap)
    % := 1
    % := 1))

;; TODO transfer try/catch
(skip "Today, bindings fail to transfer, resulting in unbound var exception. This will be fixed"
                                        ; https://www.notion.so/hyperfiddle/photon-binding-transfer-unification-of-client-server-binding-7e56d9329d224433a1ee3057e96541d1
  (l/def foo)
  (with ((l/single {} (try
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
  (with ((l/single {} (tap (Math/max 2 1))) tap tap)
    % := 2))

;; TODO transfer try/catch
(skip "static method call in e/server"
  (with ((l/single {} (try (tap (e/server (Math/max 2 1)))
                       (catch Pending _))) tap tap)
    % := 2))

(tests "static method call in e/server"
  (with ((l/local {} (tap (e/server (Math/max 2 1)))) tap tap)
    % := 2))

;; TODO transfer try/catch
(skip "static method call in e/client"
  (with ((l/single {} (try (tap (e/server (subvec (vec (range 10))
                                        (Math/min 1 1)
                                        (Math/min 3 3))))
                       (catch Pending _))) tap tap)
    % := [1 2]))

(tests "static method call in e/client"
  (with ((l/local {} (tap (e/server (subvec (vec (range 10))
                                       (Math/min 1 1)
                                       (Math/min 3 3))))) tap tap)
    % := [1 2]))

(def !state3 (atom 0))
(def global)
(tests "Inline cc/fn support"
  (reset! !state3 0)
  (with ((l/single {} (let [state (e/watch !state3)
                            local [:local state]
                            f     (binding [global [:global state]]
                                    (fn ([a] [a local hyperfiddle.electric-de-test/global])
                                      ([a b] [a b local global])
                                      ([a b & cs] [a b cs local global])))]
                        (tap (f state))
                        (tap (f state :b))
                        (tap (f state :b :c :d)))) tap tap)
    (hash-set % % %) :=
    #{[0 [:local 0] [:global 0]]
      [0 :b [:local 0] [:global 0]]
      [0 :b '(:c :d) [:local 0] [:global 0]]}
    (swap! !state3 inc)
    (hash-set % % %) :=
    #{[1 [:local 1] [:global 1]]
      [1 :b [:local 1] [:global 1]]
      [1 :b '(:c :d) [:local 1] [:global 1]]}))

(def !state4 (atom 0))
(tests
  (reset! !state4 0)
  (with ((l/single {}
           (let [state (e/watch !state4)]
             (tap [state state])
             (tap [state state])))
         tap tap)
    % := [0 0]
    % := [0 0]
    (swap! !state4 inc)
    % := [1 1]
    % := [1 1]))

(tests "cc/fn lexical bindings are untouched"
  (with ((l/single {} (let [a 1
                            b 2
                            f (fn [a] (let [b 3] [a b]))]
                        (tap (f 2)))) tap tap)
    % := [2 3]))

(tests "Inline cc/fn shorthand support"
  (with ((l/single {} (tap (#(inc %) 1))) tap tap)
    % := 2))

(tests "inline m/observe support"
  (with ((l/single {}
           (tap (e/input (m/observe (fn [!]
                                      (tap :up)
                                      (! :observe)
                                      #(tap :down)))))) tap tap)
    % := :up
    % := :observe)
  % := :down
  (instance? Cancelled %) := true)

(tests "inline m/observe support"
  (let [!state (atom 0)]
    (with ((l/single {} (let [state     (e/watch !state)
                              lifecycle (m/observe (fn [push]
                                                     (tap :up)
                                                     (push state)
                                                     #(tap :down)))
                              val       (e/input lifecycle)]
                          (tap val))) tap tap)
      % := :up
      % := 0
      (swap! !state inc)
      % := :up
      % := 1
      % := :down)
    % := :down
    (instance? Cancelled %) := true))


(tests "Inline letfn support"
      (with ((l/single {} (tap (letfn [(descent  [x] (cond (pos? x) (dec x)
                                                           (neg? x) (inc x)
                                                           :else    x))
                                       (is-even? [x] (if (zero? x) true  (is-odd?  (descent x))))
                                       (is-odd?  [x] (if (zero? x) false (is-even? (descent x))))]
                                 (tap [(is-even? 0) (is-even? 1) (is-even? 2) (is-even? -2)])
                                 (tap [(is-odd?  0) (is-odd?  2) (is-odd?  3) (is-odd? -3)])))) tap tap)
        (hash-set % %) :=
        #{[true false true true]
          [false false true true]}
        % := [false false true true]))

(def !state (atom 0))
(def global)
(tests "Inline letfn support"
  (reset! !state 0)
  (with ((l/single {} (let [state (e/watch !state)
                        local [:local state]]
                    (binding [global [:global state]]
                      (letfn [(f ([a] [a local hyperfiddle.electric-de-test/global])
                                ([a b] [a b local global])
                                ([a b & cs] [a b cs local global]))]
                        (tap (f state))
                        (tap (f state :b))
                        (tap (f state :b :c :d)))))) tap tap)
    (hash-set % % %) :=
    #{[0 [:local 0] [:global 0]]
      [0 :b [:local 0] [:global 0]]
      [0 :b '(:c :d) [:local 0] [:global 0]]}
    (swap! !state inc)
    (hash-set % % %) :=
    #{[1 [:local 1] [:global 1]]
      [1 :b [:local 1] [:global 1]]
      [1 :b '(:c :d) [:local 1] [:global 1]]}))

#?(:clj
   (tests "e/fn is undefined in clojure-land"
     (tap (try (eval '(l/single {} (fn [] (e/fn []))))
               (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric-de/fn) inside a Clojure function"))

#?(:clj
   (tests "e/client is undefined in clojure-land"
     (tap (try (eval '(l/single {} (fn [] (e/client [])))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric-de/client) inside a Clojure function"))

#?(:clj
   (tests "e/server is undefined in clojure-land"
     (tap (try (eval '(l/single {} (fn [] (e/server [])))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric-de/server) inside a Clojure function"))

#?(:clj
   (tests "e/watch is undefined in clojure-land"
     (tap (try (eval '(l/single {} (fn [] (e/watch (atom :nomatter))))) (catch Throwable e (ex-message (ex-cause e)))))
     % := "Electric code (hyperfiddle.electric-de/watch) inside a Clojure function"))

;; 0 can be skipped because tap and reset! are concurrent
(skip "cycle"
  (with ((l/single {}
           (let [!F (atom (e/fn [] 0))]
             (tap ($ (e/watch !F)))
             (let [y 1] (reset! !F (e/fn [] y))))) tap tap)
    % := 0
    % := 1))

#?(:clj ; test broken in cljs, not sure why
   (tests "loop/recur"
     (e/defn fib [n] (loop [n n] (if (<= n 2) 1 (+ (recur (dec n)) (recur (- n 2))))))
     (with ((l/single {} (tap (e/for-by identity [i (range 1 11)] ($ fib i)))) tap tap)
       % := [1 1 2 3 5 8 13 21 34 55])))

;; currently broken https://www.notion.so/hyperfiddle/cr-macro-internal-mutation-violates-photon-purity-requirement-119c18755ddd466384beb15f1e2317c5
#_
(comment
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
  (def z 3)
  (def !x (atom 4))
  (with ((l/single {} (let [y 2] (letfn [(f [x] (g x)) (g [x] [x y z])] (tap (f (e/watch !x)))))) tap tap)
    % := [4 2 3]
    (swap! !x inc)
    % := [5 2 3]))

;; currently broken https://www.notion.so/hyperfiddle/cr-macro-internal-mutation-violates-photon-purity-requirement-119c18755ddd466384beb15f1e2317c5
#_
(comment
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
     (with ((l/single {} (let [pt (java.awt.Point. 1 2)
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
       (with ((l/single {} (let [a (.createElement js/document "a")
                             href (e/watch !href)]
                         (set! (.-href a) href)
                         (tap [href a]))) tap tap)
         (bypass-rcf-bug %) := ["href1" "href1"]
         (reset! !href "href2")
         (bypass-rcf-bug %) := ["href2" "href2"]))))

#?(:clj (tests "set! with electric value"
          (with ((l/single {} (tap (let [pt (java.awt.Point. 1 2)]
                                 (set! (.-y pt) ($ (e/fn [] 0)))))) tap tap)
            % := 0)))

#?(:cljs (tests "set! with electric value"
           (with ((l/single {} (tap (let [o (js/Object.)]
                                  (set! (.-x o) ($ (e/fn [] 0)))))) tap tap)
             % := 0)))

(def a-root 1)
#?(:cljs
   (tests "set! to alter root binding"
     (with ((l/single {} (set! a-root 2)) tap tap))
     a-root := 2))

;; TODO e/fn arity check, try/catch
(skip "e/fn arity check"
  (with ((l/single {} (try (new (e/fn [x y z] (throw (ex-info "nope" {}))) 100 200 300 400)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called <unnamed-efn> with 4 arguments but it only supports 3"))

;; TODO e/fn arity check, try/catch
;; (l/defn ThreeThrow [_ _ _] (throw (ex-info "nope")))
(skip "e/fn arity check"
  (with ((l/single {} (try (new ThreeThrow 100 200 300 400)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called ThreeThrow with 4 arguments but it only supports 3"))

;; TODO e/fn arity check, try/catch
(skip "e/fn arity check"
  (with ((l/single {} (try (new (e/fn Named [x y] (throw (ex-info "nope" {}))) 100)
                       (catch ExceptionInfo e (tap e))
                       (catch Cancelled _)
                       (catch Throwable t (prn t)))) tap tap)
    (ex-message %) := "You called Named with 1 argument but it only supports 2"))

;; TODO e/partial
(skip "Partial application"
  (with ((l/single {}
           (tap (new (e/partial 0 (e/fn [] :a)) ))
           (tap (new (e/partial 1 (e/fn [a] a) :a)))
           (tap (new (e/partial 2 (e/fn [a b] [a b]) :a) :b))
           (tap (new (e/partial 4 (e/fn [a b c d] [a b c d]) :a :b) :c :d))) tap tap)
    % := :a
    % := :a
    % := [:a :b]
    % := [:a :b :c :d]))

(e/defn Factorial-gen [Rec]
  (e/fn [n]
    (if (zero? n)
      1
      (* n ($ Rec (dec n))))))

(e/defn Y [f]
  ($
    (e/fn [x] ($ x x))
    (e/fn [x] ($ f (e/fn [y] ($ ($ x x) y))))))

(tests "Y-Combinator"
  (let [!n (atom 5)]
    (with ((l/single {} (tap ($ ($ Y Factorial-gen) (e/watch !n)))) tap tap)
      % := 120
      (reset! !n 20)
      % := 2432902008176640000)))

(tests "clojure def inside electric code"
  (def !x (atom 0))
  (with ((l/single {} (def --foo (tap (e/watch !x)))) tap tap)
                    % := 0, --foo := 0
    (swap! !x inc)  % := 1, --foo := 1))

;; TODO try/catch
(skip "catch handlers are work skipped"
  (def !x (atom 0))
  (with ((l/single {} (try (e/watch !x)
                  (throw (ex-info "hy" {}))
                  (catch ExceptionInfo e (tap e))
                  (catch Cancelled _ (tap :cancelled)))) tap tap)
   (ex-message %) := "hy"      ; exception tapped by `ExceptionInfo` catch block
   (swap! !x inc))              ; same exception, so work skipped
  % := :cancelled)

;; TODO try/catch
(skip "pendings don't enter cc/fn's"
  (with ((l/single {} (try (let [v (new (m/observe (fn [!] (! r/pending) (def ! !) #(do))))]
                         (#(tap [:v %]) v))
                       (catch Pending _ (tap :pending))
                       (catch #?(:clj Throwable :cljs :default) e (prn [(type e) (ex-message e)])))) tap tap)
    % := :pending
    (! 1)
    % := [:v 1]))

;; TODO try/catch
(skip "catch code reacts to changes"
  (def !x (atom 0))
  (with ((l/single {} (tap (try (throw (ex-info "boom" {}))
                            (catch Throwable _ (e/watch !x))))) tap tap)
    % := 0
    (swap! !x inc)
    % := 1))

;; TODO try/catch, electric binding conveyance
(skip "Electric dynamic scope is available in cc/fn"
  (l/def ^:dynamic dynfoo 1)
  (with ((l/single {}
           (try
             ((fn []
                (tap dynfoo)))
             (binding [dynfoo 2]
               ((fn [] (tap dynfoo))))
             (catch #?(:clj Throwable, :cljs js/Error) t (prn t)))) tap tap)
    % := 1
    % := 2))

#?(:clj ; fail to compile in cljs: `Can't set! local var or non-mutable field` (foo177584 is not dynamic)
   (comment "l/def are not dynamic by default in cc/fn"
     (l/def foo177584 1)
     (with ((l/single {}
              (try
                ((fn [] (binding [foo177584 2] (tap foo177584)))) ; foo177584 is not ^:dynamic
                (catch #?(:clj Throwable, :cljs js/Error) t (tap (ex-message t))))) tap tap)
       % := "Can't dynamically bind non-dynamic var: hyperfiddle.electric-test/foo177584")))

;; TODO try/catch, electric binding conveyance
(skip "Injecting an l/def binding in cc/fn respects dynamic scope rules"
  (def ^:dynamic dynfoo 1)
  (with ((l/single {}
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
    (binding [#?(:clj foo, :cljs hyperfiddle.electric-de-test/foo) 3] ; always rebind var in clojure. Cljs requires fully qualified name.
      foo := 2 ; unqualified name resolves to lexical scope
      hyperfiddle.electric-de-test/foo := 3))) ; qualified name resolves to the var

;; TODO try/catch, electric binding conveyance
#?(:clj
   (skip "cc/fn args shadow l/def injections"
     (def ^:dynamic dynfoo 1)
     (with ((l/single {}
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

;; TODO try/catch, electric binding conveyance
#?(:clj
   (skip "Injected lexical scope respects precedence over injected dynamic scope"
     (def ^:dynamic dynfoo 1)
     (with ((l/single {}
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

;; TODO try/catch, electric binding conveyance
#?(:clj
   (skip "Shadowing injected dynamic scope in cc context respects clojure shadowing rules"
     (def ^:dynamic dynfoo 1)
     (with ((l/single {}
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

;; TODO e/snapshot - is this still a thing?
(skip "snapshot"
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

;; TODO e/for-event, is this still a thing?
(skip "for-event"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (with ((l/single {} (tap (try (e/for-event [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
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

;; TODO e/for-event-pending, is this still a thing?
(skip "for-event-pending"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/single {} (tap (e/for-event-pending [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
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

;; TODO e/for-event-pending-switch, is this still a thing?
(skip "for-event-pending-switch"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/single {} (tap (e/for-event-pending-switch [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
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

;; TODO e/do-event, is this still a thing?
(skip "do-event"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (with ((l/single {} (tap (try (e/do-event [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
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

;; TODO e/do-event-pending, is this still a thing?
(skip "do-event-pending"
  (def ! (atom nil))
  (def !resolvers (atom {}))
  (defn !! [k v] (reset! (@!resolvers k) v))
  (def fail (ex-info "i fail" {}))
  (with ((l/single {} (tap (e/do-event-pending [e (m/observe (fn [!!] (reset! ! !!) #(do)))]
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

;; TODO try/catch, e/offload, requires Pending
#?(:clj
   (skip "e/offload starts Pending"
     (def dfv (m/dfv))
     (with ((l/single {} (tap (try (e/offload #(m/? dfv))
                               (catch Pending ex ex)
                               (catch Throwable ex (prn ex))))) tap tap)
       % := e/pending
       (dfv 1)
       % := 1)))

;; TODO try/catch, e/offload, requires Pending
#?(:clj
   (skip "e/offload doesn't throw Pending subsequently"
     (def !dfv (atom (m/dfv)))
     (with ((l/single {} (tap (try (let [dfv (e/watch !dfv)]
                                 (e/offload #(m/? dfv)))
                               (catch Pending ex ex)
                               (catch Throwable ex (prn ex))))) tap tap)
       % := e/pending
       (@!dfv 1)
       % := 1
       (reset! !dfv (m/dfv))
       (@!dfv 2)
       % := 2)))

;; TODO try/catch, e/offload, requires Pending
#?(:clj
    (skip "e/offload on overlap uses latest value and discards previous"
      (def d1 (m/dfv))
      (def !dfv (atom d1))
      (with ((l/single {} (try (let [dfv (e/watch !dfv)]
                             (tap (e/offload #(m/? dfv))))
                           (catch Pending _)
                           (catch Throwable ex (prn [(type ex) (ex-message ex)])))) tap tap)

        (def d2 (reset! !dfv (m/dfv)))
        (d2 2)
        % := 2
        (d1 1))))

;; TODO try/catch, e/offload, requires Pending
#?(:clj
   (skip "e/offload thunk is running on another thread"
     (defn get-thread [] (Thread/currentThread))
     (with ((l/single {} (try (tap (e/offload get-thread))
                          (catch Pending _)
                          (catch Throwable ex (prn ex)))) tap tap)
       (count (hash-set % (get-thread))) := 2)))

#?(:cljs
   (do-browser
     (tests "goog module calls don't trigger warnings"
       ;; this includes a goog test namespace, so if there are warnings the CI will blow up.
       ;; The blow up is configured as a shadow build hook in `hyperfiddle.browser-test-setup`
       (with ((l/single {} (tap (case ($ hyperfiddle.goog-calls-test-de/Main) :ok))) tap tap)
         % := :ok))))

;; TODO try/catch
(skip
  (with ((l/single {} (tap (try ($ nil) (catch #?(:clj Throwable :cljs :default) e e)))) tap tap)
    (ex-message %) := "called `new` on nil"))

;; TODO try/catch
(skip
  (with ((l/single {} (tap (try (e/watch :foo) (throw (ex-info "nope" {}))
                            (catch ExceptionInfo e e)))) tap tap)
    (str/includes? (ex-message %) ":foo") := true))

(tests "e/fn varargs"
  (with ((l/single {} ($ (e/fn [x & xs] (tap [x xs])) 1 2 3 4)) tap tap)
    % := [1 [2 3 4]]))
;; TODO try/catch
(skip "e/fn varargs recur is arity-checked"
  (with ((l/single {} (tap (try (new (e/fn [x & xs] (recur)) 1 2 3)
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You `recur`d in <unnamed-efn> with 0 arguments but it has 2 positional arguments"))

(e/defn MapVararg [& {:keys [x] :or {x 1} :as mp}] [x mp])
(tests "map vararg with no args is nil"
  (with ((l/single {} (tap ($ MapVararg))) tap tap)
    % := [1 nil]))
(tests "map vararg with kw args"
  (with ((l/single {} (tap ($ MapVararg :x 2))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with map arg"
  (with ((l/single {} (tap ($ MapVararg {:x 2}))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with mixture"
  (with ((l/single {} (tap ($ MapVararg :y 3 {:x 2}))) tap tap)
    % := [2 {:x 2, :y 3}]))
(tests "map vararg trailing map takes precedence"
  (with ((l/single {} (tap ($ MapVararg :x 3 {:x 2}))) tap tap)
    % := [2 {:x 2}]))
(tests "map vararg with positional arguments"
  (with ((l/single {} (tap ($ (e/fn [a & {:keys [x]}] [a x]) 1 :x 2))) tap tap)
    % := [1 2]))

;; TODO try/catch
(skip "e/fn recur is arity checked"
  (with ((l/single {} (tap (try (new (e/fn X [x] (recur x x)) 1)
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You `recur`d in X with 2 arguments but it has 1 positional argument"))

(e/defn One [x] x)
(e/defn Two [x y] [x y])
(e/defn VarArgs [x & xs] [x xs])
(tests "($ One 1)"
  (with ((l/single {} (tap ($ One 1))) tap tap)
    % := 1))
(tests "($ VarArgs 1 2 3)"
  (with ((l/single {} (tap ($ VarArgs 1 2 3))) tap tap)
    % := [1 [2 3]]))
(skip "varargs arity is checked"
  (with ((l/single {} (tap (try (new VarArgs)
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You called VarArgs with 0 arguments but it only supports 1"))

(tests "e/apply"
  (with ((l/single {} (tap ($ e/Apply VarArgs [1 2 3]))) tap tap)
    % := [1 [2 3]]))
(tests "e/apply"
  (with ((l/single {} (tap ($ e/Apply Two 1 [2]))) tap tap)
    % := [1 2]))
(tests "e/apply"
  (with ((l/single {} (tap ($ e/Apply Two [1 2]))) tap tap)
    % := [1 2]))
(tests "e/apply"
  (with ((l/single {} (tap ($ e/Apply Two [1 (inc 1)]))) tap tap)
    % := [1 2]))
;; TODO try/catch
(skip "e/apply"
  (with ((l/single {} (tap (try ($ e/Apply Two [1 2 3]) (throw (ex-info "boo" {}))
                            (catch ExceptionInfo e e)))) tap tap)
    (ex-message %) := "You called Two with 3 arguments but it only supports 2"))

(tests "multi-arity e/fn"
  (with ((l/single {} (tap ($ (e/fn ([_] :one) ([_ _] :two)) 1))) tap tap)
    % := :one))
(tests "multi-arity e/fn"
  (with ((l/single {} (tap ($ (e/fn ([_] :one) ([_ _] :two)) 1 2))) tap tap)
    % := :two))
(tests "multi-arity e/fn"
  (with ((l/single {} (tap ($ (e/fn ([_]) ([_ & xs] (mapv inc xs))) 1 2 3 4))) tap tap)
    % := [3 4 5]))
(tests "multi-arity e/fn"
  (with ((l/single {} (tap ($ e/Apply (e/fn ([_] :one) ([_ _] :two)) 1 [2]))) tap tap)
    % := :two))
(tests "multi-arity e/fn"
  (with ((l/single {} (tap ($ e/Apply (e/fn ([_]) ([_ & xs] (mapv inc xs))) 1 2 [3 4]))) tap tap)
    % := [3 4 5]))

(tests "self-recur by name, e/fn"
  (with ((l/single {} (tap ($ (e/fn fib [n] (case n 0 0 1 1 (+ ($ fib (- n 1)) ($ fib (- n 2))))) 6))) tap tap)
    % := 8))
(tests "self-recur by recur, e/fn"
  (with ((l/single {} (tap ($ (e/fn fib [n] (case n 0 0 1 1 (+ (recur (- n 1)) (recur (- n 2))))) 6))) tap tap)
    % := 8))
(e/defn Fib [n] (case n 0 0 1 1 (+ ($ Fib (- n 1)) ($ Fib (- n 2)))))
(tests "self-recur by name, e/defn"
  (with ((l/single {} (tap ($ Fib 7))) tap tap)
    % := 13))
(tests "self-recur by name, e/fn thunk"
  (def !x (atom 2))
  (with ((l/single {} ($ (e/fn X [] (if (pos-int? (tap (swap! !x dec))) ($ X) (tap :done))))) tap tap)
    % := 1
    % := 0
    % := :done))
(tests "self-recur by name, to different arity"
  (with ((l/single {} (tap ($ (e/fn X ([] ($ X 0)) ([n] (inc n)))))) tap tap)
    % := 1))
(tests "self-recur by name, varargs"
  (with ((l/single {} ($ (e/fn Chomp [& xs] (if (tap (seq xs)) ($ Chomp) (tap :done))) 0 1 2)) tap tap)
    % := [0 1 2]
    % := nil
    % := :done))
(tests "self-recur by recur, varargs"
  (with ((l/single {} ($ (e/fn [& xs] (if (tap (seq xs)) (recur nil) (tap :done))) 0 1 2)) tap tap)
    % := [0 1 2]
    % := nil
    % := :done))

#?(:clj
   (tests "e/fn multi-arity mistakes"
     (try (lang/expand-all {} '(e/fn Named ([x] x) ([y] y)))
          (catch Throwable e (tap e)))
     (ex-message %) := "Conflicting arity definitions in Named: [x] and [y]"

     (try (lang/expand-all {} '(e/fn Named ([x] x) ([& ys] ys)))
          (catch Throwable e (tap e)))
     (ex-message %) := "Conflicting arity definitions in Named: [x] and [& ys]"

     (try (lang/expand-all {} '(e/fn ([x & ys] x) ([x y & zs] ys)))
          (catch Throwable e (tap e)))
     (ex-message %) := "Conflicting arity definitions: [x & ys] and [x y & zs]"))

#?(:cljs
   (tests "#js"
     (def !x (atom 0))
     (with ((l/single {} (let [x (e/watch !x)]
                           (tap [(.-x #js {:x x})
                                 (aget #js [:x x] 1)]))) tap tap)
       % := [0 0]
       (swap! !x inc)
       % := [1 1])))

#?(:clj
   (tests "jvm interop"
     (with ((l/single {}
              (let [f (java.io.File. "src")
                    pt (java.awt.Point. 1 2)]
                (tap [(.getName f)                            ; instance method
                      (.-x pt)                                ; field access
                      (java.awt.geom.Point2D/distance 0 0 1 0) ; static method
                      ]))) tap tap)
       % := ["src" 1 1.0])))

#?(:cljs
   (tests "js interop"
     (with ((l/single {}
              (let [^js o #js {:a 1 :aPlus (fn [n] (inc n))}]
                (tap [(.aPlus o 1) (.-a o)])))  tap tap)
       % := [2 1])))

;; TODO cljs
#?(:clj
   (skip "we capture invalid calls"
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
  (with ((l/single {} (tap (e/client 1 2))) tap tap)
    % := 2))

;; TODO
#?(:clj
   (skip "l/def marks the namespace"
     (e/defn Foo [] 1)
     (-> *ns* meta ::lang/has-edef?) := true))

;; TODO
#?(:clj
   (skip "cljs macroexpansion regression"
     (-> (lang/expand-all {::lang/peers {:server :clj, :client :cljs}, ::lang/current :client, ::lang/me :server, :ns 'hyperfiddle.electric-test}
           '(e/fn Foo []))
       first) := ::lang/ctor))

(tests "set literal"
  (def !v (atom 1))
  (with ((l/single {} (tap #{(e/watch !v)})) tap tap)
    % := #{1}
    (swap! !v inc)
    % := #{2}))

(let [x 1] (e/defn XX [] [x x]))
(tests "let over e/defn"
  (with ((l/single {} (tap ($ XX))) tap tap)
    % := [1 1]))

(deftype FieldAccess [x])
(tests "non-static first arg to . or .. works"
  (with ((l/single {} (tap (.. (FieldAccess. 1) -x))) tap tap)
    % := 1))

(tests "lexical first arg to . or .. works"
  (with ((l/single {} (let [fa (FieldAccess. 1)] (tap (.. fa -x)))) tap tap)
    % := 1))

(tests "()"
  (with ((l/single {} (tap ())) tap tap)
    % := ()))

(tests "(#())"
  (with ((l/single {} (tap (#()))) tap tap)
    % := ()))

(tests "((fn []))"
  (with ((l/single {} (tap ((fn [])))) tap tap)
    % := nil))

(tests "binding in interop fn"
  (with ((l/single {} (tap ((fn [] (binding [*out* nil] 1))))) tap tap)
    % := 1))

(tests "e/letfn"
       (with ((l/single {}
                (tap (e/letfn [(Odd? [x] (or (zero? x) ($ Even? (dec x))))
                               (Even? [x] (or (zero? x) ($ Odd? (dec x))))]
                       ($ Even? 2)))) tap tap)
         % := true))

(tests "e/letfn"
       (with ((l/single {}
                (tap (e/letfn [(Even? [x] (if (zero? x) true ($ Even? (dec x))))]
                       ($ Even? 2)))) tap tap)
         % := true))

(e/defn Self [] Self)
(tests
  (with ((l/single {}
           (let [Bar Self]
             (binding [Self (e/fn [] 111)]
               (tap (= Bar (e/$ Bar)))))) tap tap)
    % := false))

(tests
  (def !offset (atom 0))
  (with ((l/local {}
           (e/cursor [j (let [o (e/watch !offset)]
                          (e/diff-by identity
                            (range o (+ o 2))))]
             (e/server (tap j))))
         tap tap)
    (hash-set % %) := #{0 1}
    (swap! !offset inc)
    % := 2))

(comment
  (defn payT [_] (m/sp (m/? (m/sleep 10)) (rand-int 1000)))
  (defn task->incseq [T] (m/ap (m/amb (i/empty-diff 0) (assoc (i/empty-diff 1) :grow 1, :change {0 (m/? T)}))))

  (defn uf->is [uf]
    (m/ap (m/amb (i/empty-diff 0)
            (let [!first (atom true) v (m/?> uf)]
              (assoc (i/empty-diff 1) :grow (if @!first (do (swap! !first not) 1) 0), :change {0 v})))))

  (defn event->task [flow]
    (uf->is (m/ap
              (let [!busy? (atom false)
                    v (m/?> (m/eduction (remove (fn [_] @!busy?)) flow))
                    dfv (m/dfv), done! #(dfv false)]
                (m/amb
                  [v done! (reset! !busy? true)]
                  [v done! (reset! !busy? (m/? dfv))])))))


  (def !c (atom nil))
  (tests
    (with ((l/local {}
             (let [[v done!] (e/join (event->task (m/observe (fn [!] (reset! !c !) #()))))]
               (case (e/server (e/join (task->incseq (payT v))))
                 (tap (done!))))) tap tap)
      (@!c 1)
      % := false
      (@!c 2)
      % := false))
  )
