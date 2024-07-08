(ns hyperfiddle.electric-de-exceptions-test
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-local-def :as l]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.rcf :as rcf :refer [tests with % tap]]
            [missionary.core :as m])
  (:import [hyperfiddle.electric Pending Failure]
           [missionary Cancelled]
           #?(:clj [clojure.lang ExceptionInfo])))

;; * Behavior of exceptions
;; ** in v2
;; *** Implicit Either monad
;;     - An expression always return one value.
;;     - Group expressions (do, try, catch, etc.) return the value of their last
;;       (right-most) expression.
;;     - Thrown exceptions are special values:
;;       - thrown exceptions are wrapped in a Failure instance
;;         (e.g. Either Exception Value = Failure Exception | Value)
;;       - Failure instances propagate through the return channel
;;       - Failure instances short-circuits function application:
;;         #+begin_src clojure
;;           (do a <failure> b)  ; returns <failure>, not b, a and b are still sampled (still run)
;;           (prn a <failure> b) ; returns <failure>, `prn` call is bypassed
;;         #+end_src
;;       - The left-most Failure instance takes precedence:
;;         #+begin_src clojure
;;           (do a <failureA> <failureB> c) ; returns <failureA>
;;           (prn a <failureA> <failureB> c) ; returns <failureA>, `prn` call is bypassed
;;         #+end_src
;; *** Try/catch behavior
;;     #+begin_src clojure
;;       (try <try-body> (catch DispatchType ex <catch-body>) (catch ...) ... (finally ...))
;;     #+end_src
;;     - a try block returns either:
;;       - in absence of any Failure in try-body
;;         - the right-most value of `try-body`
;;       - in presence of a Failure in try-body
;;         - if one `catch` DispatchType matches - as per `instanceOf`
;;           - the left-most matching catch takes precedence, not the exception
;;             type hierarchy.
;;           - return the right-most value of corresponding catch-body
;;         #+begin_src clojure
;;           (try (throw (ex-info "message" {})) ; throws an ExceptionInfo
;;                (catch ExceptionInfo ex        ; this block matches
;;                  [:ex-info (ex-message ex)])  ; this value is returned
;;                (catch Throwable ex            ; unreachable code
;;                  [:throwable (ex-message ex)]))
;;         #+end_src
;;     - a finally block
;;       - runs for effects only (return value is discarded)
;;       - mounts at the same time as try's body
;;       #+begin_src clojure
;;         (try (prn :a) 1 (finally (prn :b) 2))
;;         ;; prints :a then :b
;;         ;; return 1
;;       #+end_src
;;


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

(tests "For reference, Clojure exceptions have dynamic scope"
  (try (let [f (try (fn [] (throw (ex-info "boom" {}))) ; this exception will escape
                    (catch #?(:clj Exception, :cljs :default) _ ::inner))]
                                        ; the lambda doesn't know it was constructed in a try/catch block
         (f))
       (catch #?(:clj Exception, :cljs :default) _ ::outer))
  := ::outer)

(tests "Reactor crashes on uncaugh exceptions"
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

(tests "All Pending instances are equal"
  (= (Pending.) (Pending.)) := true)

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

(tests "snapshot" ; captures first non-pending value
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
