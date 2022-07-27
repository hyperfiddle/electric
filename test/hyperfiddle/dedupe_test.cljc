(ns hyperfiddle.dedupe-test
  (:require  [missionary.core :as m]
             [hyperfiddle.rcf :as rcf :refer [tests ! %]]
             [hyperfiddle.photon :as p])
  (:import [hyperfiddle.photon Pending Failure])
  #?(:cljs (:require-macros hyperfiddle.dedupe-test)))

(defmacro deduping [& body] `(new (m/relieve {} (m/eduction (dedupe) (p/fn [] ~@body)))))

(tests
  (def !atom (atom 1))
  (p/def x (p/watch !atom))
  (def dispose (p/run (! (try (deduping (do (assert (odd? x)) x))
                              (catch #?(:clj Throwable, :cljs :default) _ ::boom)))))

  % := 1
  (swap! !atom dec)
  % := ::boom

  (swap! !atom inc)
  % := 1     ; the exception is no longer present therefore the flow must notify

  (dispose))

(tests
  "Pending is a special case of Throwable and impacts deduping the same way"
  (def !atom (atom 1))
  (def dispose (p/run (! (try (deduping (p/watch !atom))
                              (catch Pending _ ::pending)))))

  % := 1

  (reset! !atom (Failure. (Pending.)))
  % := ::pending

  (reset! !atom 1)
  % := 1                      ; no longer pending therefore the flow must notify

  (dispose)

  )


(tests
  "Pending is a special case of Throwable and impacts deduping the same way"
  (def !atom (atom 1))
  (def dispose (p/run (try (! (deduping (p/watch !atom)))
                           (catch Pending _))))

  ;; If userland never traps pending, should it be as if pending never happened?
  ;; Pending is meant to be managed but leaks into constructs like deduping.
  % := 1
  (reset! !atom (Failure. (Pending.)))
  (reset! !atom 1)
  % := 1                      ; no longer pending therefore the flow must notify

  (dispose)
  )


;; https://www.notion.so/hyperfiddle/Different-transfer-behavior-between-clj-and-cljs-example-43b59e02d5ea4d20a79225e23410dda1
#?(:clj                                ; Guarded until bug fixed, not a blocker.
   (tests
     (def !atom (atom 0))
     (def dispose (p/run (! (try (deduping (p/server (p/watch !atom)))
                                 (catch Pending _ ::pending)))))
     % := ::pending
     % := 0                             ; FAIL in cljs, sees ::rcf/timeout

     (swap! !atom inc)
     % := 1                             ; FAIL in cljs, sees ::rcf/timeout

     (swap! !atom identity)
     % := ::rcf/timeout
     (dispose)))


;; https://www.notion.so/hyperfiddle/distribution-glitch-stale-local-cache-of-remote-value-should-be-invalidated-pending-47f5e425d6cf43fd9a37981c9d80d2af
#_
(tests                                  ; FAIL will be adressed later
  "Distributed glitch"
  (def !atom (atom 0))
  (p/def x (p/watch !atom))
  (def dispose (p/run (! (try (deduping (let [y x] (p/server y)))
                              (catch Pending _ ::pending)))))

  % := ::pending
  % := 0

  (swap! !atom inc)
  % := ::pending
  % := 1

  (swap! !atom identity)
  % := ::pending
  % := 1                                ; We should not see duplicates
  (dispose)

  )
