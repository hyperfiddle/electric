(ns hyperfiddle.spool
  #?(:cljs (:require-macros hyperfiddle.spool))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.rcf :refer [tests with % tap]]
            [missionary.core :as m]))

; If a e/fn param is touched, the output will be touched, even if the result will work skip.
; That means the diff will emit again, and be considered a new diff, which is wrong. (it's ok
; in contunuous time but not differential). This is a consequence of sequential let
; in both the macroexpansion of e/fn and also new, which expands -> binding -> let
; so if any arg is pending, the pending propagates (causally dependent), and also
; duplicate emissions. But we're not sure what's the exact cause - could be either.
; D: I think it was the duplicate on the min when it work skipped.
; L: To be clear, this pattern is the best we can do right now but I still think it
; is not immune to duplicates. If you have two successive diffs that happen to be the same
; there will be work skipping. So you will incorrectly skipp when diff. I'm not sure
; if can happen in your use case.
; if it did happen, the Electric network runtime will consider it a duplicate and decline
; to send it over the wire. as posted in slack yesterday.

; Theorem: duplicate diffs do not need to be seen. (e.g. grow 1 shrink 1 will squash
; to the empty diff which cannot have downstream consequence!)
; L: It's not exactly that; grow 1 shrink 1 can have meaning actually in succession, but if
; the elements are identical then it becomes a nil operation.
; Diffs have an additive identity (zero)? L: No, due to degree. The diffs depend on the current
; size of the collection which is the degree. Note that the degree is impl detail, we don't
; need to carry it we could accumulate it, so why did you? L: I needed degree for compose,
; if you can find a compose impl that doesn't need degree, then we have a zero element
; and therefore the semigroup is in fact a monoid. (note combine is squashing diffs without
; knowing anything about the previous state, so you cannot actually accumulate the degree
; . therefore degree is just a hint used to compute the before/after size of the diffs.)

; The reason why the semigroup works is bc we allow a grow followed by shrink which is a noop
; so the information cancels out. If we don't assume that then we cannot impl the semigroup,
; or rather we could but it would take infinite memory (proportional to the size of the history)

(e/defn Spool
  "maintain a playback buffer of width `limit` in response to changing playback
cursor `offset`"
  [<buffer log offset limit]
  (let [ ; differential flow. NOT rebuilt when offset changes.
        target (+ offset limit)] ; front wants to be limit ahead of offset
    (e/with-cycle [front (e/snapshot offset)] ; as offset slides, retain cursor state

      (cond
        (< front target) ; move forward
        (let [evict-n (- front limit) ; evict left-most element
              load-n front] ; load right-most element
          #_(println 'forward front '-> target 'load-n load-n 'evict-n evict-n)
          (<buffer evict-n {} nil)
          (<buffer load-n {} (get log load-n))
          (inc front))

        (= front target) ; caught up
        front

        (> front target) ; move backward
        (let [evict-n (dec front) ; evict right-most element
              load-n (dec (- front limit))] ; load right-most element
          #_(println 'backward front '-> target 'load-n load-n 'evict-n evict-n)
          (<buffer evict-n {} nil)
          (<buffer load-n {} (get log load-n))
          (dec front))))))

(tests
  "differential spooling"
  (def !offset (atom 0))
  (def !limit (atom 3))
  (with
    (e/run
      (let [spool (Spool. (vec (range 10)) (e/watch !offset) (e/watch !limit))
            window (new (m/reductions i/patch-vec [] (e/flow spool)))]
        (tap [window spool])))
    % := [[0] {:grow 1, :degree 1, :shrink 0, :permutation {}, :freeze #{}, :change {0 0}}]
    % := [[0 1] {:grow 1, :degree 2, :shrink 0, :permutation {}, :change {1 1}, :freeze #{}}]
    % := [[0 1 2] {:grow 1, :degree 3, :shrink 0, :permutation {}, :change {2 2}, :freeze #{}}]
    (swap! !offset inc) % := [[1 2 3] {:degree 4, :permutation {0 1, 1 2, 2 3, 3 0}, :grow 1, :shrink 1, :change {2 3}, :freeze #{}}]
    (swap! !offset inc) % := [[2 3 4] {:degree 4, :permutation {0 1, 1 2, 2 3, 3 0}, :grow 1, :shrink 1, :change {2 4}, :freeze #{}}]
    (swap! !offset dec) % := [[1 2 3] {:degree 4, :permutation {2 1, 3 2, 1 0, 0 3}, :grow 1, :shrink 1, :change {0 1}, :freeze #{}}]
    (swap! !offset dec) % := [[0 1 2] {:degree 4, :permutation {2 1, 3 2, 1 0, 0 3}, :grow 1, :shrink 1, :change {0 0}, :freeze #{}}]))

;; (tests
;;   "bigger jump"
;;   (def !offset (atom 0))
;;   (def !limit (atom 3))
;;   (def cancel
;;     (e/run
;;       (let [spool (Spool. (vec (range 100)) (e/watch !offset) (e/watch !limit))
;;             window (new (m/reductions i/patch-vec [] (e/flow spool)))]
;;         (tap [window spool]))))

;;   (swap! !offset inc)
;;   (swap! !offset dec)
;;   (swap! !offset (fn [x] (+ x 2)))
;;   (swap! !offset (fn [x] (- x 2)))
;;   (swap! !offset (fn [x] (+ x 10)))
;;   (swap! !offset (fn [x] (- x 10)))
;;   (cancel))


;; ; limit moves, not offset
;; ; log moves

;; (tests
;;   "limit update" ; happens because we take min with model-t-n
;;   (def !offset (atom 0))
;;   (def !limit (atom 3))
;;   (def cancel
;;     (e/run
;;       (let [spool (Spool. (vec (range 100)) (e/watch !offset) (e/watch !limit))
;;             window (new (m/reductions i/patch-vec [] (e/flow spool)))]
;;         (tap [window spool]))))

;;   (swap! !offset (fn [x] (+ x 10)))
;;   (swap! !offset (fn [x] (- x 10)))
;;   (swap! !limit inc)
;;   (swap! !limit dec)
;;   (cancel))

;; (tests
;;   "log update"
;;   (def !offset (atom 0))
;;   (def !limit (atom 3))
;;   (def !log (atom (vec (range 10))))
;;   (def cancel
;;     (e/run
;;       (let [spool (Spool. (e/watch !log) (e/watch !offset) (e/watch !limit))
;;             window (new (m/reductions i/patch-vec [] (e/flow spool)))]
;;         (tap [window spool]))))

;;   (swap! !log conj 42)

;;   (swap! !offset (fn [x] (+ x 10)))
;;   (swap! !offset (fn [x] (- x 10)))
;;   (swap! !offset inc)
;;   (swap! !offset dec)
;;   (swap! !limit inc)
;;   (swap! !limit dec)
;;   (cancel))

; Do not touch, internals knowledge here.
; Spine diffs are discrete; Electric's continuous time semantics can erroneously
; skip or duplicate diffs (corrupting the spine) . During network transfer,
; we must join the flow with `new` to move diff values over network, but we have
; carefully minimized the exposure of the flow to Electric evaluation model to
; ensure that each diff is seen once and only once (at the point of transfer)
; and then immediately lifted them back into a flow.
(defmacro spool-transfer [log offset limit]
  `(e/client
     (m/eduction (remove e/failure?) ; remove pending and cancelled (do we need to see cancelled?)
       (e/flow ; immediately lift diffs back into flow to shield from diff skipping and duplication
         (e/server ; point of transfer, see each diff once
           (new ; temporarily join flow for transfer
            (let [<buffer (i/spine)]
              (e/inhibit ; prevent implicit do (in let) from duplicating diffs
                (new Spool <buffer ~log ~offset ~limit))
              <buffer)))))))
