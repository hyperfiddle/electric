(ns geoffrey.differ
  (:require [differ.core :refer [diff patch]]
            [minitest :refer [tests]]))

;;; differ/differ {:mvn/version "0.3.3"}

;;; Differ is a fast¹ but constrained diffing library.
;;; It provides two functions:

;;; `differ.core/diff` ([state new-state])
;;; Returns a vector containing the differing, and non-existant elements, of
;;; two clojure datastructures.

;;; `differ.core/patch` ([state [alterations removals]])
;;; Applies a diff, as created by the diff function, to any² datastructure.

;;; ¹ See benchmark charts on this page https://github.com/juji-io/editscript ²
;;; This is slightly incorrect, differ got flaws but they might not apply to our
;;; use case.

(tests
 ;; Scalars
 (diff :a :b) := [:b :a]   ; :b replaces :a
 (patch :a [:b :a]) := :b  ; Apply on :a that :b replaces :a
 (diff 1 2) := [2 1]       ; 2  replaces 1
 (patch 1 [2 1]) := 2      ; Apply on 1 that 2 replaces 1

 (diff "Alice" "Bob") := ["Bob" "Alice"] ; Strings are considered scalars
 )

(tests
 ;; Sets
 (diff #{:a} #{:a :b}) := [ #{:b}  #{} ]  ; [adds rets]
 (patch #{:a} [#{:b} #{}]) := #{:a :b}

 (diff #{:a :b} #{:a}) := [#{} #{:b}] ; :b got removed
 (patch #{:a :b} [#{} #{:b}]) := #{:a}

 ;; ⚠ Here be dragons, differ performs a shallow diff on sets.
 (diff #{#{:a}} #{#{:a :b} :c}) := [#{#{:b :a} :c} #{#{:a}}]
 (patch #{#{:a}} [#{#{:b :a} :c} #{#{:a}}]) := #{#{:a :b} :c}

 ;; The shallow diff don't depend on values inside the set. Set elements form
 ;; their own identity from differ POV.
 (diff #{:a {:b 1}} #{{:b 2}}) := [#{{:b 2}} #{{:b 1} :a}]
 )

(tests
 ;; Vectors
 (diff [:a :b] [:a :b :c]) := [[:+ :c] []] ; <left coll> is smaller then <right coll>, so add
                                           ; (:+) the value :c at the end.

 (diff [:a :b] [:a :b :c :d]) := [[:+ :c :+ :d] []] ; Looks like it's missing a simple grammar, :+
                                                    ; could have been factored. But it's fine we
                                                    ; use Transit.

 (diff [:a :b :c] [:d :c :b :a]) := [[0 :d 1 :c 2 :b :+ :a] []]
 ;; Index 0 is now :d
 ;;       1 is now :c
 ;;       2 is now :b
 ;;       append :a

 (diff [:a :b :c :d] [:a :c]) := [[1 :c] [2]]
 ;; Index 1 is now :c
 ;; Drop the last 2 elements.
 )


(tests
 ;; Lists
 (diff '(:a :b) '(:a :b :c))    := '[(:+ :c) ()]
 (diff '(:a :b :c :d) '(:a :c)) := '[(1 :c)  (2)]

 ;; It works the same for vectors and lists. Unfortunately, the implementation
 ;; for lists is naive: it diffs them as vectors, then uses `(into () (reverse …))`.
 ;; See `differ.diff/alterations`
 )

(tests
 ;; Maps
 (diff [{:a 1}] [{:a 1, :b 2}]) := [[0 {:b 2}] []]
 (diff {:a 1, :b 2} {:a 1}) := [{} {:b 0}] ; Odd design decision, 0 means _. A rets set would
                                           ; have been enough.

 ;; Nested maps
 (diff {:a 1, :b {:c 2}} {:a 1, :b {:c 3}})       := [{:b {:c 3}} {}] ; ⚠ Is this diff shallow?
 (diff {:a 1, :b {:c 3}} {:a 1, :b {:c 3, :d 4}}) := [{:b {:d 4}} {}] ; ⚠ No it's not.
 )



;;;; Summary
;;; - Differ performs a deep linear diff of two data structures of the same type.
;;; - On sets, it performs a shallow diff.
;;; - It's time complexity might be high on huge lists diffs.
;;; - Its implementation is 200 LOC, we could easily fork it and make it shallow on all
;;;   sequential structures, and keep it deep on maps.

;;;; Opinion
;;; This library is effective for simple use cases. We might prefer something
;;; more robust and efficient like
;;; `https://github.com/juji-io/editscript/issues`, though it's not performing
;;; shallow diffs.
