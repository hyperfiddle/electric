(ns contrib.test-match
  (:require [contrib.debug :as dbg]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [fipp.ednize]))

(set! *warn-on-reflection* true)

;; test matcher
;; goal: succinct, simple, useful test output
;; patterns:
;; _             -> any value
;; _&            -> any values
;; view f subpat -> match subpat on (f v)
;;
;; missing:
;; - strict map check (all keys)
;; - compiler instead of interpreter
;; - locals inside pattern
;; - unification (logical vars)
;; - guard predicates
;;
;; differences to matcher-combinators:
;; - no pretty printing. Result can be used programatically
;; - more concise syntax
;; - fits on 1 page
;; - test framework agnostic
;;
;; how to check if pattern matched?
;;   (= v (test-match v pat))
;; this is also RCF-friendly

(deftype Diff [a b]
  Object
  (toString [_] (str "<<DIFF " a " -- " b " >>"))
  (hashCode [_] (+ (.hashCode a) (.hashCode b)))
  (equals [_ that]
    (and (instance? Diff that)
      (= a (.-a ^Diff that)) (= b (.-b ^Diff that))))
  clojure.lang.IPersistentCollection
  (equiv [this that] (.equals this that))
  fipp.ednize/IEdn
  (-edn [_] (list '<<DIFF  a b))
  )

(tests
  (->Diff 1 [2]) := (->Diff 1 [2])
  #{(->Diff 1 [2])} := #{(->Diff 1 [2])}
  )

(defmethod print-method Diff [^Diff d ^java.io.Writer w]
  (.write w "<<DIFF ") (.write w (str (.-a d))) (.write w " -- ") (.write w (str (.-b d))) (.write w " >>"))

(deftype Missing []
  Object (toString [_] "_")
  fipp.ednize/IEdn (-edn [_] '_)
  )
(defmethod print-method Missing [_ ^java.io.Writer w] (.write w "_"))
(def missing (Missing.))

(defn pair [v pat]
  (loop [v v, pat pat, ret []]
    (let [v* (if (seq v) (first v) missing)
          pat* (if (seq pat) (first pat) missing)]
      (if (= missing v* pat*)
        ret
        (recur (rest v) (rest pat) (conj ret [v* pat*]))))))

(tests
  (pair [1] [:a]) := [[1 :a]]
  (pair [1 2] [:a]) := [[1 :a] [2 missing]]
  (pair [1] [:a :b]) := [[1 :a] [missing :b]]
  )

(defn test-match [v pat]
  (cond
    (coll? pat) (if (and (or (list? pat) (seq? pat)) (= `view (first pat)))
                  ;; TODO turn into pattern compiler so we don't need `eval`
                  (let [[_ ap subpat] pat, subv (eval (list ap v)), ret (test-match subv subpat)]
                    (if (= subv ret) v ret))
                  ;; if (contrib.debug/dbgv (instance? (class pat) v))
                  (if (coll? v)
                    (cond
                      (map? v)
                      (if (map? pat)
                        (let [[v pat] (reduce-kv (fn [[ac pat] k v]
                                                   (if (contains? pat k)
                                                     [(assoc ac k (test-match v (get pat k))) (dissoc pat k)]
                                                     [(assoc ac k v) pat]))
                                        [{} pat] v)]
                          (reduce-kv (fn [ac k pat] (assoc ac k (test-match missing pat))) v pat))
                        (->Diff v pat))

                      (set? v)
                      (if (set? pat)
                        (reduce (fn [v nx] (if (contains? v nx) v (conj v (->Diff missing nx)))) v pat)
                        (->Diff v pat))

                      :else
                      (let [ret (first (reduce (fn [[ac care?] [v pat]]
                                                 (if care?
                                                   (let [ret (test-match v pat)]
                                                     (if (= ::dont-care ret)
                                                       [(conj ac v) false]
                                                       [(conj ac ret) care?]))
                                                   [(conj ac v) false]))
                                         [(empty v) true] (pair v pat)))
                            listy-v? (or (list? v) (seq? v)), listy-pat? (or (list? pat) (seq? pat))]
                        (if (and (seq v) (every? #(instance? Diff %) ret))
                          (->Diff (into (empty v) (map #(.-a ^Diff %)) ret)
                            (into (empty pat) (map #(.-b ^Diff %)) (cond-> ret (not= listy-v? listy-pat?) reverse)))
                          (cond-> ret (or (list? v) (seq? v)) reverse))))
                    (->Diff v pat)))
    (= `_& pat) ::dont-care
    (= `_ pat) v
    (= v pat) v
    :else (->Diff v pat))
  )

(tests
  (test-match 1 1) := 1
  (test-match :x :x) := :x
  (test-match 1 0) := (->Diff 1 0)
  (test-match 1 2) := (->Diff 1 2)
  (test-match [1 2] [1 2]) := [1 2]
  (test-match [1 2] [1 0]) := [1 (->Diff 2 0)]
  (test-match '(1 2) [1 0]) := (list 1 (->Diff 2 0))
  (test-match '(1 2) '(1 2)) := '(1 2)
  (class (test-match '(1 2) '(1 2))) := (class '(1 2))
  (test-match [1 2 3] [1 2]) := [1 2 (->Diff 3 missing)]
  (test-match [1 2] [1 2 3]) := [1 2 (->Diff missing 3)]
  (test-match [1 2 3] [1 `_&]) := [1 2 3]
  (test-match [1 2 3] [1 `_ 3]) := [1 2 3]
  (test-match [1 2] [1 2 `_]) := [1 2 missing]
  (test-match [1 [2 3]] [1 [2 `_]]) := [1 [2 3]]
  (test-match `(inc (dec x)) `(inc (dec _))) := `(inc (dec x))
  (test-match {:x 1} {:x `_}) := {:x 1}
  (test-match {:x 1} 1) := (->Diff {:x 1} 1)
  (test-match 1 {:x 1}) := (->Diff 1 {:x 1})
  (test-match {:x 1} {:x 1 :y 2}) := {:x 1 :y (->Diff missing 2)}
  (test-match {:x 1} {:x 1 :y `_}) := {:x 1 :y missing}
  (test-match {:x 1, :y 2} {:y 2}) := {:x 1, :y 2}
  (test-match {:x [1 2], :y 3} {:x [1 `_]}) := {:x [1 2], :y 3}
  (test-match [1 2] `(view first 1)) := [1 2]
  (test-match [1 2] `(view first 2)) := (->Diff 1 2)
  (test-match [1 2] `[(view identity 0) 2]) := [(->Diff 1 0) 2]
  (test-match [1 2] [3 4]) := (->Diff [1 2] [3 4])
  (test-match '(1 2) [3 4]) := (->Diff '(1 2) [3 4])
  (test-match [1 2] '(3 4)) := (->Diff [1 2] '(3 4))
  (test-match '(1 2) '(3 4)) := (->Diff '(1 2) '(3 4))
  (test-match [] []) := []
  (test-match #{1 2 3} #{1 2}) := #{1 2 3}
  (test-match #{1 2} #{2 3}) := #{1 2 (->Diff missing 3)}
  (test-match #{1 2 3} [1 2]) := (->Diff #{1 2 3} [1 2])
  (test-match {:a 1} [:a 1]) := (->Diff {:a 1} [:a 1])

  (require '[hyperfiddle.electric.impl.lang-de :as-alias lang])
  (require '[hyperfiddle.electric.impl.runtime-de :as-alias r])
  (let [v `(r/peer
             (lang/r-defs
               (lang/r-static 1)
               (lang/r-ap (lang/r-static
                            (clojure.core/fn [x32133]
                              (clojure.core/fn [& rest-args32134]
                                (clojure.core/let [x x32133]
                                  (clojure.core/apply (fn* ([] x)) rest-args32134)))))
                 (lang/r-local 0)))
             [] 1)]
    (test-match v
      `(r/peer
         (lang/r-defs
           (lang/r-static 1)
           (lang/r-ap (lang/r-static (clojure.core/fn _&))
             (lang/r-local 0)))
         [] 1)) := v)
  )
