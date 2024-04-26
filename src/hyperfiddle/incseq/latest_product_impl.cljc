(ns hyperfiddle.incseq.latest-product-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.diff-impl :as d])
  #?(:clj (:import (java.util.concurrent.locks Lock ReentrantLock)
                   (clojure.lang IFn IDeref))))

(def slot-lock 0)
(def slot-notifier 1)
(def slot-terminator 2)
(def slot-combinator 3)
(def slot-processes 4)
(def slot-buffers 5)
(def slot-freezers 6)
(def slot-counts 7)
(def slot-ready 8)
(def slot-push 9)
(def slot-live 10)
(def slot-args 11)
(def slots 12)

(defn lock [^objects state]
  #?(:clj (.lock ^Lock (aget state slot-lock))))

(defn unlock [^objects state]
  #?(:clj (.unlock ^Lock (aget state slot-lock))))

(defn call [f ^objects args]
  (case (alength args)
    0  (f)
    1  (f (aget args 0))
    2  (f (aget args 0) (aget args 1))
    3  (f (aget args 0) (aget args 1) (aget args 2))
    4  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3))
    5  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4))
    6  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5))
    7  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6))
    8  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7))
    9  (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8))
    10 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9))
    11 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10))
    12 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11))
    13 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12))
    14 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13))
    15 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14))
    16 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14) (aget args 15))
    17 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14) (aget args 15) (aget args 16))
    18 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14) (aget args 15) (aget args 16) (aget args 17))
    19 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14) (aget args 15) (aget args 16) (aget args 17) (aget args 18))
    20 (f (aget args 0) (aget args 1) (aget args 2) (aget args 3) (aget args 4) (aget args 5) (aget args 6) (aget args 7) (aget args 8) (aget args 9) (aget args 10) (aget args 11) (aget args 12) (aget args 13) (aget args 14) (aget args 15) (aget args 16) (aget args 17) (aget args 18) (aget args 19))
    (apply f (aclone args))))

(defn combine-indices [total-card degree r j]
  (eduction
    (mapcat (fn [k] (range k (unchecked-add-int k r))))
    (range (unchecked-multiply-int j r) total-card
      (unchecked-multiply-int degree r))))

(defn ensure-capacity [^objects freezers ^objects buffers item grow degree]
  (let [^ints freezer (aget freezers item)
        n (bit-shift-left (alength freezer) 5)]
    (when (< n degree)
      (loop [n n]
        (let [n (bit-shift-left n 1)]
          (if (< n degree)
            (recur n)
            (let [a (int-array (bit-shift-right n 5))
                  s (-> (unchecked-subtract-int degree grow)
                      (bit-shift-right 5)
                      (unchecked-inc-int))]
              #?(:clj (System/arraycopy freezer 0 a 0 s)
                 :cljs (dotimes [i s] (aset a i (aget freezer i))))
              (aset freezers item a)))))))
  (let [^objects buffer (aget buffers item)
        n (alength buffer)]
    (when (< n degree)
      (loop [n n]
        (let [n (bit-shift-left n 1)]
          (if (< n degree)
            (recur n)
            (let [a (object-array n)
                  s (unchecked-subtract-int degree grow)]
              #?(:clj (System/arraycopy buffer 0 a 0 s)
                 :cljs (dotimes [i s] (aset a i (aget buffer i))))
              (aset buffers item a))))))))

(defn compute-permutation [l r grow degree shrink permutation]
  (let [lr (unchecked-multiply l r)
        size-after (unchecked-subtract degree shrink)
        size-before (unchecked-subtract degree grow)
        r-create (unchecked-multiply r grow)
        r-degree (unchecked-multiply r degree)
        r-remove (unchecked-multiply r shrink)
        r-size-before (unchecked-multiply r size-before)
        r-size-after (unchecked-multiply r size-after)
        lr-size-after (unchecked-multiply lr size-after)
        lr-degree (unchecked-multiply lr degree)
        create-offset (unchecked-subtract lr-degree r-create)
        remove-offset (unchecked-subtract lr-size-after r-size-after)]
    (p/compose
      (reduce p/compose {}
        (eduction
          (map (fn [k]
                 (p/split-swap
                   (+ r-size-after (* k r-degree)) r-remove
                   (- remove-offset (* k r-size-after)))))
          (range l)))
      permutation
      (reduce p/compose {}
        (eduction
          (map (fn [k]
                 (p/split-swap
                   (- create-offset (* k r-degree))
                   (* k r-size-before) r-create)))
          (range l))))))

(defn freeze! [^ints freezer i]
  (let [j (int (bit-shift-right i 5))
        k (int (bit-and i (unchecked-dec (bit-shift-left 1 5))))]
    (aset freezer j (int (bit-set (aget freezer j) k)))))

(defn unfreeze! [^ints freezer i]
  (let [j (int (bit-shift-right i 5))
        k (int (bit-and i (unchecked-dec (bit-shift-left 1 5))))]
    (aset freezer j (int (bit-clear (aget freezer j) k)))))

(defn frozen? [^ints freezer i]
  (let [j (bit-shift-right i 5)
        k (bit-and i (unchecked-dec (bit-shift-left 1 5)))]
    (bit-test (aget freezer j) k)))

(defn flush-ready [^objects state item pull]
  (let [^objects processes (aget state slot-processes)
        ^ints ready (aget state slot-ready)
        arity (alength processes)]
    (loop [item item
           pull pull]
      (try @(aget processes item) (catch #?(:clj Throwable :cljs :default) _))
      (let [item (aget ready pull)]
        (when-not (== arity item)
          (aset ready pull arity)
          (recur item (rem (unchecked-inc-int pull) arity)))))))

(defn cancel [^objects state]
  (let [^objects processes (aget state slot-processes)]
    (dotimes [item (alength processes)] ((aget processes item)))))

(defn transfer [^objects state]
  (let [^objects processes (aget state slot-processes)
        ^objects freezers (aget state slot-freezers)
        ^objects buffers (aget state slot-buffers)
        ^objects args (aget state slot-args)
        ^ints counts (aget state slot-counts)
        ^ints ready (aget state slot-ready)
        offset (bit-shift-right (alength counts) 1)
        arity (alength processes)
        f (aget state slot-combinator)
        item (aget ready 0)]
    (aset ready 0 arity)
    (lock state)
    (try (loop [item item
                pull (rem 1 arity)
                diff (d/empty-diff (aget counts 1))]
           (let [count-index (unchecked-add-int offset item)
                 item-diff @(aget processes item)
                 item-grow (:grow item-diff)
                 item-shrink (:shrink item-diff)
                 item-degree (:degree item-diff)]
             (ensure-capacity freezers buffers item item-grow item-degree)
             (let [^ints freezer (aget freezers item)
                   ^objects buffer (aget buffers item)
                   size-before (unchecked-subtract-int item-degree item-grow)
                   size-after (unchecked-subtract-int item-degree item-shrink)]
               (aset counts count-index size-after)
               (loop [i size-before]
                 (when (< i item-degree)
                   (aset buffer i buffer)
                   (recur (unchecked-inc-int i))))
               (let [[l r] (loop [l 1, r 1, i count-index]
                             (case i
                               1 [l r]
                               (let [j (bit-shift-right i 1)]
                                 (if (odd? i)
                                   (let [x (aget counts (unchecked-dec-int i))]
                                     (aset counts j (unchecked-multiply-int x (aget counts i)))
                                     (recur (unchecked-multiply-int x l) r j))
                                   (let [x (aget counts (unchecked-inc-int i))]
                                     (aset counts j (unchecked-multiply-int x (aget counts i)))
                                     (recur l (unchecked-multiply-int x r) j))))))
                     lr-size-after (aget counts 1)
                     foreign-degree (unchecked-multiply-int l r)
                     product-degree (unchecked-multiply-int item-degree foreign-degree)
                     product-cycles (into #{}
                                      (mapcat
                                        (fn [cycle]
                                          (let [k (nth cycle 0)
                                                x (aget buffer k)
                                                f (frozen? freezer k)
                                                l (reduce
                                                    (fn [k l]
                                                      (aset buffer k (aget buffer l))
                                                      ((if (frozen? freezer l)
                                                         freeze! unfreeze!)
                                                       freezer k) l)
                                                    k (subvec cycle 1))]
                                            (aset buffer l x)
                                            ((if f freeze! unfreeze!)
                                             freezer k))
                                          (->> cycle
                                            (map (partial combine-indices product-degree item-degree r))
                                            (apply map vector))))
                                      (p/decompose (:permutation item-diff)))]
                 (loop [i size-after]
                   (when (< i item-degree)
                     (unfreeze! freezer i)
                     (aset buffer i nil)
                     (recur (unchecked-inc-int i))))
                 (let [product-grow (unchecked-multiply-int item-grow foreign-degree)
                       product-permutation (compute-permutation l r item-grow item-degree item-shrink
                                             (p/recompose product-cycles))
                       product-shrink (unchecked-multiply-int item-shrink foreign-degree)
                       product-change (persistent!
                                        (reduce-kv
                                          (fn [m k v]
                                            (let [^objects buffer (aget buffers item)]
                                              (if (= (aget buffer k) (aset buffer k v))
                                                m (reduce (fn [m i] (assoc! m i nil))
                                                    m (combine-indices lr-size-after size-after r k)))))
                                          (transient {}) (:change item-diff)))
                       product-freeze (persistent!
                                        (reduce
                                          (fn [s k]
                                            (freeze! (aget freezers item) k)
                                            (reduce conj! s (combine-indices lr-size-after size-after r k)))
                                          (transient #{}) (:freeze item-diff)))
                       diff (d/combine diff {:grow product-grow
                                             :degree product-degree
                                             :shrink product-shrink
                                             :permutation product-permutation
                                             :change product-change
                                             :freeze product-freeze})
                       item (aget ready pull)]
                   (if (== arity item)
                     (assoc diff
                       :change (persistent!
                                 (reduce (fn [m i]
                                           (loop [n i
                                                  j (alength buffers)]
                                             (let [j (unchecked-dec-int j)
                                                   c (aget counts (unchecked-add-int offset j))]
                                               (aset args j (aget ^objects (aget buffers j) (rem n c)))
                                               (if (pos? j)
                                                 (recur (quot n c) j)
                                                 (assoc! m i (call f args))))))
                                   (transient {}) (keys (:change diff))))
                       :freeze (persistent!
                                 (reduce (fn [s i]
                                           (loop [n i
                                                  j (alength freezers)]
                                             (let [j (unchecked-dec-int j)
                                                   c (aget counts (unchecked-add-int offset j))]
                                               (if (frozen? (aget freezers j) (rem n c))
                                                 (if (pos? j)
                                                   (recur (quot n c) j)
                                                   (conj! s i)) s))))
                                   (transient #{}) (:freeze diff))))
                     (do (aset ready pull arity)
                         (recur item (rem (unchecked-inc-int pull) arity) diff))))))))
         (catch #?(:clj Throwable :cljs :default) e
           (aset state slot-notifier nil)
           (cancel state)
           (let [push (aget state slot-push)]
             (loop [pull push]
               (let [item (aget ready pull)
                     pull (rem (unchecked-inc-int pull) arity)]
                 (if (== item arity)
                   (when-not (== pull push) (recur pull))
                   (flush-ready state item pull)))))
           (throw e))
         (finally
           (aset state slot-push nil)
           (let [live (aget state slot-live)]
             (unlock state)
             (when (zero? live)
               ((aget state slot-terminator))))))))

(defn terminated [^objects state]
  (lock state)
  (if (zero? (aset state slot-live (dec (aget state slot-live))))
    (if (nil? (aget state slot-push))
      (do (unlock state)
          ((aget state slot-terminator)))
      (unlock state)) (unlock state)))

(defn input-ready [^objects state item]
  (lock state)
  (let [^objects processes (aget state slot-processes)
        ^ints ready (aget state slot-ready)
        arity (alength processes)
        item (int item)]
    (if-some [i (aget state slot-push)]
      (do (aset state slot-push (identity (rem (unchecked-inc-int i) arity)))
          (aset ready i item)
          (unlock state))
      (do (aset state slot-push (identity (rem 1 arity)))
          (if-some [cb (aget state slot-notifier)]
            (do (aset ready 0 item)
                (unlock state)
                (cb))
            (do (flush-ready state item (rem 1 arity))
                (aset state slot-push nil)
                (if (zero? (aget state slot-live))
                  (do (unlock state)
                      ((aget state slot-terminator)))
                  (unlock state))))))))

(defn input-spawn [^objects state item flow]
  (let [^objects freezers (aget state slot-freezers)
        ^objects buffers (aget state slot-buffers)
        ^objects processes (aget state slot-processes)]
    (aset freezers item (int-array 1))
    (aset buffers item (object-array 1))
    (aset processes item
      (flow #(input-ready state item)
        #(terminated state))))
  state)

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn flow [f & diffs]
  (let [diffs (vec diffs)]
    (fn [n t]
      (let [state (object-array slots)
            arity (count diffs)
            ready (int-array arity)]
        (dotimes [i arity] (aset ready i arity))
        #?(:clj (aset state slot-lock (ReentrantLock.)))
        (aset state slot-notifier n)
        (aset state slot-terminator t)
        (aset state slot-combinator f)
        (aset state slot-args (object-array arity))
        (aset state slot-buffers (object-array arity))
        (aset state slot-freezers (object-array arity))
        (aset state slot-processes (object-array arity))
        (aset state slot-ready ready)
        (aset state slot-counts
          (let [o (loop [o 1]
                    (if (< o arity)
                      (recur (bit-shift-left o 1)) o))
                n (bit-shift-left o 1)
                arr (int-array n)]
            (loop [f (unchecked-subtract o arity)
                   o o
                   n n]
              (when (< 1 o)
                (loop [i (unchecked-subtract n f)]
                  (when (< i n)
                    (aset arr i 1)
                    (recur (unchecked-inc i))))
                (recur (bit-shift-right f 1)
                  (bit-shift-right o 1) o))) arr))
        (aset state slot-live (identity arity))
        (reduce-kv input-spawn state diffs)
        (->Ps state)))))