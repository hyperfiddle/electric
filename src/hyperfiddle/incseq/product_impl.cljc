(ns hyperfiddle.incseq.product-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.sync-impl :refer [get-sync cas-sync]])
  (:import #?(:clj (java.util.concurrent.atomic AtomicReference))
           #?(:clj (clojure.lang IFn IDeref))
           (missionary.impl PairingHeap)))

(def slot-step 0)
(def slot-done 1)
(def slot-thunk 2)
(def slot-inputs 3)
(def slot-children 4)
(def slot-siblings 5)
(def slot-buffers 6)
(def slot-counts 7)
(def slot-pending 8)
(def slot-owner 9)
(def slot-head 10)
(def slot-sync 11)
(def slots 12)

(defn inputs ^objects [^objects state]
  (aget state slot-inputs))

(defn buffers ^objects [^objects state]
  (aget state slot-buffers))

(defn children ^objects [^objects state]
  (aget state slot-children))

(defn siblings ^objects [^objects state]
  (aget state slot-siblings))

(def idle #?(:clj (Object.) :cljs (js-obj)))

(def impl
  (PairingHeap/impl
    (fn [_ x y] (< x y))
    (fn [state i] (aget (children state) i))
    (fn [state i x] (aset (children state) i x))
    (fn [state i] (aget (siblings state) i))
    (fn [state i x] (aset (siblings state) i x))))

(defn terminated [^objects state i]
  (identical? idle (aget (children state) i)))

(defn event [^objects state i]
  (aset state slot-pending
    (dec (aget state slot-pending)))
  (aset (siblings state) i nil)
  (when-not ^boolean (terminated state i)
    (aset state slot-head
      (if-some [h (aget state slot-head)]
        (PairingHeap/meld impl state h i)
        i)) nil))

(defn consume [^objects state i]
  (loop [i i]
    (let [s (aget (siblings state) i)]
      (event state i)
      (when (some? s)
        (recur s)))))

(defn dequeue [^objects state]
  (aset state slot-pending (inc (aget state slot-pending)))
  (let [h (aget state slot-head)]
    (aset state slot-head (PairingHeap/dmin impl state h))
    (aset (siblings state) h idle) h))

(defn claim [^objects state]
  (aset state slot-owner #?(:clj (Thread/currentThread) :cljs true)))

(defn yield [^objects state]
  (aset state slot-owner #?(:clj nil :cljs false)))

(defn ready [^objects state]
  (loop []
    (if (nil? (aget state slot-head))
      (if (zero? (aget state slot-pending))
        ((aget state slot-done))
        (cas-sync state slot-sync nil idle
          nil (do (loop []
                    (let [s (get-sync state slot-sync)]
                      (cas-sync state slot-sync s nil
                        (consume state s)
                        (recur))))
                  (recur))))
      (if-some [step (aget state slot-step)]
        (step)
        (do (claim state)
            (try @(aget (inputs state) (dequeue state))
                 (catch #?(:clj Throwable :cljs :default) _))
            (yield state)
            (recur))))))

(defn double-upto [n degree]
  (loop [n n]
    (let [n (bit-shift-left n 1)]
      (if (< n degree)
        (recur n) n))))

(defn ensure-capacity ^objects [^objects buffers item degree]
  (let [buffer (aget buffers item)
        n (alength buffer)]
    (if (< n degree)
      (let [b (object-array (double-upto n degree))]
        (a/acopy buffer 0 b 0 n)
        (aset buffers item b))
      buffer)))

(defn cancel [^objects state]
  (dotimes [i (alength (inputs state))]
    ((aget (inputs state) i))))

(defn combine-indices [total-card degree r j]
  (eduction
    (mapcat (fn [k] (range k (unchecked-add k r))))
    (range (unchecked-multiply j r) total-card
      (unchecked-multiply degree r))))

(defn compute-diff [^objects state l r grow degree shrink cycles]
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
    (d/diff (into []
              (mapcat (fn [i]
                        (eduction
                          (map (fn [j]
                                 (let [^ints counts (aget state slot-counts)]
                                   (aset counts 0 j)
                                   (let [result ((aget state slot-thunk))]
                                     (aset counts 1 lr-size-after) result))))
                          (combine-indices lr-size-after size-after r i))))
              (range size-before degree))
      lr-degree (unchecked-multiply lr shrink)
      (p/compose
        (reduce p/compose {}
          (eduction
            (map (fn [k]
                   (p/split-swap
                     (unchecked-subtract create-offset (unchecked-multiply k r-degree))
                     r-create
                     (unchecked-multiply k r-size-before))))
            (range l)))
        (p/recompose
          (eduction
            (map (fn [cycle]
                   (->> cycle
                     (map (partial combine-indices lr-degree degree r))
                     (apply map vector))))
            cycles))
        (reduce p/compose {}
          (eduction
            (map (fn [k]
                   (p/split-swap
                     (unchecked-add r-size-after (unchecked-multiply k r-degree))
                     (unchecked-subtract remove-offset (unchecked-multiply k r-size-after))
                     r-remove)))
            (range l)))))))

(defn apply-cycle [^objects buffer cycle]
  (let [k (peek cycle)
        x (aget buffer k)]
    (aset buffer
      (reduce (fn [k l] (aset buffer l (aget buffer k)) l)
        k (pop cycle)) x)
    buffer))

(defn clear-slot [^objects buffer i]
  (aset buffer i nil) buffer)

(defn transfer [^objects state]
  (when (nil? (aget state slot-step))
    (ready state)
    (throw (new #?(:clj Error :cljs js/Error) "Uninitialized incseq.")))
  (claim state)
  (when (pos? (aget state slot-pending))
    (when-some [s (get-sync state slot-sync)]
      (loop [s s]
        (cas-sync state slot-sync s nil
          (consume state s)
          (recur (get-sync state slot-sync))))))
  (try
    (let [^ints counts (aget state slot-counts)
          offset (bit-shift-right (alength counts) 1)]
      (loop [d (d/empty-diff (aget counts 1))]
        (if (nil? (aget state slot-head))
          d (let [i (dequeue state)
                  j (unchecked-add offset i)
                  diff @(aget (inputs state) i)
                  buffer (ensure-capacity (aget state slot-buffers) i (d/degree diff))
                  append (d/append diff)
                  degree (d/degree diff)
                  shrink (d/shrink diff)
                  grow (count append)
                  size-before (unchecked-subtract degree grow)
                  size-after (unchecked-subtract degree shrink)
                  cycles (p/decompose into #{} (d/permutation diff))]
              (reduce-kv (fn [_ i x] (aset buffer (unchecked-add size-before i) x)) nil append)
              (reduce apply-cycle buffer cycles)
              (reduce clear-slot buffer (range size-after degree))
              (aset counts j size-after)
              (recur
                (d/combine d
                  (loop [l 1, r 1, j j]
                    (case j
                      1 (compute-diff state l r grow degree shrink cycles)
                      (let [c (aget counts j)
                            k (bit-shift-right j 1)]
                        (if (odd? j)
                          (let [x (aget counts (unchecked-dec j))]
                            (aset counts k (unchecked-multiply x c))
                            (recur (unchecked-multiply x l) r k))
                          (let [x (aget counts (unchecked-inc j))]
                            (aset counts k (unchecked-multiply x c))
                            (recur l (unchecked-multiply x r) k))))))))))))
    (catch #?(:clj Throwable :cljs :default) e
      (aset state slot-step nil)
      (cancel state)
      (throw e))
    (finally
      (yield state)
      (ready state))))

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn arg [^objects state j]
  (let [^objects buffer (aget (buffers state) j)
        ^ints counts (aget state slot-counts)
        i (aget counts 0)
        n (aset counts 1 (quot (aget counts 1) (aget counts (unchecked-add-int (bit-shift-right (alength counts) 1) j))))]
    (aset counts 0 (rem i n))
    (aget buffer (quot i n))))

(defn thunk [arity f arg]
  (case arity
    0 f
    1 #(f (arg 0))
    2 #(f (arg 0) (arg 1))
    3 #(f (arg 0) (arg 1) (arg 2))
    4 #(f (arg 0) (arg 1) (arg 2) (arg 3))
    5 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4))
    6 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5))
    7 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6))
    8 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7))
    9 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8))
    10 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9))
    11 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10))
    12 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11))
    13 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12))
    14 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13))
    15 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14))
    16 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14) (arg 15))
    17 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14) (arg 15) (arg 16))
    18 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14) (arg 15) (arg 16) (arg 17))
    19 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14) (arg 15) (arg 16) (arg 17) (arg 18))
    20 #(f (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7) (arg 8) (arg 9) (arg 10) (arg 11) (arg 12) (arg 13) (arg 14) (arg 15) (arg 16) (arg 17) (arg 18) (arg 19))
    #(apply f (map arg (range arity)))))

(defn step [^objects state i]
  (if #?(:clj (identical? (Thread/currentThread) (aget state slot-owner))
         :cljs ^boolean (aget state slot-owner))
    (event state i)
    (loop []
      (let [s (get-sync state slot-sync)]
        (if (identical? s idle)
          (cas-sync state slot-sync idle nil
            (do (event state i)
                (ready state)) (recur))
          (do (aset (siblings state) i s)
              (cas-sync state slot-sync s i
                nil (recur))))))))

(defn spawn [^objects state i flow]
  (let [p (aget state slot-pending)]
    (aset (buffers state) i (object-array 1))
    (aset (inputs state) i
      (flow #(step state i)
        #(do (aset (children state) i idle)
             (step state i))))
    (or (== p (aget state slot-pending)) ^boolean (terminated state i))))

(defn flow [f & incseqs]
  (let [incseqs (vec incseqs)]
    (fn [s d]
      (let [state (object-array slots)
            arity (count incseqs)]
        (claim state)
        (aset state slot-step s)
        (aset state slot-done d)
        (aset state slot-thunk (thunk arity f (partial arg state)))
        (aset state slot-inputs (object-array arity))
        (aset state slot-children (object-array arity))
        (aset state slot-siblings (object-array arity))
        (aset state slot-buffers (object-array arity))
        (aset state slot-counts (a/weight-tree arity))
        (aset state slot-pending (identity arity))
        #?(:clj (aset state slot-sync (AtomicReference.)))
        (when (< (reduce-kv
                   (fn [initialized i flow]
                     (if ^boolean (spawn state i flow)
                       initialized (unchecked-inc initialized)))
                   0 incseqs) arity)
          (aset state slot-step nil)
          (cancel state))
        (yield state) (s)
        (->Ps state)))))