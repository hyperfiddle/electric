(ns hyperfiddle.incseq.to-stateful-impl
  (:require [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.stateful-diff-impl :as sd]
            [hyperfiddle.incseq.sync-impl :refer [get-sync cas-sync]])
  (:import #?(:clj (java.util.concurrent.atomic AtomicReference))
           #?(:clj (clojure.lang IFn IDeref))
           (missionary.impl PairingHeap)))

(def slot-step 0)
(def slot-done 1)
(def slot-input 2)
(def slot-degree 3)
(def slot-buffer 4)
(def slot-pending 5)
(def slot-child 6)
(def slot-sibling 7)
(def slot-owner 8)
(def slot-head 9)
(def slot-sync 10)
(def slots 11)

(def inner-slot-position 0)
(def inner-slot-input 1)
(def inner-slot-current 2)
(def inner-slot-frozen 3)
(def inner-slot-child 4)
(def inner-slot-sibling 5)
(def inner-slots 6)

(defn get-input [^objects state ^objects item]
  (if (identical? state item) (aget state slot-input) (aget item inner-slot-input)))

(defn get-child [^objects state ^objects item]
  (if (identical? state item) (aget state slot-child) (aget item inner-slot-child)))

(defn set-child [^objects state ^objects item x]
  (if (identical? item state) (aset state slot-child x) (aset item inner-slot-child x)))

(defn get-sibling [^objects state ^objects item]
  (if (identical? item state) (aget state slot-sibling) (aget item inner-slot-sibling)))

(defn set-sibling [^objects state ^objects item x]
  (if (identical? item state) (aset state slot-sibling x) (aset item inner-slot-sibling x)))

(def impl
  (PairingHeap/impl (fn [r x _] (identical? r x))
    get-child set-child get-sibling set-sibling))

(def idle #?(:clj (Object.) :cljs (js-obj)))

(defn claim [^objects state]
  (aset state slot-owner #?(:clj (Thread/currentThread) :cljs true)))

(defn yield [^objects state]
  (aset state slot-owner #?(:clj nil :cljs false)))

(defn terminated [^objects state item]
  (identical? idle (get-child state item)))

(defn dequeue [^objects state head]
  (aset state slot-pending (inc (aget state slot-pending)))
  (aset state slot-head (PairingHeap/dmin impl state head))
  (set-sibling state head idle))

(defn event [^objects state item]
  (aset state slot-pending
    (dec (aget state slot-pending)))
  (set-sibling state item nil)
  (when-not ^boolean (terminated state item)
    (aset state slot-head
      (if-some [h (aget state slot-head)]
        (PairingHeap/meld impl state h item)
        item)) nil))

(defn consume [^objects state item]
  (loop [i item]
    (let [s (get-sibling state i)]
      (event state i)
      (when (some? s)
        (recur s)))))

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
        (let [^objects item (aget state slot-head)]
          (dequeue state item)
          (if (or (identical? state item) (not (aget item inner-slot-frozen)))
            (do (claim state)
                (try @(get-input state item)
                     (catch #?(:clj Throwable :cljs :default) _))
                (yield state))
            (do (aset item inner-slot-child idle)
                (event state item)))
          (recur))))))

(defn ensure-capacity [^objects state cap]
  (let [^objects buffer (aget state slot-buffer)
        size (alength buffer)]
    (if (< size cap)
      (let [b (object-array
                (loop [i size]
                  (let [i (bit-shift-left i 1)]
                    (if (< i cap) (recur i) i))))]
        (a/acopy buffer 0 b 0 size)
        (aset state slot-buffer b))
      buffer)))

(defn move-at [^objects buffer ^objects inner pos]
  (aset inner inner-slot-position pos)
  (aset buffer pos inner) buffer)

(defn apply-cycle [^objects buffer cycle]
  (let [i (nth cycle 0)]
    (move-at buffer
      (aget buffer i)
      (loop [i i
             k 1]
        (let [j (nth cycle k)
              k (unchecked-inc-int k)]
          (move-at buffer (aget buffer j) i)
          (if (< k (count cycle))
            (recur j k) j))))))

(defn step [^objects state item]
  (if #?(:clj (identical? (Thread/currentThread) (aget state slot-owner))
         :cljs ^boolean (aget state slot-owner))
    (event state item)
    (loop []
      (let [s (get-sync state slot-sync)]
        (if (identical? s idle)
          (cas-sync state slot-sync idle nil
            (do (event state item)
                (ready state)) (recur))
          (do (set-sibling state item s)
              (cas-sync state slot-sync s item
                nil (recur))))))))

(defn spawn-inner [^objects state flow]
  (let [^objects buffer (aget state slot-buffer)
        inner (object-array inner-slots)
        p (unchecked-inc (aget state slot-pending))
        pos (aget state slot-degree)]
    (aset buffer pos inner)
    (aset state slot-pending p)
    (aset state slot-degree (unchecked-inc pos))
    (aset inner inner-slot-current inner)
    (aset inner inner-slot-position pos)
    (aset inner inner-slot-frozen false)
    (aset inner inner-slot-input
      (flow #(step state inner)
        #(do (aset inner inner-slot-frozen true)
             (step state inner))))
    (when (or (== p (aget state slot-pending)) ^boolean (aget inner inner-slot-frozen))
      (throw (new #?(:clj Error :cljs js/Error) "Uninitialized flow.")))
    state))

(defn cancel-inner [^objects state]
  (let [pos (unchecked-dec (aget state slot-degree))
        ^objects buffer (aget state slot-buffer)
        ^objects inner (aget buffer pos)]
    (aset state slot-degree pos)
    (aset inner inner-slot-position nil)
    ((aget inner inner-slot-input))
    (aset buffer pos nil) buffer))

(defn cancel [^objects state]
  (let [^objects buffer (aget state slot-buffer)
        size (aget state slot-degree)]
    ((aget state slot-input))
    (loop [i 0]
      (when (< i size)
        (let [^objects inner (aget buffer i)]
          ((aget inner inner-slot-input))
          (recur (unchecked-inc i)))))))

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
    (loop [d (sd/empty-diff (aget state slot-degree))]
      (if-some [^objects item (aget state slot-head)]
        (do (dequeue state item)
            (recur (if (identical? state item)
                     (let [diff @(aget state slot-input)
                           append (d/append diff)
                           degree (d/degree diff)
                           shrink (d/shrink diff)
                           permutation (p/inverse (d/permutation diff))
                           ^objects buffer (ensure-capacity state degree)]
                       (reduce spawn-inner state append)
                       (p/decompose apply-cycle buffer permutation)
                       (dotimes [_ shrink] (cancel-inner state))
                       (sd/combine d
                         {:grow        (count append)
                          :shrink      shrink
                          :degree      degree
                          :permutation permutation
                          :change      {}
                          :freeze      #{}}))
                     (if ^boolean (aget item inner-slot-frozen)
                       (do (aset item inner-slot-child idle)
                           (event state item)
                           (if-some [pos (aget item inner-slot-position)]
                             (update d :freeze conj pos) d))
                       (if-some [pos (aget item inner-slot-position)]
                         (let [x @(aget item inner-slot-input)]
                           (if (= (aget item inner-slot-current)
                                 (aset item inner-slot-current x))
                             d (update d :change assoc pos x)))
                         (do (try @(aget item inner-slot-input)
                                  (catch #?(:clj Throwable :cljs :default) _))
                             d)))))) d))
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

(defn flow [incseq-of-continuous-flows]
  (fn [s d]
    (let [state (object-array slots)]
      (claim state)
      (aset state slot-step s)
      (aset state slot-done d)
      (aset state slot-degree (identity 0))
      (aset state slot-buffer (object-array 1))
      (aset state slot-pending (identity 1))
      #?(:clj (aset state slot-sync (AtomicReference.)))
      (aset state slot-input
        (incseq-of-continuous-flows #(step state state)
          #(do (aset state slot-child idle) (step state state))))
      (when (or (pos? (aget state slot-pending)) ^boolean (terminated state state))
        (aset state slot-step nil)
        (cancel state))
      (yield state) (s)
      (->Ps state))))