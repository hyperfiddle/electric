(ns hyperfiddle.incseq.latest-concat-impl
  (:require [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.diff-impl :as d])
  #?(:clj (:import (clojure.lang IFn IDeref))))

(def slot-notifier 0)
(def slot-terminator 1)
(def slot-process 2)
(def slot-buffer 3)
(def slot-counts 4)
(def slot-ready 5)
(def slot-push 6)
(def slot-live 7)
(def slot-busy 8)
(def slot-value 9)
(def slots 10)

(def inner-slot-process 0)
(def inner-slot-index 1)
(def inner-slots 2)

(defn nop [])

(defn drain [^objects state]
  (loop [i 0]
    (if (aget state slot-busy)
      (do (aset state slot-busy false)
          (try @(aget state slot-process)
               (catch #?(:clj Throwable :cljs :default) _))
          (recur i))
      (let [^objects q (aget state slot-ready)]
        (if-some [^objects input (aget q i)]
          (do (aset q i nil)
              (try @(aget input inner-slot-process)
                   (catch #?(:clj Throwable :cljs :default) _))
              (recur (rem (unchecked-inc-int i) (alength q))))
          (do (aset state slot-push nil)
              (if (zero? (aget state slot-live))
                (aget state slot-terminator) nop)))))))

(defn outer-ready [^objects state]
  ((locking state
     (aset state slot-busy true)
     (if (nil? (aget state slot-push))
       (do (aset state slot-push 0)
           (if-some [cb (aget state slot-notifier)]
             cb (drain state))) nop))))

(defn inner-ready [^objects state ^objects input]
  ((locking state
     (let [^objects q (aget state slot-ready)
           c (alength q)]
       (if-some [i (aget state slot-push)]
         (do (aset state slot-push
               (identity
                 (if (nil? (aget q i))
                   (do (aset q i input)
                       (rem (unchecked-inc-int i) c))
                   (let [n (bit-shift-left c 1)
                         a (object-array n)]
                     (aset state slot-ready a)
                     (a/acopy q i a i
                       (unchecked-subtract-int c i))
                     (a/acopy q 0 a c i)
                     (let [p (unchecked-add-int i c)]
                       (aset a p input)
                       (rem (unchecked-inc-int p) n)))))) nop)
         (do (aset state slot-push (identity (rem 1 c)))
             (aset q 0 input)
             (if-some [cb (aget state slot-notifier)]
               cb (drain state))))))))

(defn terminated [^objects state]
  ((locking state
     (if (zero? (aset state slot-live (dec (aget state slot-live))))
       (if (nil? (aget state slot-push)) (aget state slot-terminator) nop) nop))))

(defn cancel [^objects state]
  (locking state
    ((aget state slot-process))
    (let [^objects buffer (aget state slot-buffer)
          ^ints counts (aget state slot-counts)]
      (dotimes [i (aget counts 0)]
        (let [^objects inner (aget buffer i)]
          ((aget inner inner-slot-process)))))))

(defn index-in-counts [^ints counts index]
  (unchecked-add (bit-shift-right (alength counts) 1) index))

(defn compute-offset [^ints counts index l]
  (let [delta (unchecked-subtract-int l (aget counts index))]
    (loop [o 0, i (int index)]
      (aset counts i (unchecked-add-int (aget counts i) delta))
      (case i
        1 o
        (recur (if (even? i)
                 o (unchecked-add o
                     (aget counts (unchecked-dec-int i))))
          (bit-shift-right i 1))))))

(defn ensure-capacity [^objects state grow degree shrink]
  (loop []
    (let [counts ^ints (aget state slot-counts)
          buffer ^objects (aget state slot-buffer)
          length (alength buffer)]
      (if (< length degree)
        (let [new-length (alength counts)
              new-counts (a/int-array (bit-shift-left new-length 1))]
          (a/acopy buffer 0 (aset state slot-buffer (object-array new-length)) 0 length)
          (aset new-counts 1 (aget counts 1))
          (loop [i 1]
            (let [j (bit-shift-left i 1)]
              (a/acopy counts i new-counts j i)
              (when-not (== j new-length)
                (recur j))))
          (aset state slot-counts new-counts)
          (recur))
        (loop [i (unchecked-subtract-int degree grow)]
          (if (< i degree)
            (let [inner (object-array inner-slots)]
              (aset buffer i inner)
              (aset inner inner-slot-index (identity i))
              (recur (unchecked-inc-int i)))
            (aset counts 0 (unchecked-subtract-int degree shrink))))))))

(defn swap-buffer [^objects buffer i j]
  (let [xi ^objects (aget buffer i)
        xj ^objects (aget buffer j)]
    (aset xi inner-slot-index j)
    (aset xj inner-slot-index i)
    (aset buffer i xj)
    (aset buffer j xi)))

(defn transfer [^objects state]
  ((locking state
     (try
       (loop [i 0]
         (if (aget state slot-busy)
           (do (aset state slot-busy false)
               (let [{:keys [grow degree shrink permutation change]} @(aget state slot-process)]
                 (ensure-capacity state grow degree shrink)
                 (let [^objects buffer (aget state slot-buffer)
                       ^ints counts (aget state slot-counts)
                       global-degree (aget counts 1)
                       perm (loop [p permutation
                                   q {}]
                              (case p
                                {} (reduce
                                     (fn [q index]
                                       (let [^objects inner (aget buffer index)
                                             ^objects inner (if-some [ps (aget inner inner-slot-process)]
                                                              (let [clone (object-array inner-slots)]
                                                                (aset clone inner-slot-index index)
                                                                (aset inner inner-slot-index nil)
                                                                (aset buffer index clone)
                                                                (ps) clone) inner)]
                                         (aset state slot-live (inc (aget state slot-live)))
                                         (aset inner inner-slot-process
                                           ((change index) #(inner-ready state inner) #(terminated state)))
                                         (let [k (index-in-counts counts index)
                                               l (aget counts k)
                                               o (compute-offset counts k 0)
                                               s (aget counts 1)]
                                           (p/compose
                                             (->> (range o (unchecked-add-int o l))
                                               (eduction (map (fn [i] (p/cycle i (unchecked-add-int s i)))))
                                               (reduce p/compose {})) q))))
                                     q (sort (keys change)))
                                (let [[i j] (first p)
                                      k2 (index-in-counts counts (max i j))
                                      k1 (index-in-counts counts (min i j))
                                      l2 (aget counts k2)
                                      l1 (aget counts k1)
                                      o2 (compute-offset counts k2 l1)
                                      o1 (compute-offset counts k1 l2)]
                                  (swap-buffer buffer i j)
                                  (recur (p/compose p (p/cycle i j))
                                    (p/compose (p/split-long-swap o1 l1
                                                 (unchecked-subtract-int
                                                   (unchecked-subtract-int o2 o1)
                                                   l1) l2) q)))))]
                   (dotimes [i shrink]
                     (let [index (unchecked-dec-int (unchecked-subtract-int degree i))
                           ^objects inner (aget buffer index)]
                       (aset buffer index nil)
                       (aset inner inner-slot-index nil)
                       ((aget inner inner-slot-process))
                       (compute-offset counts (index-in-counts counts index) 0)))
                   (aset state slot-value
                     (d/combine (aget state slot-value)
                       {:grow        0
                        :degree      global-degree
                        :permutation perm
                        :shrink      (unchecked-subtract global-degree (aget counts 1))
                        :change      {}
                        :freeze      #{}}))))
               (recur i))
           (let [^objects q (aget state slot-ready)]
             (if-some [^objects input (aget q i)]
               (do (aset q i nil)
                   (if-some [index (aget input inner-slot-index)]
                     (let [{:keys [degree shrink] :as d} @(aget input inner-slot-process)
                           ^ints counts (aget state slot-counts)]
                       (aset state slot-value
                         (d/combine (aget state slot-value)
                           (d/subdiff d (aget counts 1)
                             (compute-offset counts (index-in-counts counts index)
                               (unchecked-subtract-int degree shrink))))))
                     (try @(aget input inner-slot-process)
                          (catch #?(:clj Throwable :cljs :default) _)))
                   (recur (rem (unchecked-inc-int i) (alength q))))
               (do (aset state slot-push nil)
                   (if (zero? (aget state slot-live))
                     (aget state slot-terminator) nop))))))
       (catch #?(:clj Throwable :cljs :default) e
         (aset state slot-notifier nil)
         (aset state slot-value e)
         (cancel state)
         (drain state)))))
  (let [x (aget state slot-value)]
    (aset state slot-value (d/empty-diff (aget ^ints (aget state slot-counts) 1)))
    (if (nil? (aget state slot-notifier)) (throw x) x)))

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn flow [input]
  (fn [n t]
    (let [state (object-array slots)]
      (aset state slot-notifier n)
      (aset state slot-terminator t)
      (aset state slot-buffer (object-array 1))
      (aset state slot-counts (a/int-array 2))
      (aset state slot-ready (object-array 1))
      (aset state slot-live (identity 1))
      (aset state slot-busy false)
      (aset state slot-value (d/empty-diff 0))
      (aset state slot-process (input #(outer-ready state) #(terminated state)))
      (->Ps state))))