(ns hyperfiddle.incseq.latest-concat-impl
  (:require [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.diff-impl :as d])
  #?(:clj (:import (clojure.lang IFn IDeref)
                   (java.util.concurrent.locks ReentrantLock))))

(def slot-step 0)
(def slot-done 1)
(def slot-lock 2)
(def slot-buffer 3)
(def slot-counts 4)
(def slot-queue 5)
(def slot-alive 6)
(def slot-ready 7)
(def slot-pending 8)
(def slot-cancelled 9)
(def slot-process 10)
(def slots 11)

(def inner-slot-state 0)
(def inner-slot-queue 1)
(def inner-slot-index 2)
(def inner-slot-process 3)
(def inner-slots 4)

(defn enter [^objects state]
  #?(:clj (let [^ReentrantLock lock (aget state slot-lock)
                held (.isHeldByCurrentThread lock)]
            (.lock lock) held)
     :cljs (let [held (aget state slot-lock)]
             (aset state slot-lock true) held)))

(defn unlock [^objects state held]
  #?(:clj  (.unlock ^ReentrantLock (aget state slot-lock))
     :cljs (aset state slot-lock held)))

(defn exit [^objects state held]
  (let [step (aget state slot-step)
        done (aget state slot-done)]
    (if (or held (aget state slot-pending))
      (unlock state held)
      (if (or (some? (aget state slot-queue)) (aget state slot-ready))
        (do (aset state slot-pending true)
            (unlock state held) (step))
        (if (zero? (aget state slot-alive))
          (do (unlock state held) (done))
          (unlock state held))))))

(defn outer-ready [^objects state]
  (let [held (enter state)]
    (if (some? (aget state slot-step))
      (aset state slot-ready true)
      (try @(aget state slot-process)
           (catch #?(:clj Throwable :cljs :default) _)))
    (exit state held)))

(defn inner-ready [^objects inner]
  (let [^objects state (aget inner inner-slot-state)
        held (enter state)]
    (if (nil? (aget inner inner-slot-index))
      (try @(aget inner inner-slot-process)
           (catch #?(:clj Throwable :cljs :default) _))
      (do (aset inner inner-slot-queue (aget state slot-queue))
          (aset state slot-queue inner)))
    (exit state held)))

(defn terminated [^objects state]
  (let [held (enter state)]
    (aset state slot-alive (dec (aget state slot-alive)))
    (exit state held)))

(defn cancel [^objects state]
  (let [held (enter state)]
    (when-not (aget state slot-cancelled)
      (aset state slot-cancelled true)
      ((aget state slot-process))
      (let [^objects buffer (aget state slot-buffer)
            ^ints counts (aget state slot-counts)]
        (dotimes [i (aget counts 0)]
          (let [^objects inner (aget buffer i)]
            ((aget inner inner-slot-process))))))
    (exit state held)))

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

(defn make-inner [^objects state index]
  (let [inner (object-array inner-slots)]
    (aset inner inner-slot-state state)
    (aset inner inner-slot-index index)
    (aset inner inner-slot-queue inner)
    inner))

(defn ensure-capacity [^objects state grow degree shrink]
  (loop []
    (let [^ints counts (aget state slot-counts)
          ^objects buffer (aget state slot-buffer)
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
            (do (aset buffer i (make-inner state i))
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
  (let [held (enter state)]
    (try
      (loop [diff (d/empty-diff (aget ^ints (aget state slot-counts) 1))]
        (if (aget state slot-ready)
          (do (aset state slot-ready false)
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
                                      (aset state slot-alive (inc (aget state slot-alive)))
                                      (let [^objects inner (aget buffer index)
                                            ^objects inner (if-some [ps (aget inner inner-slot-process)]
                                                             (do (aset inner inner-slot-index nil) (ps)
                                                                 (aset buffer index (make-inner state index)))
                                                             inner)
                                            ps ((change index) #(inner-ready inner) #(terminated state))]
                                        (aset inner inner-slot-process ps)
                                        (when (aget state slot-cancelled) (ps))
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
                  (recur (d/combine diff
                           {:grow        0
                            :degree      global-degree
                            :permutation perm
                            :shrink      (unchecked-subtract global-degree (aget counts 1))
                            :change      {}
                            :freeze      #{}})))))
          (if-some [^objects inner (aget state slot-queue)]
            (do (aset state slot-queue (aget inner inner-slot-queue))
                (aset inner inner-slot-queue inner)
                (recur (if-some [index (aget inner inner-slot-index)]
                         (let [{:keys [degree shrink] :as d} @(aget inner inner-slot-process)
                               ^ints counts (aget state slot-counts)]
                           (d/combine diff
                             (d/subdiff d (aget counts 1)
                               (compute-offset counts (index-in-counts counts index)
                                 (unchecked-subtract-int degree shrink)))))
                         (do (try @(aget inner inner-slot-process)
                                  (catch #?(:clj Throwable :cljs :default) _))
                             diff))))
            (do (aset state slot-pending false)
                (exit state held) diff))))
      (catch #?(:clj Throwable :cljs :default) e
        (aset state slot-step nil)
        (let [^objects buffer (aget state slot-buffer)
              ^ints counts (aget state slot-counts)]
          (dotimes [i (aget counts 0)]
            (let [^objects inner (aget buffer i)]
              (aset inner inner-slot-index nil)
              ((aget inner inner-slot-process)))))
        (when-not (aget state slot-cancelled)
          (aset state slot-cancelled true)
          ((aget state slot-process)))
        (loop []
          (if (aget state slot-ready)
            (do (aset state slot-ready false)
                (try @(aget state slot-process)
                     (catch #?(:clj Throwable :cljs :default) _))
                (recur))
            (when-some [^objects inner (aget state slot-queue)]
              (aset state slot-queue (aget inner inner-slot-queue))
              (aset inner inner-slot-queue inner)
              (try @(aget inner inner-slot-process)
                   (catch #?(:clj Throwable :cljs :default) _))
              (recur))))
        (aset state slot-pending false)
        (exit state held) (throw e)))))

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn flow [input]
  (fn [step done]
    (let [state (object-array slots)]
      (aset state slot-step step)
      (aset state slot-done done)
      (aset state slot-lock #?(:clj (ReentrantLock.) :cljs false))
      (aset state slot-buffer (object-array 1))
      (aset state slot-counts (a/int-array 2))
      (aset state slot-alive (identity 1))
      (aset state slot-ready false)
      (aset state slot-cancelled false)
      (aset state slot-pending false)
      (aset state slot-process (input #(outer-ready state) #(terminated state)))
      (->Ps state))))