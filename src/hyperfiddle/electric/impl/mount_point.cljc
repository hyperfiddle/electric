(ns hyperfiddle.electric.impl.mount-point
  (:require [hyperfiddle.kvs :refer [KVS]]
            [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.electric.impl.runtime-de :as r])
  #?(:clj (:import (clojure.lang IFn IDeref)
                   (java.util.concurrent.locks Lock ReentrantLock))))

(def slot-lock 0)
(def slot-blocks 1)
(def slot-reader 2)
(def slots 3)

(def reader-slot-state 0)
(def reader-slot-step 1)
(def reader-slot-done 2)
(def reader-slot-queue 3)
(def reader-slot-push 4)
(def reader-slot-root 5)
(def reader-slots 6)

(def call-slot-reader 0)
(def call-slot-block 1)
(def call-slot-index 2)
(def call-slot-buffer 3)
(def call-slot-weight 4)
(def call-slot-process 5)
(def call-slots 6)

(def block-slot-parent 0)
(def block-slot-index 1)
(def block-slot-frame 2)
(def block-slot-children 3)
(def block-slot-weights 4)
(def block-slots 5)

(defn enter [^objects state]
  (.lock ^Lock (aget state slot-lock)))

(defn exit [^objects state]
  (.unlock ^Lock (aget state slot-lock)))

(defn frame->block [^objects state frame]
  (get (aget state slot-blocks) frame))

(defn make-block [frame]
  (let [size (r/frame-call-count frame)
        children (object-array size)]
    (dotimes [index (alength children)]
      (when-not (r/frame-call frame index)
        (aset children index children)))
    (doto (object-array block-slots)
      (aset block-slot-frame frame)
      (aset block-slot-children children)
      (aset block-slot-weights (a/weight-tree size)))))

(defn ensure-capacity [^objects buffer cap]
  (let [n (alength buffer)]
    (if (< n cap)
      (let [b (object-array (bit-shift-left n 1))]
        (a/acopy buffer 0 b 0 n) b) buffer)))

(defn call-weight [^objects call]
  (aget call call-slot-weight))

(defn local-block-offset [^objects call index]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)
        ^objects state (aget reader reader-slot-state)]
    (loop [index index
           offset 0]
      (if (zero? index)
        offset
        (let [index (dec index)]
          (recur index
            (if-some [^objects block (frame->block state (aget buffer index))]
              (let [^ints weights (aget block block-slot-weights)]
                (unchecked-add-int offset (aget weights 1)))
              offset)))))))

(defn local-tag-offset [^objects block index]
  (let [^ints weights (aget block block-slot-weights)]
    (loop [o 0, i (unchecked-add (bit-shift-right (alength weights) 1) index)]
      (case i
        1 o
        (recur (if (even? i)
                 o (unchecked-add o
                     (aget weights (unchecked-dec i))))
          (bit-shift-right i 1))))))

(defn tag-offset [^objects block index]
  (loop [^objects block block
         index index
         offset 0]
    (let [offset (unchecked-add-int offset (local-tag-offset block index))]
      (if-some [^objects call (aget block block-slot-parent)]
        (recur (aget call call-slot-block) (aget call call-slot-index)
          (unchecked-add-int offset (local-block-offset call (aget block block-slot-index))))
        offset))))

(defn update-local-weights [^ints weights index delta]
  (loop [i (unchecked-add (bit-shift-right (alength weights) 1) index)]
    (aset weights i (unchecked-add-int (aget weights i) delta))
    (when (< 1 i) (recur (bit-shift-right i 1)))))

(defn update-weights [^objects block index delta]
  (loop [^objects block block]
    (update-local-weights (aget block block-slot-weights) index delta)
    (when-some [^objects call (aget block block-slot-parent)]
      (aset call call-slot-weight (+ delta (aget call call-slot-weight)))
      (recur (aget call call-slot-block)))))

(defn swap-indices [^objects call i j]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)
        ^objects state (aget reader reader-slot-state)
        fi (aget buffer i)
        fj (aget buffer j)]
    (aset buffer i fj)
    (aset buffer j fi)
    (when-some [^objects block (frame->block state fi)]
      (aset block block-slot-index j))
    (when-some [^objects block (frame->block state fj)]
      (aset block block-slot-index i))))

(defn offset-of [^objects call index]
  (unchecked-add-int (local-block-offset call index)
    (tag-offset (aget call call-slot-block)
      (aget call call-slot-index))))

(defn block-weight [^objects block]
  (let [^ints weights (aget block block-slot-weights)]
    (aget weights 1)))

(defn current-size [^objects reader]
  (if-some [root (aget reader reader-slot-root)]
    (block-weight root) 0))

(defn weight-between [^objects call i j]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)
        ^objects state (aget reader reader-slot-state)]
    (loop [i i
           w 0]
      (let [i (unchecked-inc-int i)]
        (if (== i j)
          w (recur i
              (if-some [^objects block (frame->block state (aget buffer i))]
                (unchecked-add-int w (block-weight block)) w)))))))

(defn drain-exit [^objects reader]
  (loop [pull 0]
    (let [^objects queue (aget reader reader-slot-queue)]
      (when-some [^objects block (aget queue pull)]
        (let [pull+ (unchecked-inc-int pull)
              index (aget queue pull+)
              frame (aget block block-slot-frame)
              ^objects children (aget block block-slot-children)]
          (when (r/frame-call frame index)
            (let [^objects call (aget children index)]
              (try @(aget call call-slot-process)
                   (catch #?(:clj Throwable :cljs :default) _))))
          (recur (rem (unchecked-inc-int pull+) (alength queue)))))))
  (aset reader reader-slot-push nil)
  (exit (aget reader reader-slot-state)))

(defn enqueue [^objects reader ^objects block index]
  (let [^objects queue (aget reader reader-slot-queue)
        cap (alength queue)]
    (if-some [i (aget reader reader-slot-push)]
      (do (aset reader reader-slot-push
            (identity
              (if (nil? (aget queue i))
                (let [i+ (unchecked-inc-int i)]
                  (aset queue i block)
                  (aset queue i+ index)
                  (rem (unchecked-inc-int i+) cap))
                (let [n (bit-shift-left cap 1)
                      q (object-array n)]
                  (aset reader reader-slot-queue q)
                  (a/acopy queue i q i
                    (unchecked-subtract-int cap i))
                  (a/acopy queue 0 q cap i)
                  (let [i (unchecked-add-int i cap)
                        i+ (unchecked-inc-int i)]
                    (aset q i block)
                    (aset q i+ index)
                    (rem (unchecked-inc-int i+) n))))))
          false)
      (do (aset reader reader-slot-push (identity (rem 2 cap)))
          (aset queue 0 block)
          (aset queue 1 index)
          true))))

(defn step-exit [^objects reader]
  (let [step (aget reader reader-slot-step)]
    (exit (aget reader reader-slot-state)) (step)))

(defn done-exit [^objects reader]
  (let [done (aget reader reader-slot-done)
        live (aset reader reader-slot-root
               (dec (aget reader reader-slot-root)))]
    (exit (aget reader reader-slot-state))
    (when (zero? live) (done))))

(defn mount-block [^objects reader ^objects block]
  (let [frame (aget block block-slot-frame)
        ^objects children (aget block block-slot-children)]
    (dotimes [index (alength children)]
      (if (r/frame-call frame index)
        (when-some [^objects call (aget children index)]
          (aset call call-slot-process
            ((r/flow (r/->Slot frame index))
             #(let [^objects reader (aget call call-slot-reader)
                    ^objects state (aget reader reader-slot-state)]
                (enter state)
                (if (enqueue reader block index)
                  (if (identical? reader (aget state slot-reader))
                    (step-exit reader)
                    (drain-exit state))
                  (exit state)))
             #(let [^objects reader (aget call call-slot-reader)
                    ^objects state (aget reader reader-slot-state)]
                (enter state)
                (if (identical? reader (aget state slot-reader))
                  (do (comment TODO mark as done) (exit state))
                  (done-exit reader))))))
        (when-not (identical? children (aget children index))
          (enqueue reader block (- index (r/frame-call-count frame))))))))

(defn cancel-calls [^objects reader ^objects block]
  (let [^objects state (aget reader reader-slot-state)
        frame (aget block block-slot-frame)
        ^objects children (aget block block-slot-children)
        ^ints weights (aget block block-slot-weights)
        offset (bit-shift-right (alength weights) 1)]
    (dotimes [index (alength children)]
      (update-local-weights weights index
        (- (aget weights (unchecked-add-int offset index))))
      (when (r/frame-call frame index)
        (when-some [^objects call (aget children index)]
          (let [^objects buffer (aget call call-slot-buffer)
                process (aget call call-slot-process)]
            (aset call call-slot-weight 0)
            (aset reader reader-slot-root
              (inc (aget reader reader-slot-root)))
            (loop [i 0]
              (when (< i (alength buffer))
                (when-some [f (aget buffer i)]
                  (when-some [b (frame->block state f)]
                    (cancel-calls reader b)
                    (recur (inc i))))))
            (process)))))))

(defn unmount-block [^objects reader ^objects block]
  (when-some [^objects call (aget block block-slot-parent)]
    (let [delta (unchecked-negate-int (block-weight block))]
      (aset call call-slot-weight (unchecked-add-int (aget call call-slot-weight) delta))
      (update-weights (aget call call-slot-block) (aget call call-slot-index) delta)))
  (cancel-calls reader block))

(defn reader-cancel [^objects reader]
  (let [^objects state (aget reader reader-slot-state)]
    (enter state)
    (if (identical? reader (aget state slot-reader))
      (let [root (aget reader reader-slot-root)]
        (aset reader reader-slot-root (identity 1))
        (when-not (nil? root) (unmount-block reader root))
        (aset state slot-reader nil)
        (if (nil? (aget reader reader-slot-push))
          (do (aset reader reader-slot-push (identity 0))
              (step-exit reader))
          (drain-exit reader)))
      (exit state))))

(defn reader-transfer [^objects reader]
  (let [^objects state (aget reader reader-slot-state)]
    (enter state)
    (if (identical? reader (aget state slot-reader))
      (loop [pull 0
             diff (d/empty-diff (current-size reader))]
        (let [^objects queue (aget reader reader-slot-queue)]
          (if-some [^objects block (a/aget-aset queue pull nil)]
            (let [pull+ (unchecked-inc-int pull)
                  index (a/aget-aset queue pull+ nil)
                  frame (aget block block-slot-frame)
                  children (aget block block-slot-children)
                  size-before (current-size reader)]
              (recur (rem (unchecked-inc-int pull+) (alength queue))
                (if (neg? index)
                  (let [index (+ index (r/frame-call-count frame))]
                    (if (r/frame-call frame index)
                      (assert false)
                      (let [state (aget children index)
                            offset (tag-offset block index)]
                        (update-weights block index 1)
                        (d/combine diff
                          {:grow        1
                           :degree      (inc size-before)
                           :shrink      0
                           :permutation (p/rotation size-before offset)
                           :change      {offset state}
                           :freeze      #{}}))))
                  (if (r/frame-call frame index)
                    (let [^objects call (aget children index)
                          {:keys [degree shrink permutation change]} @(aget call call-slot-process)
                          ^objects buffer (aset call call-slot-buffer (ensure-capacity (aget call call-slot-buffer) degree))
                          perm (loop [p permutation
                                      q {}]
                                 (case p
                                   {} (reduce-kv
                                        (fn [q i f]
                                          (let [p (aget buffer i)
                                                o (offset-of call i)
                                                l (if-some [^objects block (frame->block state p)]
                                                    (do (unmount-block reader block)
                                                        (block-weight block)) 0)
                                                r (-> (current-size reader)
                                                    (unchecked-subtract-int l)
                                                    (unchecked-subtract-int o))]
                                            (aset buffer i f)
                                            (when-some [^objects block (frame->block state f)]
                                              (aset block block-slot-index i)
                                              (mount-block reader block))
                                            (p/compose (p/split-swap o l r) q)))
                                        q change)
                                   (let [[i j] (first p)
                                         k1 (min i j)
                                         k2 (max i j)
                                         r (p/split-long-swap
                                             (offset-of call k1)
                                             (if-some [^objects block (frame->block state (aget buffer k1))]
                                               (block-weight block) 0)
                                             (weight-between call k1 k2)
                                             (if-some [^objects block (frame->block state (aget buffer k2))]
                                               (block-weight block) 0))]
                                     (swap-indices call i j)
                                     (recur (p/compose p (p/cycle i j))
                                       (p/compose r q)))))]
                      (dotimes [i shrink]
                        (let [i (unchecked-subtract degree
                                  (unchecked-inc-int i))
                              f (aget buffer i)]
                          (aset buffer i nil)
                          (when-some [^objects block (frame->block state f)]
                            (unmount-block reader block))))
                      (d/combine diff
                        {:grow        0
                         :degree      size-before
                         :shrink      (unchecked-subtract size-before
                                        (current-size reader))
                         :permutation perm
                         :change      {}
                         :freeze      #{}}))
                    (let [state (aget children index)
                          offset (tag-offset block index)]
                      (d/combine diff
                        (if (identical? state children)
                          (do (update-weights block index -1)
                              {:grow        0
                               :degree      size-before
                               :shrink      1
                               :permutation (p/rotation offset (dec size-before))
                               :change      {}
                               :freeze      #{}})
                          {:grow        0
                           :degree      size-before
                           :shrink      0
                           :permutation {}
                           :change      {offset state}
                           :freeze      #{}})))))))
            (do (aset reader reader-slot-push nil)
                (exit state) diff))))
      (do (done-exit reader)
          (throw (missionary.Cancelled.))))))

(deftype Reader [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (reader-cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (reader-transfer state)))

(defn subtree [root frame]
  (loop [frame frame
         path (list)]
    (if (identical? root frame)
      path (when-some [slot (r/frame-slot frame)]
             (recur (r/slot-frame slot) (conj path slot))))))

(defn store-block [^objects state frame block]
  (aset state slot-blocks (assoc (aget state slot-blocks) frame block)))

(defn make-call [^objects reader ^objects block index]
  (let [^objects children (aget block block-slot-children)]
    (aset children index
      (doto (object-array call-slots)
        (aset call-slot-reader reader)
        (aset call-slot-block block)
        (aset call-slot-index index)
        (aset call-slot-buffer (object-array 1))
        (aset call-slot-weight (identity 0))))))

(defn make-block-and-call [^objects reader ^objects call frame id]
  (let [block (make-block frame)]
    (store-block (aget reader reader-slot-state) frame block)
    (aset block block-slot-parent call)
    (make-call reader block id)))

(defn make-blocks-and-calls [^objects reader ^objects call path]
  (loop [call call
         path path]
    (case path
      [] call
      (let [slot (peek path)]
        (recur (make-block-and-call reader call
                 (r/slot-frame slot) (r/slot-id slot))
          (pop path))))))

(defn insert-block [^objects reader block]
  (let [^objects state (aget reader reader-slot-state)]
    (if-some [root (aget reader reader-slot-root)]
      (loop [^objects root root]
        (let [root-frame (aget root block-slot-frame)]
          (if-some [path (subtree root-frame (aget block block-slot-frame))]
            (aset block block-slot-parent
              (loop [call nil
                     path path]
                (case path
                  [] call
                  (let [slot (peek path)
                        path (pop path)
                        frame (r/slot-frame slot)
                        id (r/slot-id slot)]
                    (if-some [^objects block (frame->block state frame)]
                      (let [^objects children (aget block block-slot-children)]
                        (if-some [^objects call (aget children id)]
                          (recur call path)
                          (make-blocks-and-calls reader (make-call reader block id) path)))
                      (make-blocks-and-calls reader (make-block-and-call reader call frame id) path))))))
            (let [slot (r/frame-slot root-frame)
                  frame (r/slot-frame slot)
                  block (make-block frame)]
              (store-block state frame block)
              (aset reader reader-slot-root block)
              (aset root block-slot-parent
                (make-call reader block (r/slot-id slot)))
              (recur block)))))
      (aset reader reader-slot-root block))
    reader))

(defn reader-spawn [^objects state step done]
  (let [reader (object-array reader-slots)]
    (aset reader reader-slot-state state)
    (aset reader reader-slot-step step)
    (aset reader reader-slot-done done)
    (aset reader reader-slot-queue (object-array 2))
    (aset reader reader-slot-push (identity 0))
    (enter state)
    (when (nil? (aget state slot-reader))
      (aset state slot-reader reader)
      (reduce insert-block reader (vals (aget state slot-blocks)))
      (when-some [root (aget reader reader-slot-root)]
        (mount-block reader root)))
    (exit state) (step)
    (->Reader reader)))

(defn enqueue-exit [^objects state ^objects block index]
  (if-some [^objects reader (aget state slot-reader)]
    (if (enqueue reader block index)
      (step-exit reader)
      (exit state))
    (exit state)))

(defn failure-exit [^objects state e]
  (exit state) (throw e))

(defn error [^String msg]
  (new #?(:clj Error :cljs js/Error) msg))

(deftype MountPoint [^objects state]
  KVS
  (insert! [_ tag init]
    (let [frame (r/tag-frame tag)
          index (r/tag-index tag)]
      (enter state)
      (let [blocks (aget state slot-blocks)]
        (if-some [^objects block (get blocks frame)]
          (let [^objects children (aget block block-slot-children)]
            (if (identical? children (aget children index))
              (do (aset children index init)
                  (enqueue-exit state block (- index (r/frame-call-count frame))))
              (do (exit state)
                  (throw (error "Can't insert - tag already present.")))))
          (let [^objects block (make-block frame)
                ^objects children (aget block block-slot-children)]
            (aset state slot-blocks (assoc blocks frame block))
            (aset children index init)
            (when-some [reader (aget state slot-reader)]
              (insert-block reader block))
            (enqueue-exit state block (- index (r/frame-call-count frame))))))))
  (update! [_ tag f]
    (let [frame (r/tag-frame tag)
          index (r/tag-index tag)]
      (enter state)
      (let [blocks (aget state slot-blocks)]
        (if-some [^objects block (get blocks frame)]
          (let [^objects children (aget block block-slot-children)
                x (aget children index)]
            (if (identical? children x)
              (failure-exit state (error "Can't update - tag is absent."))
              (if (= x (aset children index (f x)))
                (exit state)
                (enqueue-exit state block index))))
          (failure-exit state (error "Can't update - tag is absent."))))))
  (remove! [_ tag]
    (let [frame (r/tag-frame tag)
          index (r/tag-index tag)]
      (enter state)
      (let [blocks (aget state slot-blocks)]
        (if-some [^objects block (get blocks frame)]
          (let [^objects children (aget block block-slot-children)]
            (if (identical? children (aget children index))
              (failure-exit state (error "Can't remove - tag is absent."))
              (do (aset children index children)
                  ;; TODO if block becomes empty, remove from store
                  (enqueue-exit state block index))))
          (failure-exit state (error "Can't remove - tag is absent."))))))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (reader-spawn state step done)))

(defn create []
  (->MountPoint
    (doto (object-array slots)
      (aset slot-lock #?(:clj (ReentrantLock.) :cljs nil)))))

#_
(defn find-block [^objects state frame]
  (when-some [root (aget state slot-root)]
    (loop [^objects root root]
      (when-some [path (subtree root frame)]
        (loop [^objects block root
               path path]
          (case path
            [] block
            (let [slot (peek path)
                  path (pop path)
                  id (r/slot-id slot)
                  frame (r/slot-frame slot)
                  ^objects children (aget block block-slot-children)]
              (when-some [^objects call (aget children id)]
                (when-some [^objects block (get (aget call call-slot-children) frame)]
                  (recur block path))))))))))

#_
(defn remove-block [^objects reader ^objects block index]
  (loop [^objects block block
         index index]
    (let [^objects children (aget block block-slot-children)]
      (aset children index children)
      (when (loop [i 0]
              (if (identical? children (aget children i))
                (let [i (inc i)]
                  (if (< i (alength children))
                    (recur i) true)) false))
        (if (identical? block (aget reader reader-slot-root))
          (aset reader reader-slot-root nil)
          (let [^objects call (aget block block-slot-parent)]
            (when (zero? (count (aset call call-slot-children
                                  (dissoc! (aget call call-slot-children)
                                    (aget block block-slot-frame)))))
              ;; TODO cancel process
              (recur (aget call call-slot-block)
                (aget call call-slot-index)))))))))
