(ns hyperfiddle.incseq.items-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.test :refer [is]])
  (:import #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.util.concurrent.locks ReentrantLock))
           missionary.Cancelled))

(def slot-lock 0)
(def slot-busy 1)
(def slot-done 2)
(def slot-buffer 3)
(def slot-input 4)
(def slot-output 5)
(def slot-diff 6)
(def slot-head-step 7)
(def slot-head-done 8)
(def slots 9)

(def item-slot-parent 0)
(def item-slot-frozen 1)
(def item-slot-state 2)
(def item-slot-current 3)
(def item-slots 4)

(declare item-cancel item-transfer)

(deftype Item [state step done next]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (item-cancel this))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (item-transfer this)))

(declare cancel transfer)

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn get-next [^Item item]
  (aget ^objects (.-next item) 0))

(defn set-next [^Item item x]
  (aset ^objects (.-next item) 0 x))

(defn acquire [^objects state]
  #?(:clj  (let [^ReentrantLock lock (aget state slot-lock)
                 held (.isHeldByCurrentThread lock)]
             (.lock lock) held)
     :cljs (let [held (aget state slot-lock)]
             (aset state slot-lock true) held)))

(defn release [^objects state held]
  (if held
    #?(:clj  (.unlock ^ReentrantLock (aget state slot-lock))
       :cljs (aset state slot-lock held))
    (let [^objects head-step (aget state slot-head-step)
          ^objects head-done (aget state slot-head-done)]
      (aset state slot-head-step nil)
      (aset state slot-head-done nil)
      #?(:clj  (.unlock ^ReentrantLock (aget state slot-lock))
         :cljs (aset state slot-lock held))
      (loop [^Item head head-step]
        (when-not (nil? head)
          (let [item (get-next head)]
            (set-next head nil)
            ((.-step head))
            (recur item))))
      (loop [^objects head head-done]
        (when-not (nil? head)
          (let [item (get-next head)]
            (set-next head nil)
            ((.-done head))
            (recur item)))))))

(defn ensure-capacity [^objects state n]
  (let [^objects b (aget state slot-buffer)
        l (alength b)]
    (if (< l n)
      (let [a (object-array
                (loop [l l]
                  (let [l (bit-shift-left l 1)]
                    (if (< l n) (recur l) l))))]
        #?(:cljs (dotimes [i l] (aset a i (aget b i)))
           :clj (System/arraycopy b 0 a 0 l))
        (aset state slot-buffer a)) b)))

(defn apply-cycle [^objects buffer cycle]
  (let [i (nth cycle 0)
        x (aget buffer i)
        j (loop [i i
                 k 1]
            (let [j (nth cycle k)
                  y (aget buffer j)
                  k (unchecked-inc-int k)]
              (aset buffer i y)
              (if (< k (count cycle))
                (recur j k) j)))]
    (aset buffer j x) buffer))

(defn propagate-change [^objects buffer i x]
  (aset ^objects (aget buffer i) item-slot-state x) buffer)

(defn propagate-freeze [^objects buffer i]
  (aset ^objects (aget buffer i) item-slot-frozen true) buffer)

(defn detach [^objects buffer i]
  (propagate-freeze buffer i) (aset buffer i nil) buffer)

(defn item-step [^Item item]
  (let [^objects state (.-state item)
        ^objects parent (aget state item-slot-parent)]
    (set-next item (aget parent slot-head-step))
    (aset parent slot-head-step item)))

(defn item-done [^Item item]
  (let [^objects state (.-state item)
        ^objects parent (aget state item-slot-parent)]
    (set-next item (aget parent slot-head-done))
    (aset parent slot-head-done item)))

(defn get-cursor [^objects state]
  (fn [step done]
    (let [^objects parent (aget state item-slot-parent)
          held (acquire parent)]
      (if (nil? (aget state item-slot-current))
        (let [item (->Item state step done (object-array 1))]
          (aset state item-slot-current item) (item-step item)
          (release parent held) item)
        (do (release parent held)
            (throw (#?(:clj Error. :cljs js/Error.) "Illegal concurrent cursor.")))))))

(defn create-item [^objects parent i]
  (let [state (object-array item-slots)
        ^objects buffer (aget parent slot-buffer)]
    (aset state item-slot-parent parent)
    (aset state item-slot-frozen false)
    (aset buffer i state) parent))

(defn input-transfer [^objects parent]
  (when (aget parent slot-busy)
    (loop []
      (try
        (let [{:keys [grow degree shrink permutation change freeze]} @(aget parent slot-input)
              ^objects buffer (ensure-capacity parent degree)
              created (range (- degree grow) degree)
              iperm (p/inverse permutation)
              indices (into #{} (map (fn [i] (iperm i i))) created)]
          (reduce create-item parent created)
          (reduce apply-cycle buffer (p/decompose permutation))
          (reduce detach buffer (range (- degree shrink) degree))
          (reduce-kv propagate-change buffer change)
          (reduce propagate-freeze buffer freeze)
          (let [diff {:grow        grow
                      :degree      degree
                      :shrink      shrink
                      :permutation permutation
                      :change      (reduce
                                     (fn [m i]
                                       (assoc m i (get-cursor (aget buffer i))))
                                     {} indices)
                      :freeze      indices}]
            (aset parent slot-diff
              (if-some [d (aget parent slot-diff)]
                (d/combine d diff) diff))))
        (catch #?(:clj Throwable :cljs :default) e
          (aset parent slot-input nil)
          (aset parent slot-diff e)))
      (when (aset parent slot-busy (not (aget parent slot-busy))) (recur)))))

(defn enqueue-all [^objects parent head]
  (let [^objects buffer (aget parent slot-buffer)]
    (loop [i 0
           h (let [^Item output (aget parent slot-output)]
               (when (identical? output (get-next output))
                 (set-next output head) output))]
      (if (< i (alength buffer))
        (if-some [^objects state (aget buffer i)]
          (if-some [^Item item (aget state item-slot-current)]
            (recur (inc i)
              (if (identical? item (get-next item))
                (do (set-next item h) item) h)) h) h) h))))

(defn item-cancel [^Item item]
  (let [^objects state (.-state item)
        parent (aget state item-slot-parent)
        held (acquire parent)]
    (when (identical? item (aget state item-slot-current))
      (aset state item-slot-current nil)
      (when (identical? item (get-next item))
        (item-step item)))
    (release parent held)))

(defn item-transfer [^Item item]
  (let [^objects state (.-state item)
        parent (aget state item-slot-parent)
        held (acquire parent)]
    (if (identical? item (aget state item-slot-current))
      (do (input-transfer parent)
          (let [diff (aget state item-slot-state)]
            (if (or (aget state item-slot-frozen) (aget parent slot-done))
              (item-done item) (set-next item item))
            (release parent held) diff))
      (do (item-done item)
          (release parent held)
          (throw (Cancelled. "Cursor cancelled."))))))

(defn cancel [^objects parent]
  (when-some [ps (aget parent slot-input)] (ps)))

(defn transfer [^objects parent]
  (let [held (acquire parent)
        output (aget parent slot-output)]
    (input-transfer parent)
    (let [diff (aget parent slot-diff)]
      (aset parent slot-diff nil)
      (if (aget parent slot-done)
        (do (set-next output (aget parent slot-head-done))
            (aset parent slot-head-done output))
        (set-next output output))
      (if (nil? (aget parent slot-input))
        (do (release parent held) (throw diff))
        (do (release parent held) diff)))))

(defn flow [incseq]
  (fn [step done]
    (let [parent (object-array slots)
          output (->Item parent step done (object-array 1))]
      (set-next output output)
      (aset parent slot-lock #?(:clj (ReentrantLock.) :cljs false))
      (aset parent slot-busy false)
      (aset parent slot-buffer (object-array 1))
      (aset parent slot-output output)
      (aset parent slot-input
        (incseq #(let [held (acquire parent)]
                   (aset parent slot-busy (not (aget parent slot-busy)))
                   (aset parent slot-head-step (enqueue-all parent (aget parent slot-head-step)))
                   (release parent held))
          #(let [held (acquire parent)]
             (aset parent slot-done true)
             (aset parent slot-head-done (enqueue-all parent (aget parent slot-head-done)))
             (release parent held))))
      (->Ps parent))))

(tests
  (let [q #?(:clj (let [q (java.util.LinkedList.)]
                    (fn
                      ([] (.remove q))
                      ([x] (.add q x) nil)))
             :cljs (let [q (make-array 0)]
                     (fn
                       ([]
                        (when (zero? (alength q))
                          (throw (js/Error. "No such element.")))
                        (.shift q))
                       ([x] (.push q x) nil))))
        ps ((flow (fn [step done]
                    (q [step done])
                    (step)
                    (reify
                      IFn
                      (#?(:clj invoke :cljs -invoke) [_]
                        (q :cancel))
                      IDeref
                      (#?(:clj deref :cljs -deref) [_]
                        (q)))))
            #(q :step) #(q :done))
        [step done] (q)
        _ (is (::rcf/= (q) :step))
        _ (q (assoc (d/empty-diff 2)
               :change {0 :foo 1 :bar}
               :grow 2))
        diff @ps
        _ (is (::rcf/= (dissoc diff :change)
                (assoc (dissoc (d/empty-diff 2) :change)
                  :freeze #{0 1}
                  :grow 2)))
        [item0 item1] (map (:change diff) [0 1])
        ps0 (item0 #(q :step0) #(q :done0))
        _ (is (::rcf/= (q) :step0))
        _ (is (::rcf/= @ps0 :foo))
        ps1 (item1 #(q :step1) #(q :done1))
        _ (is (::rcf/= (q) :step1))
        _ (step)
        _ (is (::rcf/= (hash-set (q) (q)) #{:step :step0}))
        _ (q (assoc (d/empty-diff 2)
               :permutation {0 1 1 0}
               :change {1 :foo 0 :BAR}))
        _ (is (::rcf/= @ps (assoc (d/empty-diff 2) :permutation {0 1 1 0})))
        _ (is (::rcf/= @ps1 :BAR))
        _ (is (::rcf/= @ps0 :foo))
        _ (ps0)
        _ (is (::rcf/= (q) :step0))
        ps0- (item0 #(q :step0-) #(q :done0-))
        _ (is (::rcf/= (q) :step0-))
        _ (is (::rcf/= nil (try @ps0 (catch Cancelled _))))
        _ (is (::rcf/= (q) :done0))
        _ (step)
        _ (is (::rcf/= (hash-set (q) (q)) #{:step :step1}))
        _ (q (assoc (d/empty-diff 2)
               :change {1 :FOO}))
        _ (is (::rcf/= @ps0- :FOO))
        _ (is (::rcf/= nil (try (item1 #(q :step1-) #(q :done1-))
                                (catch #?(:clj Error :cljs js/Error) _))))
        _ (step)
        _ (is (::rcf/= (hash-set (q)) #{:step0-}))
        _ (q (assoc (d/empty-diff 2)
               :freeze #{0 1}))
        _ (is (::rcf/= @ps1 :BAR))
        _ (is (::rcf/= (q) :done1))
        _ (is (::rcf/= @ps0- :FOO))
        _ (is (::rcf/= (q) :done0-))
        _ (is (::rcf/= @ps (d/empty-diff 2)))
        _ (done)
        _ (is (::rcf/= (q) :done))]))