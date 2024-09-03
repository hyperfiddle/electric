(ns hyperfiddle.incseq.fixed-impl
  (:require [hyperfiddle.incseq.arrays-impl :as a])
  (:import #?(:clj (clojure.lang IFn IDeref))))

(def slot-notifier 0)
(def slot-terminator 1)
(def slot-processes 2)
(def slot-ready 3)
(def slot-push 4)
(def slot-live 5)
(def slot-value 6)
(def slots 7)

(deftype EmptySeq [t]
  IFn
  (#?(:clj invoke :cljs -invoke) [_])
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (t) {:grow 0
         :shrink 0
         :degree 0
         :permutation {}
         :change {}
         :freeze #{}}))

(defn empty-seq [n t]
  (n) (->EmptySeq t))

(defn nop [])

(defn input-ready [^objects state item]
  ((locking state
     (let [^objects processes (aget state slot-processes)
           ^ints ready (aget state slot-ready)
           arity (alength processes)
           item (int item)]
       (if-some [i (aget state slot-push)]
         (do (aset state slot-push (identity (rem (unchecked-inc-int i) arity)))
             (aset ready i item) nop)
         (do (aset state slot-push (identity (rem 1 arity)))
             (if-some [cb (aget state slot-notifier)]
               (do (aset ready 0 item) cb)
               (loop [item item
                      i (rem 1 arity)]
                 (if (neg? item)
                   (aset state slot-live (dec (aget state slot-live)))
                   (try @(aget processes item) (catch #?(:clj Throwable :cljs :default) _)))
                 (let [item (aget ready i)]
                   (if (== arity item)
                     (do (aset state slot-push nil)
                         (if (zero? (aget state slot-live))
                           (aget state slot-terminator) nop))
                     (do (aset ready i arity)
                         (recur item (rem (unchecked-inc-int i) arity)))))))))))))

(defn item-spawn [^objects state item flow]
  (let [^objects processes (aget state slot-processes)
        arity (alength processes)]
    (aset processes item
      (flow #(input-ready state item)
        #(input-ready state (unchecked-subtract-int item arity)))))
  state)

(defn cancel [^objects state]
  (let [^objects processes (aget state slot-processes)]
    (dotimes [item (alength processes)] ((aget processes item)))))

(defn transfer [^objects state]
  (let [^objects processes (aget state slot-processes)
        ^ints ready (aget state slot-ready)
        arity (alength processes)
        item (aget ready 0)]
    (aset ready 0 arity)
    ((locking state
       (loop [item item
              i (rem 1 arity)]
         (if (nil? (aget state slot-notifier))
           (if (neg? item)
             (aset state slot-live (dec (aget state slot-live)))
             (try @(aget processes item) (catch #?(:clj Throwable :cljs :default) _)))
           (let [diff (aget state slot-value)]
             (aset state slot-value
               (if (neg? item)
                 (do (aset state slot-live (dec (aget state slot-live)))
                     (update diff :freeze conj (unchecked-add-int arity item)))
                 (try (update diff :change assoc item @(aget processes item))
                      (catch #?(:clj Throwable :cljs :default) e
                        (aset state slot-notifier nil)
                        (cancel state) e))))))
         (let [item (aget ready i)]
           (if (== arity item)
             (do (aset state slot-push nil)
                 (if (zero? (aget state slot-live))
                   (aget state slot-terminator) nop))
             (do (aset ready i arity)
                 (recur item (rem (unchecked-inc-int i) arity))))))))
    (let [x (aget state slot-value)]
      (aset state slot-value
        {:grow 0
         :shrink 0
         :degree arity
         :permutation {}
         :change {}
         :freeze #{}})
      (if (nil? (aget state slot-notifier))
        (throw x) x))))

(deftype Ps [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

(defn flow
  ([] empty-seq)
  ([item & items]
   (let [items (into [item] items)]
     (fn [n t]
       (let [state (object-array slots)
             arity (count items)
             ready (a/int-array arity)]
         (dotimes [i arity] (aset ready i arity))
         (aset state slot-notifier n)
         (aset state slot-terminator t)
         (aset state slot-processes (object-array arity))
         (aset state slot-ready ready)
         (aset state slot-live (identity arity))
         (aset state slot-value
           {:grow        arity
            :degree      arity
            :shrink      0
            :permutation {}
            :change      {}
            :freeze      #{}})
         (reduce-kv item-spawn state items)
         (->Ps state))))))