(ns hyperfiddle.detest.incseq-test
  (:import #?(:clj [clojure.lang IFn IDeref])
           [missionary Cancelled])
  (:require [hyperfiddle.detest :as dt]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [clojure.set :as set]
            [contrib.assert :as ca]
            [contrib.debug :as dbg]
            [contrib.data :refer [->box]]
            [clojure.test :as t]
            [missionary.core :as m]
            [hyperfiddle.incseq.flow-protocol-enforcer :as fpe]))

(defn pick [ngn v]
  (if (seq v)
    (let [i (dt/roll ngn (count v))]
      [(nth v i) (reduce-kv (fn [ac j x] (if (= i j) ac (conj ac x))) [] v)])
    v))

(defn %next-diff [prev-size ngn next-fn]
  (let [grow (dt/roll ngn 10), degree (+ prev-size grow)
        shrink (dt/roll ngn prev-size), size (- degree shrink)
        to-rot (vec (range (max size prev-size) degree))
        stay-space (vec (range #_size (min size prev-size)))
        perm-vec (loop [to-rot to-rot, stay stay-space, ret []]
                   (if (seq to-rot)
                     (let [[i stay] (pick ngn stay), [j to-rot] (pick ngn to-rot)]
                       (recur to-rot stay (conj ret i j)))
                     ret))
        ;; _ (prn 'perm-vec perm-vec)
        perm (if (seq perm-vec) (p/recompose #{perm-vec}) {})
        ;; perm (p/rotation (dt/roll ngn degree) (dt/roll ngn degree))
        inv (set/map-invert perm)
        ;; _ (prn 'inv inv)
        change (reduce (fn [m i] #_(prn 'i i (inv i i)) (let [i (inv i i)] (cond-> m (< i size) (assoc i (next-fn ngn)))))
                 {} (range prev-size (+ prev-size grow)))
        ;; _ (prn 'change-grown change)
        change (reduce (fn [m i] (cond-> m (zero? (dt/roll ngn 2)) (assoc i (next-fn ngn))))
                 change (range size))]
    {:grow grow :degree degree :shrink shrink :permutation perm :change change :freeze #{}}))

(defn %rand-incseq [ngn next-incseq-fn]
  (cond->> (fn [step done]
             (step)
             (let [!should-step? (atom false), !v (atom (d/empty-diff 0)), !done? (atom false), !cancelled? (atom false)
                   ;; !dbg (atom [])
                   fin #(when-not (first (reset-vals! !done? true)) (done))
                   proc
                   (reify
                     IFn
                     (#?(:clj invoke :cljs -invoke) [_]
                       (let [cancelled? (first (reset-vals! !cancelled? true))]
                         (when (and @!should-step? (not @!done?) (not cancelled?))
                           (swap! !should-step? not) (step))))
                     (#?(:clj invoke :cljs -invoke) [this n]
                       (if @!done?
                         (dt/del-proc ngn this)
                         (if (> 1 (mod n 100))
                           (when @!should-step? (fin))
                           (when (and @!should-step? (not @!done?)) (swap! !should-step? not) (step)))))
                     IDeref
                     (#?(:clj deref :cljs -deref) [_]
                       (cond
                         @!done?
                         (throw (ex-info "transfer after done" {}))

                         @!cancelled?
                         (do (fin) (throw (Cancelled.)))

                         @!should-step?
                         (throw (ex-info "transfer without step" {}))

                         :else
                         (do (swap! !should-step? not)
                             (if (> 1 (dt/roll ngn 100))
                               (do (fin) (throw (ex-info "[DETEST OK] random incseq throw" {})))
                               (do (condp > (dt/roll ngn 100)
                                     1 (fin)
                                     25 (do (swap! !should-step? not) (step))
                                     #_else nil)
                                   (swap! !v next-incseq-fn ngn)
                                   #_(let [nx (swap! !v next-incseq-fn ngn)]
                                       (prn 'state (swap! !dbg i/patch-vec nx))
                                       nx)))))))]
               (dt/add-proc ngn proc)
               proc))
    (dt/debug? ngn) (dt/instrument 'rand-incseq ngn)))

(defn next-diff [prev-diff ngn]
  (%next-diff (- (:degree prev-diff) (:shrink prev-diff)) ngn dt/roll))

(defn rand-incseq [ngn] (%rand-incseq ngn next-diff))

(defn next-lc-diff [prev-diff ngn]
  (%next-diff (- (:degree prev-diff) (:shrink prev-diff)) ngn rand-incseq))

(defn rand-lc-incseq [ngn] (%rand-incseq ngn next-lc-diff))

(t/deftest detest-latest-product
  (dotimes [_ 100]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn (i/latest-product vector (rand-incseq ngn) (rand-incseq ngn))))))))

(t/deftest detest-latest-concat
  (dotimes [_ 500]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn (i/latest-concat (rand-lc-incseq ngn))))))))

(comment
  ;; how to repro and debug print
  (let [ngn (dt/->engine {:seed -6858028806708848032, :debug :full})]
    (dt/exercise ngn (i/latest-concat (rand-lc-incseq ngn))))
  ;; how to minimize when a flow test fails often
  (let [<s> (->box)]
    (dotimes [_ 1000]
      (let [ngn (dt/->engine {:debug :steps})]
        (dt/minimize ngn <s> (i/latest-concat (rand-lc-incseq ngn)))))
    (<s>))
  )

(t/deftest detest-fixed
  (dotimes [_ 1000]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn (i/fixed (rand-incseq ngn) (rand-incseq ngn) (rand-incseq ngn)
                                 (rand-incseq ngn) (rand-incseq ngn) (rand-incseq ngn)
                                 (rand-incseq ngn) (rand-incseq ngn) (rand-incseq ngn))))))))

(t/deftest detest-diff-by
  (dotimes [_ 1000]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn (m/reductions i/patch-vec [] (rand-incseq ngn))))))))

(t/deftest detest-items
  (dotimes [_ 1000]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn (i/items (rand-incseq ngn))))))))
