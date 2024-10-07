(ns hyperfiddle.detest.incseq-test
  (:import #?(:clj [clojure.lang IFn IDeref])
           [missionary Cancelled])
  (:require [hyperfiddle.detest :as dt]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [clojure.set :as set]
            [contrib.debug :as dbg]
            [clojure.test :as t]))

(defn %next-diff [prev-degree ngn next-fn]
  (let [grow (dt/roll ngn 10), degree (+ prev-degree grow)
        shrink (dt/roll ngn degree)
        ;; TODO if shrank ensure grown items are permuted so they stay
        perm (p/rotation (dt/roll ngn degree) (dt/roll ngn degree))
        inv (set/map-invert perm)
        change (reduce (fn [m i] (assoc m (inv i i) (next-fn ngn))) {} (range prev-degree (+ prev-degree grow)))
        change (reduce (fn [m i] (cond-> m (zero? (dt/roll ngn 2)) (assoc i (next-fn ngn))))
                 change (range (- degree shrink)))]
    {:grow grow :degree degree :shrink shrink :permutation perm :change change :freeze #{}}))

(defn %rand-incseq [ngn next-incseq-fn]
  (fn [step done]
    (step)
    (let [!should-step? (atom false), !v (atom (d/empty-diff 0)), !done? (atom false), !cancelled? (atom false)
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
                          (swap! !v next-incseq-fn ngn)))))))]
      (dt/add-proc ngn proc)
      proc)))

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
              (dt/exercise ngn {} (i/latest-product vector (rand-incseq ngn) (rand-incseq ngn))))))))

(t/deftest detest-latest-concat
  (dotimes [_ 500]
    (let [ngn (dt/->engine)]
      (t/is (nil?
              (dt/exercise ngn {} (i/latest-concat (rand-lc-incseq ngn))))))))
