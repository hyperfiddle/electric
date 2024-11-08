(ns hyperfiddle.token-zoo0
  ; mirror bootstrap structure of electric3 for copy/paste
  (:refer-clojure :exclude [fn defn apply letfn for])
  (:require [clojure.core :as cc]
            [hyperfiddle.electric3 :refer [watch Snapshot]]))

(let [->token (cc/fn [!t]
                (cc/fn token
                  ([] (token nil))
                  ([ret] (reset! !t nil) ret)))
      step (cc/fn [!t v on?] (when (on? v) (compare-and-set! !t nil (->token !t))))]
  (hyperfiddle.electric3/defn TokenNofail
    ([v] (TokenNofail v some?))
    ([v on?] (let [!t (atom nil)]
               (step !t v on?)
               (watch !t)))))

(let [->spend-fn  (cc/fn [!spend!] (cc/fn f ([] (f nil)) ([ret] (reset! !spend! nil) ret)))
      step        (cc/fn [!spend! _spend! v on?] (when (on? v) (compare-and-set! !spend! nil (->spend-fn !spend!))))]
  (hyperfiddle.electric3/defn CyclicToken ; todo retry
    ([v]     (CyclicToken v some?))
    ([v on?] (let [!spend! (atom nil), spend! (watch !spend!)] (step !spend! spend! v on?) spend!))))

(let [->spend-fn  (cc/fn [!held] (cc/fn f ([] (f nil)) ([ret] (swap! !held assoc 1 nil) ret)))
      step        (cc/fn [!held v on?]
                    (let [[_ spend! :as held] @!held]
                      (when (and (not spend!) (on? v))
                        (compare-and-set! !held held [v (->spend-fn !held)]))))]
  (hyperfiddle.electric3/defn StampedToken ; todo retry
    ([v] (StampedToken v some?))
    ([v on?] (let [!held (atom [nil nil])] (step !held v on?) (watch !held)))))

(let [->spend-fn (cc/fn [!held] (cc/fn f ([] (f nil)) ([ret] (swap! !held assoc 1 nil) ret)))
      step       (cc/fn [!held _held v on?]
                   (let [[_ next! :as held] @!held]
                     (when (and (not next!) (on? v))
                       (compare-and-set! !held held [v (->spend-fn !held)]))))]
  (hyperfiddle.electric3/defn StampedCyclicToken ; todo retry
    ([v] (StampedCyclicToken v some?))
    ([v on?] (let [!held (atom [nil nil]), held (watch !held)] (step !held held v on?) held))))

(cc/letfn [(->unlatch-fn [!latched?] (cc/fn f ([] (f nil)) ([v] (reset! !latched? false) v)))
           (->latch-fn   [!latched? unlatch!] (cc/fn f ([] (reset! !latched? unlatch!)) ([_] (f))))]
  (hyperfiddle.electric3/defn Latchable [v]
    (let [!latched? (atom false), unlatch! (->unlatch-fn !latched?)]
      [(if (watch !latched?) (Snapshot v) v)  (->latch-fn !latched? unlatch!)])))