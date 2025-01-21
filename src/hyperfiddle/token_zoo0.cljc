(ns hyperfiddle.token-zoo0
                                        ; mirror bootstrap structure of electric3 for copy/paste
  (:require [hyperfiddle.electric3 :as e]))

(let [->token (fn [!t]
                (fn token
                  ([] (token nil))
                  ([ret] (reset! !t nil) ret)))
      step (fn [!t v on?] (when (on? v) (compare-and-set! !t nil (->token !t))))]
  (hyperfiddle.electric3/defn TokenNofail
    ([v] (TokenNofail v some?))
    ([v on?] (let [!t (atom nil)]
               (step !t v on?)
               (e/watch !t)))))

(let [->spend-fn  (fn [!spend!] (fn f ([] (f nil)) ([ret] (reset! !spend! nil) ret)))
      step        (fn [!spend! _spend! v on?] (when (on? v) (compare-and-set! !spend! nil (->spend-fn !spend!))))]
  (hyperfiddle.electric3/defn CyclicToken ; todo retry
    ([v]     (CyclicToken v some?))
    ([v on?] (let [!spend! (atom nil), spend! (e/watch !spend!)] (step !spend! spend! v on?) spend!))))

(let [->spend-fn  (fn [!held] (fn f ([] (f nil)) ([ret] (swap! !held assoc 1 nil) ret)))
      step        (fn [!held v on?]
                    (let [[_ spend! :as held] @!held]
                      (when (and (not spend!) (on? v))
                        (compare-and-set! !held held [v (->spend-fn !held)]))))]
  (hyperfiddle.electric3/defn StampedToken ; todo retry
    ([v] (StampedToken v some?))
    ([v on?] (let [!held (atom [nil nil])] (step !held v on?) (e/watch !held)))))

(let [->spend-fn (fn [!held] (fn f ([] (f nil)) ([ret] (swap! !held assoc 1 nil) ret)))
      step       (fn [!held _held v on?]
                   (let [[_ next! :as held] @!held]
                     (when (and (not next!) (on? v))
                       (compare-and-set! !held held [v (->spend-fn !held)]))))]
  (hyperfiddle.electric3/defn StampedCyclicToken ; todo retry
    ([v] (StampedCyclicToken v some?))
    ([v on?] (let [!held (atom [nil nil]), held (e/watch !held)] (step !held held v on?) held))))

(letfn [(->unlatch-fn [!latched?] (fn f ([] (f nil)) ([v] (reset! !latched? false) v)))
        (->latch-fn   [!latched? unlatch!] (fn f ([] (reset! !latched? unlatch!)) ([_] (f))))]
  (hyperfiddle.electric3/defn Latchable [v]
    (let [!latched? (atom false), unlatch! (->unlatch-fn !latched?)]
      [(if (e/watch !latched?) (e/Snapshot v) v)  (->latch-fn !latched? unlatch!)])))

(let [->spend-fn (fn [t !d] (when t (fn t2 ([] (t2 nil)) ([d] (reset! !d d) (t d)))))]
  (e/defn WithDataSlot
    ([t] (WithDataSlot t nil))
    ([t init-v]
     (let [!d (atom init-v)]
       [(->spend-fn t !d) (e/watch !d)]))))
