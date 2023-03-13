(ns hyperfiddle.electric.impl.yield2
  #?(:cljs (:require-macros hyperfiddle.electric.impl.yield2))
  (:import [missionary Cancelled]
           #?(:clj [clojure.lang IFn IDeref ExceptionInfo]))
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [hyperfiddle.electric.impl.array-fields :as a]
            [missionary.core :as m]))
;; #?(:clj (set! *warn-on-reflection* true))
(declare cancel transfer)
(a/deffields input recover children last-in last-out) ; Yield's array fields
(deftype Yield [checker notifier terminator state-]
  IFn (#?(:clj invoke :cljs -invoke) [this] (locking this (cancel this)))
  IDeref (#?(:clj deref :cljs -deref) [this] (locking this (transfer this))))
(a/deffields iterator notified? on-notify) ; a child's array fields
(defn input-notified [^Yield Y]
  (when-not (or (a/getset (a/fget Y input) notified? true) (some-> (a/fget Y recover) (a/get notified?)))
    ((.-notifier Y))))
(defn recover-notified [^Yield Y]
  (when-not (or (a/getset (a/fget Y recover) notified? true) (a/get (a/fget Y input) notified?))
    ((.-notifier Y))))
(defn terminated [^Yield Y] (when (zero? (a/fswap Y children dec)) ((.-terminator Y))))
(defn swallow [o] (try @(a/get o iterator) (catch #?(:clj Throwable :cljs :default) _)))
(defn trash [o] (a/set o on-notify #(swallow o)) ((a/get o iterator)) (when (a/getset o notified? false) (swallow o)))
(defn cancel [^Yield Y] ((a/get (a/fget Y input) iterator)) (when-some [rec (a/fget Y recover)] (trash rec)))
(defn create-recover [^Yield Y >r]
  (when-some [rec (a/fget Y recover)] (trash rec))
  (a/fswap Y children inc)
  (let [me (a/fset Y recover (object-array 3))]
    (a/set me on-notify #(a/set me notified? true), iterator (>r #((a/get me on-notify)) #(terminated Y)))))
(defn transfer-loop [o] (a/set o notified? false) (let [v @(a/get o iterator)] (if (a/get o notified?) (recur o) v)))
(defn transfer-recover [^Yield Y] (a/fset Y last-out (transfer-loop (a/fget Y recover))))
(defn transfer-input [^Yield Y]
  (let [in (transfer-loop (a/fget Y input))]
    (if (= in (a/fget Y last-in))
      (if (some-> (a/fget Y recover) (a/get notified?))
        (transfer-recover Y)
        (a/fget Y last-out))
      (if-some [>recover ((.-checker Y) in)]
        (let [out (do (create-recover Y >recover) (transfer-recover Y))]
          (a/set (a/fget Y recover) on-notify #(recover-notified Y))
          (a/fset Y last-in in, last-out out))
        (do (a/fset Y last-in ::none) (when-some [rec (a/fget Y recover)] (trash rec))  in)))))
(defn transfer [^Yield Y]
  (try (a/fswap Y children inc)
       (cond (a/get (a/fget Y input)   notified?) (transfer-input Y)
             (a/get (a/fget Y recover) notified?) (transfer-recover Y)
             :else (throw (ex-info "You cannot transfer a value if I haven't notified you" {})))
       (catch #?(:clj Throwable :cljs :default) e
         (trash (a/fget Y input)) (when-some [rec (a/fget Y recover)] (trash rec)) (throw e))
       (finally (terminated Y))))
(defn yield [checker >input]
  (fn [n t]
    (let [^Yield Y (->Yield checker n t (object-array 5))
          me (a/fset Y children 1, last-in ::none, input (object-array 3))]
      (a/set me on-notify #(input-notified Y), iterator (>input #((a/get me on-notify)) #(terminated Y)))
      Y)))
;;; TESTS ;;;
(tests "input flow with nil checker is noop"
  (def !x (atom 0))
  (def it ((yield (constantly nil) (m/watch !x))
           #(tap :notified) #(tap :terminated)))
  #_start         % := :notified, @it := 0
  (swap! !x inc)  % := :notified, @it := 1
  (swap! !x inc)  % := :notified, @it := 2
  (it)            % := :notified, @it :throws Cancelled, % := :terminated)
(tests "input flow runs recovery"
  (def !x (atom 0))
  (def !recover (atom 10))
  (def it ((yield (constantly (m/watch !recover)) (m/watch !x))  #(tap :notified) #(tap :terminated)))
  #_start               % := :notified, @it := 10
  (swap! !x inc)        % := :notified, @it := 10
  (swap! !recover inc)  % := :notified, @it := 11
  (it)                  % := :notified, @it :throws Cancelled, % := :terminated)
(tests "oscillate"
  (def !e (atom "odd"))
  (def !x (atom 0))
  (def it ((yield (fn [x] (when (odd? x) (m/watch !e))) (m/watch !x))  #(do) #(do)))
  #_start            @it := 0
  (swap! !x inc)     @it := "odd"
  (reset! !e "ODD")  @it := "ODD"
  (swap! !x inc)     @it := 2
  (it))
(tests "work skipping"
  (def !e (atom "odd"))
  (def !x (atom 0))
  (def it ((yield (fn [x] (tap x) (when (odd? x) (m/watch !e))) (m/watch !x))  #(do) #(do)))
  #_start             @it := 0,     % := 0
  (swap! !x inc)      @it := "odd", % := 1
  (swap! !x identity) @it := "odd"    ; nothing tapped, work skipped
  (reset! !e "ODD")   @it := "ODD"    ; recovery flow is still alive
  (swap! !x inc)      @it := 2,     % := 2
  (it))
(tests "work skipping, initially in recovery"
  (def !e (atom "odd"))
  (def !x (atom 1))
  (def it ((yield (fn [x] (tap x) (when (odd? x) (m/watch !e))) (m/watch !x))  #(do) #(do)))
  #_start             @it := "odd", % := 1
  (swap! !x identity) @it := "odd"    ; nothing tapped, work skipped
  (reset! !e "ODD")   @it := "ODD"    ; recovery flow is still alive
  (swap! !x inc)      @it := 2,     % := 2
  (it))
(tests "work skipping m/cp"
  (def !x (atom 1))
  (def it ((yield (fn [x] (tap x) (when (odd? x) (m/cp "odd"))) (m/watch !x))  #(do) #(do)))
  #_start             @it := "odd", % := 1
  (swap! !x identity) @it := "odd"    ; nothing tapped, work skipped
  (swap! !x inc)      @it := 2,     % := 2
  (it))
(tests "recovery flows are cleaned up"
  (def !x (atom 0))
  (let [->recover (m/observe (fn [!] (! :init) #(tap :unmounted)))]
    (def it ((yield (fn [x] (when (pos? x) ->recover)) (m/watch !x))  #(do) #(tap :terminated))))
  #_start         @it := 0
  (swap! !x inc)  @it := :init                           ; first recovery starts
  (swap! !x inc)  @it := :init, % := :unmounted          ; second starts, first unmounts
  (it)            @it :throws Cancelled, % := :unmounted, % := :terminated)
(tests "an immediately ready input works"
  (def it ((yield (fn [x] (when (pos? x) (m/cp :recover))) (m/seed [0 1 2]))  #(do) #(tap :terminated)))
  ;; 0 and 1 are not seen because of the consecutive transfer
  @it := :recover, (it), % := :terminated)
(tests "an immediately ready recovery works"
  (def it ((yield (fn [_] (m/seed [0 1 2])) (m/cp :hi)) #(do) #(tap :terminated)))
  ;; 0 and 1 are not seen because of the consecutive transfer
  @it := 2, (it), % := :terminated)
(tests "input throws"
  (def !x (atom 0))
  (def it ((yield (constantly nil) (m/latest #(if (pos? %) (throw (ex-info "pos" {})) %) (m/watch !x)))
           #(tap :notified) #(tap :terminated)))
                 % := :notified, @it := 0
  (swap! !x inc) % := :notified, @it :throws ExceptionInfo, % := :terminated)
(tests "recovery throws"
  (def !x (atom 0))
  (def it ((yield (fn [x] (when (pos? x) (m/cp (throw (ex-info "boom" {}))))) (m/watch !x))
           #(tap :notified) #(tap :terminated)))
  #_start         % := :notified, @it := 0
  (swap! !x inc), % := :notified, @it :throws ExceptionInfo, % := :terminated)
(tests "recovery unmounts when we switch back to input"
  (def !x (atom 0))
  (let [->recover (m/observe (fn [!] (! :init) #(tap :unmounted)))]
    (def it ((yield (fn [x] (when (odd? x) ->recover)) (m/watch !x))  #(do) #(tap :terminated))))
  #_start         @it := 0
  (swap! !x inc)  @it := :init              ; recovery starts
  (swap! !x inc)  @it := 2, % := :unmounted ; back to input, recovery stops
  (it)            @it :throws Cancelled, % := :terminated)
(tests "work skipping is invalidated after successful input"
  (def !x (atom 0))
  (def it ((yield (fn [x] (when (odd? x) (tap :recover) (m/cp :recover))) (m/watch !x))  #(do) #(tap :terminated)))
  #_start         @it := 0
  (swap! !x inc)  @it := :recover, % := :recover
  (swap! !x inc)  @it := 2
  "same input but good input in between, so won't work skip"
  (swap! !x dec)  @it := :recover, % := :recover
  (it)            @it :throws Cancelled, % := :terminated)
(tests "initial nil isn't work skipped"
  (def it ((yield (fn [_] (tap :recover) nil) (m/cp nil))  #(do) #(do)))
  @it := nil, % := :recover)
(tests "cache updates on recover values"
  (def !in (atom 0))
  (def !x (atom 0))
  (def it ((yield (fn [_] (m/watch !x)) (m/watch !in)) #(do) #(do)))
  #_start              @it := 0
  (swap! !x inc)       @it := 1
  (swap! !in identity) @it := 1)
(tests "yield stays alive as long as the recover is alive"
  (def !x (atom 0))
  (def it ((yield (fn [_] (m/watch !x)) (m/cp)) #(tap :notified) #(tap :terminated)))
  #_start        % := :notified, @it := 0
  (swap! !x inc) % := :notified, @it := 1)
(tests "if same input arrives and recover notified recover is re-transferred"
  (def !x (atom 0))
  (def !err (atom 100))
  (def it ((yield (fn [_] (m/watch !err)) (m/watch !x)) #(tap :notified) #(tap :terminated)))
  #_start                              % := :notified, @it := 100
  (swap! !err inc) (swap! !x identity) % := :notified, @it := 101
  (swap! !err inc)                     % := :notified, @it := 102)
