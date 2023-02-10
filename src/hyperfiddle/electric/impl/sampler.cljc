(ns hyperfiddle.electric.impl.sampler
  #?(:clj (:import (clojure.lang IFn IDeref)))
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.sampler :refer [lock]])))

#?(:clj
   (defmacro lock [& args]
     (cons (if (:js-globals &env) `do `locking) args)))

(deftype Sampler [deref]
  IDeref
  (#?(:clj deref :cljs -deref) [this] (deref this)))

; flow is a continuous flow
; repl operator with two features
;   as lazy as the original flow, or maybe the lazyness is controlled
;     you control when the flow is actually sampled
;   memoized, so don't need to remember if flow is ready
(defn sampler! [cb flow]
  (let [memo (int 0)
        iter (int 1)
        slots (object-array 2)
        sampler (->Sampler
                  #(lock %
                     (let [x (aget slots memo)]
                       (if (identical? x slots)
                         (loop []
                           (aset slots memo nil)
                           (let [x @(aget slots iter)]
                             (if (identical? slots (aget slots memo))
                               (recur) (aset slots memo x))))
                         x))))]
    (aset slots iter (flow #(lock sampler
                              (if (identical? slots (aget slots memo))
                                (cb sampler) (aset slots memo slots))) #()))
    (lock sampler
      (if (identical? slots (aget slots memo))
        (cb sampler) (aset slots memo slots))) sampler))