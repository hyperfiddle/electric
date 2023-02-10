(ns hyperfiddle.electric.impl.eventually
  #?(:clj (:import (clojure.lang IFn IDeref))))

(deftype It [final notifier terminator ^objects state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    ((aget state (int 0))))
  IDeref
  (#?(:clj deref :cljs -deref) [it]
    (locking it
      (if (nil? (aget state (int 1)))
        (do (terminator) final)
        (try (aset state (int 1) nil)
             (let [x @(aget state (int 0))]
               (if (nil? (aget state (int 1)))
                 (aset state (int 1) notifier)
                 (do (aset state (int 1) nil)
                     (notifier))) x)
             (catch #?(:clj Throwable :cljs :default) e
               (if (nil? (aget state (int 1)))
                 (aset state (int 1) terminator)
                 (do (aset state (int 1) nil)
                     (terminator))) (throw e)))))))

(defn eventually "
Returns a flow producing successive values of given flow, followed by given value if it terminates successfully.
" [x f]
  (fn [n t]
    (let [state (object-array 2)
          it (->It x n t state)]
      (locking it
        (aset state (int 1) n)
        (aset state (int 0)
          (f n #(locking it
                  (if-some [cb (aget state (int 1))]
                    (do (aset state (int 1) nil) (cb))
                    (aset state (int 1) it))))) it))))