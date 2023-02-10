(ns hyperfiddle.electric.impl.failer
  #?(:clj (:import (clojure.lang IDeref IFn))))

(defn run [e n t]
  (n) (reify
        IFn (#?(:clj invoke :cljs -invoke) [_])
        IDeref (#?(:clj deref :cljs -deref) [_] (t) (throw e))))