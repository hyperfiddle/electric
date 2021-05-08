(ns hfdl.impl.runtime
  (:require [hfdl.impl.util :as u])
  #?(:clj (:import (clojure.lang IFn IDeref)))
  #?(:cljs (:require-macros [hfdl.impl.runtime :refer [with-ctx get-ctx]])))

(def context #?(:clj (ThreadLocal.)))

#?(:clj
   (defmacro get-ctx []
     (if (:js-globals &env)
       `context
       `(.get ~(with-meta `context {:tag `ThreadLocal})))))

#?(:clj
   (defmacro set-ctx [c]
     (if (:js-globals &env)
       `(set! context ~c)
       `(.set ~(with-meta `context {:tag `ThreadLocal}) ~c))))

#?(:clj
   (defmacro with-ctx [ctx & body]
     `(let [ctx# (get-ctx)]
        (set-ctx ~ctx)
        (try ~@body (finally (set-ctx ctx#))))))

(deftype ContextBound [context target]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (with-ctx context (target)))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (with-ctx context @target)))

(defn bind-context [ctx flow]
  (fn [n t] (->ContextBound ctx (flow (->ContextBound ctx n) (->ContextBound ctx t)))))

(defn no-context [d n t]
  ((u/failer (ex-info "Unable to find dafaflow context" {:dataflow d})) n t))

(defrecord Dataflow [expression statements]
  IFn (#?(:clj invoke :cljs -invoke) [d n t]
        ((or (get-ctx) no-context) d n t)))