(ns hfdl.impl.util
  (:require [missionary.core :as m]
            #?(:clj [minitest :refer [tests]]))
  #?(:clj
     (:import (java.util.concurrent.atomic AtomicReference AtomicInteger)
              (clojure.lang IFn IDeref Box)))
  #?(:cljs (:require-macros [hfdl.impl.util :refer [when-fail]]
                            [minitest :refer [tests]])))

(defn nop [])

(defn pure [x]
  (fn [n t]
    (n) (reify
          IFn (#?(:clj invoke :cljs -invoke) [_])
          IDeref (#?(:clj deref :cljs -deref) [_] (t) x))))

(defn failer [e]
  (fn [n t]
    (n) (reify
          IFn (#?(:clj invoke :cljs -invoke) [_])
          IDeref (#?(:clj deref :cljs -deref) [_] (t) (throw e)))))

(def outof (partial reduce disj))
(def map-into (partial mapv into))
(def swap (juxt second first))

(defn monoid [f i]
  (fn
    ([] i)
    ([x] x)
    ([x y] (f x y))
    ([x y & zs] (reduce f (f x y) zs))))

(defn call
  ([f] (f))
  ([f a] (f a))
  ([f a b] (f a b))
  ([f a b c] (f a b c))
  ([f a b c & ds] (apply f a b c ds)))

#?(:clj
   (defmacro aget-aset [arr idx val]
     `(let [a# ~arr
            i# ~idx
            x# (aget a# i#)]
        (aset a# i# ~val) x#)))

#?(:clj
   (defmacro get-set [obj mem val]
     (let [s (symbol (str ".-" (name mem)))]
       `(let [o# ~obj
              x# (~s o#)]
          (set! (~s o#) ~val) x#))))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (js/console.error e)))

#?(:clj
   (defmacro when-fail [[s e] & body]
     `(try ~e (catch ~(if (:js-globals &env) :default 'Throwable) ~s ~@body))))

(deftype FailureLogger [it]
  IFn
  (#?(:clj invoke :cljs -invoke) [_] (it))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (when-fail [e @it]
      (pst e) (throw e))))

(defn log-failure [f]
  (comp ->FailureLogger f))

(defn log-args [f & prefix]
  (fn [& args] (apply prn (concat prefix args)) (apply f args)))

(defn slot-changes [slots]
  (m/ap
    (let [[path slot] (m/?= (m/seed (m/?= slots)))]
      {path (when-fail [_ (m/?> slot)] ::cancelled)})))

(defn foreach [f input]
  (m/ap (m/? (f (m/?> input)))))

(defn poll [task]
  ; takes a task and returns a flow that runs the task repeatedly
  (m/ap (m/? (m/?> (m/seed (repeat task))))))
