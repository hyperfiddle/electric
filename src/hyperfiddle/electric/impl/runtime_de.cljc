(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.electric.impl.local :as l]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref))))

(def peer-slot-step 0)
(def peer-slot-done 1)
(def peer-slot-defs 2)
(def peer-slot-tier 3)
(def peer-slot-input 4)
(def peer-slot-store 5)
(def peer-slots 6)

(defmacro defs [& forms]
  `(fn [i#] (case i# ~@(interleave (range) forms))))

(defn pure [form]
  (i/fixed (m/cp form)))

(defn static [form]
  (pure form))

(defn error [^String msg]
  #?(:clj (Error. msg)
     :cljs (js/Error. msg)))

(deftype Failer [done e]
  IFn
  (#?(:clj invoke :cljs -invoke) [_])
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (done) (throw e)))

(deftype Unbound [k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (step) (->Failer done (error (str "Unbound electric var lookup - " k)))))

(def current (l/local))

(deftype Tier [parent env])

(defn lookup [k]
  (let [^objects peer (l/get-local current)]
    (loop [^Tier tier (aget peer peer-slot-tier)]
      (case tier
        nil (->Unbound k)
        (if-some [s (get (.-env tier) k)]
          s (recur (.-parent tier)))))))

(defn resolve-node [^objects context tier id]
  (let [prev (l/get-local current)
        prev-tier (aget prev peer-slot-tier)]
    (l/set-local current context)
    (aset context peer-slot-tier tier)
    (try ((aget context peer-slot-defs) id)
         (finally
           (aset context peer-slot-tier prev-tier)
           (l/set-local current prev)))))

(deftype NodePs [^objects peer k ps]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (aset peer peer-slot-store
      (dissoc (aget peer peer-slot-store) k))
    (ps))
  IDeref
  (#?(:clj deref :cljs -deref) [_] @ps))

(deftype Node [^objects peer tier id]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((let [k [tier id]
           store (aget peer peer-slot-store)]
       (if-some [s (get store k)]
         s (let [n (resolve-node peer tier id)
                 s (m/signal i/combine (fn [step done] (->NodePs peer k (n step done))))]
             (aset peer peer-slot-store (assoc store k s)) s))) step done)))

(defn local [id]
  (let [peer (l/get-local current)
        tier (aget peer peer-slot-tier)]
    (->Node peer tier id)))

(defn remote [id])

(deftype Ctor [node env])

(defn ctor [id & args]
  (let [peer (l/get-local current)
        tier (aget peer peer-slot-tier)]
    (pure (->Ctor (->Node peer tier id) {}))))

(deftype Var [context k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ ^Ctor ctor incseq]
    (->Ctor (.-node ctor) (assoc (.-env ctor) k incseq))))

(defn var [k]
  (pure (->Var (l/get-local current) k)))

(defn free [id]
  )

(defn call [expr]

  )

(def ap
  (partial i/latest-product
    (fn
      ([f] (f))
      ([f a] (f a))
      ([f a b] (f a b))
      ([f a b c] (f a b c))
      ([f a b c d] (f a b c d))
      ([f a b c d & e] (apply f a b c d e)))))

(def join i/latest-concat)