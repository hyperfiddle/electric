(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.electric.impl.local :as l]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref))))

#?(:clj
   (def arg-sym
     (map (comp symbol
            (partial intern *ns*)
            (fn [i]
              (with-meta (symbol (str "%" i))
                {::type ::node})))
       (range))))
;; pre-define the first 20 for e/fn varargs expansion
(def ^{::type ::node} %0)
(def ^{::type ::node} %1)
(def ^{::type ::node} %2)
(def ^{::type ::node} %3)
(def ^{::type ::node} %4)
(def ^{::type ::node} %5)
(def ^{::type ::node} %6)
(def ^{::type ::node} %7)
(def ^{::type ::node} %8)
(def ^{::type ::node} %9)
(def ^{::type ::node} %10)
(def ^{::type ::node} %11)
(def ^{::type ::node} %12)
(def ^{::type ::node} %13)
(def ^{::type ::node} %14)
(def ^{::type ::node} %15)
(def ^{::type ::node} %16)
(def ^{::type ::node} %17)
(def ^{::type ::node} %18)
(def ^{::type ::node} %19)

(def peer-slot-step 0)
(def peer-slot-done 1)
(def peer-slot-defs 2)
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

(def this (l/local))

(deftype Tier [parent ctor static dynamic])
(deftype Ctor [peer id free vars])

(defn ctor [id & free]
  (let [^Tier tier (l/get-local this)
        ^Ctor ctor (.-ctor tier)
        ^objects peer (.-peer ctor)]
    (pure (->Ctor peer id (object-array free) {}))))

(defn bind [^Ctor ctor peer k v]
  (when-not (identical? peer (.-peer ctor))
    (throw (error "Can't bind foreign constructor.")))
  (->Ctor peer (.-id ctor) (.-free ctor)
    (assoc (.-vars ctor) k v)))

(defn lookup [k]
  (loop [^Tier tier (l/get-local this)]
    (let [^Ctor ctor (.-ctor tier)]
      (if-some [s (get (.-vars ctor) k)]
        s (if-some [p (.-parent tier)]
            (recur p) (->Unbound k))))))

(deftype NodePs [^objects peer k ps]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (aset peer peer-slot-store
      (dissoc (aget peer peer-slot-store) k))
    (ps))
  IDeref
  (#?(:clj deref :cljs -deref) [_] @ps))

(defn resolve-node [^objects peer tier id]
  (let [k [tier id]
        store (aget peer peer-slot-store)]
    (if-some [s (get store k)]
      s (let [n (let [that (l/get-local this)]
                  (l/set-local this tier)
                  (try ((aget peer peer-slot-defs) id)
                       (finally
                         (l/set-local this that))))
              s (m/signal i/combine (fn [step done] (->NodePs peer k (n step done))))]
          (aset peer peer-slot-store (assoc store k s)) s))))

(deftype Node [peer tier id]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((resolve-node peer tier id) step done)))

(defn local [id]
  (let [^Tier tier (l/get-local this)
        ^Ctor ctor (.-ctor tier)
        ^objects peer (.-peer ctor)]
    (->Node peer tier id)))

(defn remote [id])

(deftype Var [peer k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ ctor v]
    (bind ctor peer k v)))

(defn var [k]
  (let [^Tier tier (l/get-local this)
        ^Ctor ctor (.-ctor tier)
        ^objects peer (.-peer ctor)]
    (pure (->Var peer k))))

(defn free [id]
  (let [^Tier tier (l/get-local this)
        ^Ctor ctor (.-ctor tier)
        ^objects free (.-free ctor)]
    (aget free id)))

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

(defn context-input-notify [^objects state done?]

  )

(deftype PeerPs [^objects state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]


    )
  IDeref
  (#?(:clj deref :cljs -deref) [_]


    ))

(defn peer [msgs defs]
  (fn [step done]
    (let [peer (object-array peer-slots)]
      (aset peer peer-slot-step step)
      (aset peer peer-slot-done done)
      (aset peer peer-slot-defs defs)
      (aset peer peer-slot-store {})
      (aset peer peer-slot-input
        ((m/stream (m/observe msgs))
         #(context-input-notify peer false)
         #(context-input-notify peer true)))

      (->Ctor peer 0 (object-array 0) {})

      (->PeerPs peer))))
