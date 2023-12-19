(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref))))

(def ^{::type ::node, :doc "for loop/recur impl"} rec)

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

(def peer-slot-input 0)
(def peer-slot-store 1)
(def peer-slots 2)

(defn pure [form]
  (i/fixed (m/cp form)))

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

(deftype Ctor [peer slots output free vars])

(deftype Peer [step done defs state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (prn :cancel-peer)

    )
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (prn :transfer-peer)

    ))

(defn bind [^Ctor ctor peer k v]
  (when-not (identical? peer (.-peer ctor))
    (throw (error "Can't bind foreign constructor.")))
  (->Ctor peer (.-slots ctor) (.-output ctor) (.-free ctor)
    (assoc (.-vars ctor) k v)))

(defrecord Var [peer k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ ctor v]
    (bind ctor peer k v)))

(declare tier-ctor)
(declare ctor-peer)

(deftype StoredPs [k ps]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (let [peer (.-state (ctor-peer (tier-ctor (:tier k))))]
      (aset peer peer-slot-store
        (dissoc (aget peer peer-slot-store) k)))
    (ps))
  IDeref
  (#?(:clj deref :cljs -deref) [_] @ps))

(defn get-flow [tier id]
  ((.-defs (ctor-peer (tier-ctor tier))) tier id))

(defrecord Node [tier id]
  IFn
  (#?(:clj invoke :cljs -invoke) [node step done]
    ((let [^objects peer (.-state (ctor-peer (tier-ctor tier)))
           store (aget peer peer-slot-store)]
       (if-some [s (get store node)]
         s (let [n (get-flow tier id)
                 s (m/signal i/combine (fn [step done] (->StoredPs node (n step done))))]
             (aset peer peer-slot-store (assoc store node s)) s)))
     step done)))

(deftype Tier [parent slot-id ^Ctor ctor]
  IFn
  (#?(:clj invoke :cljs -invoke) [tier step done]
    ((->Node tier (.-output ctor)) step done)))

(defrecord Slot [tier id]
  IFn
  (#?(:clj invoke :cljs -invoke) [slot step done]
    ((let [^Ctor ctor (tier-ctor tier)
           ^objects peer (.-state (ctor-peer ctor))
           store (aget peer peer-slot-store)]
       (if-some [s (get store slot)]
         s (let [n (i/latest-product
                     (fn [ctor]
                       (when-not (instance? Ctor ctor)
                         (throw (error (str "Not a constructor - " ctor))))
                       (when-not (identical? peer (.-peer ^Ctor ctor))
                         (throw (error "Can't call foreign constructor.")))
                       (->Tier tier id ctor))
                     (get-flow tier (nth (.-slots ctor) id)))
                 s (m/signal i/combine (fn [step done] (->StoredPs slot (n step done))))]
             (aset peer peer-slot-store (assoc store slot s)) s)))
     step done)))

(defn context-input-notify [^Peer peer done?]
  ;; TODO
  )

(defn ctor-peer
  "Returns the peer of given constructor."
  {:tag Peer}
  [^Ctor ctor]
  (.-peer ctor))

(defn tier-parent
  "Returns the parent tier of given tier if not root, nil otherwise."
  {:tag Tier}
  [^Tier tier]
  (.-parent tier))

(defn tier-slot-id
  "Returns the index of the slot of given tier within its parent."
  [^Tier tier]
  (.-slot-id tier))

(defn tier-slot
  "Returns the slot for given tier and id."
  {:tag Slot}
  [^Tier tier id]
  (->Slot tier id))

(defn tier-slot-count
  "Returns the count of children of given tier."
  [^Tier tier]
  (count (.-slots (tier-ctor tier))))

(defn tier-output
  "Returns the output of given tier."
  {:tag Node}
  [^Tier tier]
  (->Node tier (.-output (tier-ctor tier))))

(defn tier-ctor
  "Returns the constructor of given tier."
  {:tag Ctor}
  [^Tier tier]
  (.-ctor tier))

(defn tier-peer
  "Returns the peer of given tier."
  {:tag Peer}
  [tier]
  (ctor-peer (tier-ctor tier)))

(defn tier-lookup
  "Returns the value associated with given key in the dynamic environment of given tier."
  [tier k]
  (loop [tier tier]
    (if-some [s (get (.-vars (tier-ctor tier)) k)]
      s (if-some [p (tier-parent tier)]
          (recur p) (->Unbound k)))))

(defn peer-ctor "
Returns a constructor for given peer, with slots defined by given vector of ids, output defined by given id, and
given free variables.
" [peer slots output & free]
  (->Ctor peer slots output (object-array free) {}))

(defn tier-local
  "Returns the incremental sequence signal defined by given id in given tier."
  [tier id]
  (->Node tier id))

(defn tier-remote [tier id]
  ;; TODO
  )

(defn peer-var
  "Returns the var associated with given key in given peer."
  [^Peer peer k]
  (->Var peer k))

(defn ctor-free
  "Returns the i-th free variable of given constructor."
  [^Ctor ctor i]
  (aget ^objects (.-free ctor) i))

(defn tier-slot
  "Returns the i-th slot of given tier."
  [^Tier tier i]
  (->Slot tier i))

(defn peer "
Returns a peer definition from given node definitions and root constructor.
" [defs slots output]
  (fn [msgs]
    (fn [step done]
      (let [state (object-array peer-slots)
            peer (->Peer step done defs state)]
        (aset state peer-slot-store {})
        (aset state peer-slot-input
          ((m/stream (m/observe msgs))
           #(context-input-notify peer false)
           #(context-input-notify peer true)))

        ((m/reduce (fn [_ x] (prn :output x)) nil
           (->Tier nil 0
             (->Ctor peer slots output
               (object-array 0) {})))
         #(prn :success %) #(prn :failure %))

        peer))))