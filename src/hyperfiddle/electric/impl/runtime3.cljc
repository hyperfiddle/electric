(ns hyperfiddle.electric.impl.runtime3
  (:refer-clojure :exclude [resolve *e])
  (:require [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.arrays-impl :as a]
            [missionary.core :as m]
            #?(:cljs missionary.impl.Propagator)
            [clojure.pprint]
            #?(:clj [clojure.tools.logging :as log])
            [hyperfiddle.electric.impl.lang3 :as-alias lang]
            [cognitect.transit :as t]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.stateful-diff-impl :as sd]
            [hyperfiddle.incseq.to-stateful-impl :as ts]
            [hyperfiddle.incseq.from-stateful-impl :as fs]
            [clojure.string :as str]
            [hyperfiddle.electric.impl.dynamic-local :as dl]
            [hyperfiddle.electric.impl.missionary-util :as mu]
            [hyperfiddle.incseq.sync-impl :refer [get-sync cas-sync]])
  (:import missionary.Cancelled
           missionary.impl.PairingHeap
           #?(:clj missionary.impl.Propagator$Publisher)
           #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream Writer))
           #?(:clj (java.util.concurrent.atomic AtomicInteger AtomicReference))))

;; turned off until missionary.core/amb= gets an int type hint
;; #?(:clj (set! *warn-on-reflection* true))

(def reclaim
  "Returns a fresh object. When the object is reclaimed by GC, provided function is called with no argument."
  #?(:cljs
     (let [registry (js/FinalizationRegistry. #(%))]
       (fn [f] (let [obj (js-obj)] (.register registry obj f) obj)))
     :clj (fn [f] (reify Object (finalize [_] (f))))))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (.error js/console e)))

(defn cnt [r _] (inc r))

(defn run
  ([f] (f) f)
  ([f x] (f x) f)
  ([f x y] (f x y) f)
  ([f x y & zs] (apply f x y zs) f))

(defn toggle! [xs y]
  (if (contains? xs y)
    (disj! xs y)
    (conj! xs y)))

(defn sym-diff "
Symmetric difference https://en.wikipedia.org/wiki/Symmetric_difference
" ([] #{})
  ([xs] xs)
  ([xs ys]
   (let [cxs (count xs)
         cys (count ys)]
     (if (zero? cxs)
       ys (if (zero? cys)
            xs (persistent!
                 (if (< cxs cys)
                   (reduce toggle! (transient ys) xs)
                   (reduce toggle! (transient xs) ys)))))))
  ([xs ys & zs] (reduce sym-diff (sym-diff xs ys) zs)))

(deftype Peer [site defs request next-id])

(defn peer-defs [^Peer peer]
  (.-defs peer))

(defn peer-site [^Peer peer]
  (.-site peer))

(defn next-id! [^Peer peer]
  #?(:clj (.incrementAndGet ^AtomicInteger (.-next-id peer))
     :cljs (set! (.-next-id peer) (inc (.-next-id peer)))))

(deftype Signal [slot site deps meta input request flow])

(defn signal-site [^Signal signal]
  (.-site signal))

(defn signal-deps [^Signal signal]
  (.-deps signal))

(defn signal-meta [^Signal signal]
  (.-meta signal))

(defn signal-flow [^Signal signal]
  (.-flow signal))

(def session-slot-socket 0)
(def session-slot-signal 1)
(def session-slot-store 2)
(def session-slot-request 3)
(def session-slot-pending 4)
(def session-slot-state 5)
(def session-slot-event 6)
(def session-slot-buffer 7)
(def session-slot-size 8)
(def session-slot-delayed 9)
(def session-slot-updated 10)                                ;; linked list of sessions
(def session-slots 11)

(def socket-slot-root 0)
(def socket-slot-step 1)
(def socket-slot-done 2)
(def socket-slot-shared 3)
(def socket-slot-message 4)
(def socket-slot-request 5)
(def socket-slot-head 6)
(def socket-slot-sync 7)
(def socket-slot-owner 8)
(def socket-slot-pending 9)
(def socket-slot-sessions 10)                                 ;; slot -> session
(def socket-slot-dangling-frames 11)
(def socket-slot-requested 12)
(def socket-slot-updated 13)                                ;; linked list of sessions
(def socket-slot-reader 14)
(def socket-slot-writer 15)
(def socket-slot-unacked-buffer 16)
(def socket-slot-unacked-head 17)
(def socket-slot-unacked-tail 18)
(def socket-slot-message-acks 19)
(def socket-slot-message-request 20)
(def socket-slot-message-frames 21)
(def socket-slot-message-change 22)
(def socket-slot-message-diffs 23)
(def socket-slot-message-freeze 24)
(def socket-slots 25)

(def event-slot-type 0)
(def event-slot-target 1)
(def event-slot-process 2)
(def event-slot-child 3)
(def event-slot-sibling 4)
(def event-slots 5)

(def item-slot-parent 0)
(def item-slot-current 1)
(def item-slot-position 2)
(def item-slot-event 3)
(def item-slots 4)

;; Pure | Ap | Join | Slot
(defprotocol Expr
  (deps [_ rf r]) ;; emits signals
  (t [_])
  (peephole [_])
  (pp [_]))                                               ;; returns incseq

(extend-protocol Expr
  #?(:clj Object :cljs default) (t [_]) (peephole [this] this) (deps [_ _ r] r) (pp [x] x)
  nil                           (t [_]) (peephole [this] this) (deps [_ _ r] r) (pp [x] x))

(declare ->is ->cf size-1?)

(deftype Fixed [cf ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (hash-combine (hash Fixed) (hash cf)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Fixed other)
      (= cf (.-cf ^Fixed other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::fixed)
  (peephole [this] (let [cf2 (peephole cf)] (if (= cf cf2) this (new Fixed cf2 nil))))
  (pp [_] (list 'Fixed (pp cf)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((i/fixed cf) step done)))

(defn ->fixed [cf] (->Fixed cf nil))

(deftype Failure [info])

(defn failure-info [^Failure f]
  (.-info f))

(defn failure? [x]
  (instance? Failure x))

(defn invariant [x] (m/cp x))

(deftype Id [x]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (hash x))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Id other)
      (= x (.-x ^Id other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::id)
  (peephole [this] this)
  (pp [_] (pp x))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (x step done)))

(deftype CFId [x]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (hash x))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? CFId other)
      (= x (.-x ^CFId other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::cf-id)
  (peephole [this] this)
  (pp [_] (pp x))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (x step done)))

(defn ->cf-id [x] (->CFId x))

(declare join-input)
(deftype Pure [mt value
               ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (hash-combine (hash Pure) (hash value)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Pure other)
      (= value (.-value ^Pure other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::pure)
  (peephole [this]
    (if (failure? value)
      this
      (if-some [typ (t value)]
        (if (= ::join typ)
          (->Id (join-input value))
          this)
        this)))
  (pp [_] (list 'Pure (pp value)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((dl/bind [mu/*mt mt]
       (if (failure? value)
         (m/latest #(throw (ex-info "Illegal access." {:info (failure-info value)})))
         (i/fixed (invariant value)))) step done)))

(defn pure "
-> (EXPR VOID)
T -> (EXPR T)
T T -> (EXPR T)
T T T -> (EXPR T)
" ([value] (pure {} value))
  ([mt value] (->Pure mt value nil)))

(deftype CFPure [value ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (hash-combine (hash CFPure) (hash value)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? CFPure other)
      (= value (.-value ^CFPure other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::cf-pure)
  ;; TODO cfjoin peephole?
  (peephole [this] this)
  (pp [_] (list 'CFPure (pp value)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((invariant value) step done)))

(defn ->cf-pure [v] (->CFPure v nil))

(deftype CFThunk [thunk ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (hash-combine (hash CFThunk) (hash thunk)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? CFThunk other)
      (= thunk (.-thunk ^CFThunk other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::cf-thunk)
  (peephole [this] this)
  (pp [_] '(CFThunk))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((m/cp (thunk)) step done)))

(defn ->cf-thunk [thunk] (->CFThunk thunk nil))

(defn clean-msg [mt]
  (str "L" (::lang/line mt) ":" (::lang/column mt)
    " " (::lang/ns mt) (when-some [d (::lang/def mt)] (str "/" d))))

(defn clean-ex [mt msg]
  (let [msg (str "EXCEPTION\n" (clean-msg mt) "\n" msg)]
    #?(:clj (proxy [Exception] [msg nil false false])
       :cljs (js/Error msg))))

(defn ->class-name [o] #?(:clj (.getCanonicalName ^Class o) :cljs (str o)))

(defn ex-messages [e]
  (into [] (comp (take-while some?)
             (map #(str (->class-name (type %)) (when-some [msg (ex-message %)] (str ": " msg)))))
    (iterate ex-cause e)))

(def *e nil)

(defn ?swap-exception [f mt]
  (try (f)
       (catch #?(:clj Throwable :cljs :default) e
         #?(:clj (alter-var-root #'*e (constantly e))
            :cljs (set! *e e))
         (let [clean-ex (clean-ex mt (str/join "\nvia: " (ex-messages e)))]
           #?(:clj (log/error e)
              :cljs ; console.error would print an irrelevant stacktrace
              (.log js/console (str "[ERROR] " (ex-message clean-ex))))
           (throw clean-ex)))))

(defn invoke-with [mt]
  (fn
    ([f] (?swap-exception #(f) mt))
    ([f a] (?swap-exception #(f a) mt))
    ([f a b] (?swap-exception #(f a b) mt))
    ([f a b c] (?swap-exception #(f a b c) mt))
    ([f a b c d] (?swap-exception #(f a b c d) mt))
    ([f a b c d & es] (?swap-exception #(apply f a b c d es) mt))))

(deftype CFAp [mt inputs ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash CFAp)
            (hash-ordered-coll inputs)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? CFAp other)
      (= inputs (.-inputs ^CFAp other))))
  Expr
  (deps [_ rf r]
    (reduce (fn [r x] (deps x rf r)) r inputs))
  (t [_] ::cf-ap)
  (peephole [this]
    (let [in* (mapv peephole inputs)]
      (if (every? #(= ::cf-pure (t %)) in*)
        (let [v* (mapv #(.-value ^CFPure %) in*)]
          (->cf-thunk #(apply (invoke-with mt) v*)))
        (if (= in* inputs) this (new CFAp mt in* nil)))))
  (pp [_] (cons 'CFAp (eduction (map pp) inputs)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((dl/bind [mu/*mt mt]
       (apply m/latest (invoke-with mt) inputs)) step done)))

(defn ->cf-ap [mt & inputs] (->CFAp mt inputs nil))

(deftype Ap [mt inputs
             ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash Ap)
            (hash-ordered-coll inputs)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Ap other)
      (= inputs (.-inputs ^Ap other))))
  Expr
  (deps [_ rf r]
    (reduce (fn [r x] (deps x rf r)) r inputs))
  (t [_] ::ap)
  (peephole [this]
    (let [in* (mapv peephole inputs)]
      (if (every? size-1? in*)
        (let [in* (mapv ->cf in*)]
          (->fixed (peephole (apply ->cf-ap mt in*))))
        (if (= in* inputs) this (new Ap mt in* nil)))))
  (pp [_] (cons 'Ap (eduction (map pp) inputs)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((dl/bind [mu/*mt mt]
       (let [in* (mapv ->is inputs)]
         (apply i/latest-product (invoke-with mt) in*))) step done)))

(defn error [^String msg]
  #?(:clj (Error. msg)
     :cljs (js/Error. msg)))

(defn arity-mismatch [nm arity]
  (throw (error (str nm ": wrong number of args (" arity ")"))))

(defn get-variadic [nm F arity]
  (if-some [[fixed map? ctor] (F -1)]
    (if (< arity fixed)
      (arity-mismatch nm arity)
      [fixed map? ctor])
    (arity-mismatch nm arity)))

(defn as-varargs [map?]
  (if map?
    (fn [& args]
      (loop [args args
             m nil]
        (if-some [[k & args] args]
          (if-some [[v & args] args]
            (recur args (assoc m k v))
            (merge m k)) m)))
    (fn [& args] args)))

(deftype Varargs [map? inputs ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash-combine (hash Varargs) (hash map?))
            (hash-ordered-coll inputs)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Varargs other)
      (= map? (.-map? ^Varargs other))
      (= inputs (.-inputs ^Varargs other))))
  Expr
  (deps [_ rf r]
    (reduce (fn [r x] (deps x rf r)) r inputs))
  (t [_] ::varargs)
  (peephole [this] this)
  (pp [_] (cons 'Varargs (eduction (map pp) inputs)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((if (some? inputs)
       (let [in* (mapv ->is inputs)]
         (apply i/latest-product (as-varargs map?) in*))
       (i/fixed (invariant nil))) step done)))

(defn ->varargs [map? & inputs] (->Varargs map? inputs nil))

(defn ap "
(EXPR (-> T)) -> (EXPR T)
(EXPR (A -> T)) (EXPR A) -> (EXPR T)
(EXPR (A B -> T)) (EXPR A) (EXPR B) -> (EXPR T)
(EXPR (A B C -> T)) (EXPR A) (EXPR B) (EXPR C) -> (EXPR T)
" [mt & inputs]
  (->Ap mt inputs nil))

(deftype Join [mt input ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash Join)
            (hash input)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Join other)
      (= input (.-input ^Join other))))
  Expr
  (deps [_ rf r] (deps input rf r))
  (t [_] ::join)
  (peephole [this]
    (let [in (peephole input)]
      ;; TODO generic incseq-1 carrying an incseq?
      (if (= ::pure (t in))
        (->Id (.-value ^Pure in))
        (if (= in input) this (new Join mt in nil)))))
  (pp [_] (list 'Join (pp input)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((dl/bind [mu/*mt mt] (i/latest-concat (->is input))) step done)))

(defn join "
(EXPR (IS T)) -> (EXPR T)
" ([input] (join {} input))
  ([mt input] (->Join mt input nil)))

(defn join-input [x] (.-input ^Join x))

(def effect "
-> (EXPR VOID)
(IS T) -> (EXPR T)
(IS T) (IS T) -> (EXPR T)
(IS T) (IS T) (IS T) -> (EXPR T)
" ->Id)

(def fixed-signals "
-> (IS VOID)
(CF T) -> (IS T)
(CF T) (CF T) -> (IS T)
(CF T) (CF T) (CF T) -> (IS T)
" (comp (partial m/signal i/combine) i/fixed))

(defn drain "
(IS T) -> (IS VOID)
" [incseq]
  (m/latest (constantly (sd/empty-diff 0)) incseq))

(def void (i/fixed))

(deftype Failer [done e]
  IFn
  (#?(:clj invoke :cljs -invoke) [_])
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (done) (throw e)))

(deftype Unbound [key ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash Unbound)
            (hash key)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Unbound other)
      (= key (.-key ^Unbound other))))
  Expr
  (deps [_ _ r] r)
  (t [_] ::unbound)
  (peephole [this] this)
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (step) (->Failer done (error (str "Unbound electric var lookup - " (pr-str key))))))

(deftype Cdef [frees nodes calls result build])

(def cdef ->Cdef)

(defn bind "
(CTOR T) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) (VAR B) (EXPR B) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) (VAR B) (EXPR B) (VAR C) (EXPR C) -> (CTOR T)
" ([ctor] ctor)
  ([[key idx free env] k v]
   [key idx free (assoc env k v)])
  ([[key idx free env] k v & kvs]
   [key idx free (apply assoc env k v kvs)]))

(defn bind-args [ctor & args]
  (reduce (partial apply bind) ctor (eduction (map-indexed vector) args)))

(defn bind-self [ctor]
  (bind ctor :recur (pure ctor)))

(defn dispatch-varargs [nm F arity args]
  (let [[fixed map? ctor] (get-variadic nm F arity)]
        (bind (apply bind-args (bind-self ctor) (take fixed args))
          fixed (apply ->varargs map? (drop fixed args)))))

(defn dispatch
  ([nm F] (if-some [ctor (F 0)] (bind-self ctor) (dispatch-varargs nm F 0 [])))
  ([nm F a] (if-some [ctor (F 1)] (-> ctor bind-self (bind 0 a)) (dispatch-varargs nm F 1 [a])))
  ([nm F a b] (if-some [ctor (F 2)] (-> ctor bind-self (bind 0 a) (bind 1 b)) (dispatch-varargs nm F 2 [a b])))
  ([nm F a b c] (if-some [ctor (F 3)] (-> ctor bind-self (bind 0 a) (bind 1 b) (bind 2 c)) (dispatch-varargs nm F 3 [a b c])))
  ([nm F a b c d] (if-some [ctor (F 4)] (-> ctor bind-self (bind 0 a) (bind 1 b) (bind 2 c) (bind 3 d)) (dispatch-varargs nm F 4 [a b c d])))
  ([nm F a b c d e] (if-some [ctor (F 5)] (-> ctor bind-self (bind 0 a) (bind 1 b) (bind 2 c) (bind 3 d) (bind 4 e)) (dispatch-varargs nm F 5 [a b c d e])))
  ([nm F a b c d e & args]
   (let [args (list* a b c d e args), arity (count args)]
     (if-some [ctor (F arity)]
       (apply bind-args (bind-self ctor) args)
       (dispatch-varargs nm F arity args)))))

(defn peer-resolve [^objects peer key]
  (let [defs (peer-defs peer)]
    (when-not (contains? defs key) (throw (error (str (pr-str key) " not defined"))))
    (defs key)))

(defn peer-cdef
  "Returns the cdef of given constructor."
  {:tag Cdef}
  [^objects peer key idx]
  ((peer-resolve peer key) idx))

(deftype Frame [peer slot id site ctor ^objects nodes ^objects tags
                ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (-> (hash Frame)
            (hash-combine (hash peer))
            (hash-combine (hash site))
            (hash-combine (hash id))))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Frame other)
      (= peer (.-peer ^Frame other))
      (= site (.-site ^Frame other))
      (= id (.-id ^Frame other))))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((signal-flow (aget nodes (dec (alength nodes)))) step done)))

(deftype Tag [frame index
              ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (-> (hash Tag)
                          (hash-combine (hash frame))
                          (hash-combine (hash index))))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Tag other)
      (= frame (.-frame ^Tag other))
      (= index (.-index ^Tag other)))))

(defn tag [^Frame frame id]
  (->Tag frame id nil))

(defn tag-frame [^Tag tag]
  (.-frame tag))

(defn tag-index [^Tag tag]
  (.-index tag))

(defn frame-call [^Frame frame index]
  (aget ^objects (.-tags frame) index))

(defn frame-call-count [^Frame frame]
  (alength ^objects (.-tags frame)))

(defn frame-ctor
  "Returns the constructor of given frame."
  [^Frame frame]
  (.-ctor frame))

(defn frame-peer
  "Returns the peer of given frame."
  [^Frame frame]
  (.-peer frame))

(defn frame-cdef
  "Returns the cdef of given frame."
  {:tag Cdef}
  [^Frame frame]
  (let [[key idx _ _] (frame-ctor frame)]
    (peer-cdef (.-peer frame) key idx)))

(defn resolve
  "Returns the root binding of electric var matching given keyword."
  [^Frame frame key]
  (when-some [f (peer-resolve (.-peer frame) key)]
    (f))
  #_((peer-resolve (.-peer frame) key)))

(defn frame-site
  "Returns the site of given frame."
  [^Frame frame]
  (.-site frame))

(defn frame-signal [^Frame frame id]
  (if (neg? id)
    (aget ^objects (.-nodes frame) (- -1 id))
    (aget ^objects (.-tags frame) id)))

(deftype Slot [^Frame frame id]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (hash (frame-signal frame id)))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ that]
    (and (instance? Slot that)
      (= (frame-signal frame id)
        (frame-signal (.-frame ^Slot that) (.-id ^Slot that)))))
  Expr
  (deps [_ rf r] (rf r (frame-signal frame id)))
  (t [_] ::slot)
  (peephole [this] this)
  (pp [_] (list 'Slot id))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((signal-flow (frame-signal frame id)) step done)))

(defn ->is [x]
  (case (t x)
    (::pure ::join ::ap ::slot ::id ::fixed ::unbound ::varargs) x
    (::cf-pure ::cf-ap ::cf-thunk ::cf-id) (->fixed x)
    #_else (throw (ex-info (str "cannot create incseq on type " (t x)) {:is x}))))

(defn ->cf [x]
  (let [how #(throw (ex-info "how to turn to CF?" {:x x}))]
    (case (t x)
      (::cf-pure ::cf-ap ::cf-thunk ::cf-id) x
      (::pure) (->cf-pure (.-value ^Pure x))
      (::fixed) (->cf-id (.-cf ^Fixed x))
      #_else (how))))
(defn size-1? [x]
  (case (t x)
    (::cf-pure ::cf-ap ::cf-thunk ::cf-id ::pure ::fixed) x
    #_else false))

(defn slot-signal
  {:tag 'Signal}
  [^Slot slot]
  (frame-signal (.-frame slot) (.-id slot)))

(defn frame-path [^Frame frame]
  (loop [^Frame frame frame
         path ()]
    (if-some [^Slot slot (.-slot frame)]
      (recur (.-frame slot)
        (conj path [(.-id slot) (.-id frame)]))
      (vec path))))

(defn signal-slot
  {:tag Slot}
  [^Signal signal]
  (.-slot signal))

(defn signal-coordinates [^Signal signal]
  (let [slot (signal-slot signal)]
    [(frame-path (.-frame slot)) (.-id slot)]))

(defn slot-frame
  "Returns the frame of given slot."
  {:tag Frame}
  [^Slot slot]
  (.-frame slot))

(defn socket-site [^objects socket]
  (peer-site (frame-peer (aget socket socket-slot-root))))

(defn session-signal ^Signal [^objects session]
  (aget session session-slot-signal))

(defn item-parent ^objects [^objects item]
  (aget item item-slot-parent))

(defn event-cancel [^objects event]
  ((aget event event-slot-process)))

(defn get-child [_ ^objects event]
  (aget event event-slot-child))

(defn set-child [_ ^objects event x]
  (aset event event-slot-child x))

(defn get-sibling [_ ^objects event]
  (aget event event-slot-sibling))

(defn set-sibling [_ ^objects event x]
  (aset event event-slot-sibling x))

(defn event-priority [^objects event]
  (case (aget event event-slot-type)
    :message 0
    :request 1
    (:output :item) 2))

(defn event-publisher [^objects event]
  (let [target (aget event event-slot-target)]
    (case (aget event event-slot-type)
      :output (signal-flow (session-signal target))
      :item (signal-flow (session-signal (item-parent target))))))

(def impl
  (PairingHeap/impl
    (fn [_ ^objects x ^objects y]
      (case (compare (event-priority x) (event-priority y))
        -1 true
        0 (case (compare (event-publisher x) (event-publisher y))
            -1 false
            0 (= :output (aget x event-slot-type))
            +1 true)
        +1 false))
    get-child set-child get-sibling set-sibling))

(def idle #?(:clj (Object.) :cljs (js-obj)))

(defn event-terminated [event]
  (identical? idle (get-child nil event)))

(defn socket-event [^objects socket event]
  (aset socket socket-slot-pending
    (dec (aget socket socket-slot-pending)))
  (set-sibling socket event nil)
  (when-not ^boolean (event-terminated event)
    (aset socket socket-slot-head
      (if-some [h (aget socket socket-slot-head)]
        (PairingHeap/meld impl nil h event)
        event)) nil))

(defn socket-consume [^objects socket event]
  (loop [event event]
    (let [s (get-sibling socket event)]
      (socket-event socket event)
      (when (some? s) (recur s)))))

(defn socket-dequeue [^objects socket]
  (aset socket socket-slot-pending (inc (aget socket socket-slot-pending)))
  (let [h (aget socket socket-slot-head)]
    (aset socket socket-slot-head (PairingHeap/dmin impl nil h))
    (set-sibling socket h idle) h))

(defn socket-claim [^objects socket]
  (aset socket socket-slot-owner #?(:clj (Thread/currentThread) :cljs true)))

(defn socket-yield [^objects socket]
  (aset socket socket-slot-owner #?(:clj nil :cljs false)))

(defn socket-terminate [^objects socket ^objects event]
  (set-child socket event idle)
  (socket-event socket event))

(defn socket-flush [^objects socket ^objects event]
  (socket-claim socket)
  (try @(aget event event-slot-process)
       (catch #?(:clj Throwable :cljs :default) _))
  (socket-yield socket))

(defn socket-ready [^objects socket]
  (loop []
    (if (nil? (aget socket socket-slot-head))
      (if (zero? (aget socket socket-slot-pending))
        ((aget socket socket-slot-done))
        (cas-sync socket socket-slot-sync nil idle
          nil (do (loop []
                    (let [s (get-sync socket socket-slot-sync)]
                      (cas-sync socket socket-slot-sync s nil
                        (socket-consume socket s)
                        (recur))))
                  (recur))))
      (if-some [step (aget socket socket-slot-step)]
        (step)
        (let [^objects event (socket-dequeue socket)]
          (case (aget event event-slot-type)
            :message (socket-flush socket event)
            :request (socket-flush socket event)
            :output (let [^objects target (aget event event-slot-target)]
                      (if (nil? (aget target session-slot-event))
                        (socket-terminate socket event)
                        (socket-flush socket event)))
            :item (let [^objects target (aget event event-slot-target)]
                    (if (nil? (aget target item-slot-event))
                      (socket-terminate socket event)
                      (socket-flush socket event))))
          (recur))))))

(def consecutive #{:output :item})

(defn socket-step [^objects socket event]
  (if (and (contains? consecutive (aget event event-slot-type))
        #?(:clj  (identical? (Thread/currentThread) (aget socket socket-slot-owner))
           :cljs ^boolean (aget socket socket-slot-owner)))
    (socket-event socket event)
    (loop []
      (let [s (get-sync socket socket-slot-sync)]
        (if (identical? s idle)
          (cas-sync socket socket-slot-sync idle nil
            (do (socket-event socket event)
                (socket-ready socket)) (recur))
          (do (set-sibling socket event s)
              (cas-sync socket socket-slot-sync s event
                nil (recur))))))))

(defn socket-spawn [^objects socket type flow]
  (aset socket socket-slot-pending
    (inc (aget socket socket-slot-pending)))
  (let [event (object-array event-slots)]
    (aset event event-slot-type type)
    (aset event event-slot-process
      (flow #(socket-step socket event)
        #(do (set-child socket event idle)
             (socket-step socket event))))
    event))

(defn make-signal [^Slot slot site mt deps flow]
  (let [input (m/store (i/fixed))]
    (->Signal slot site deps mt input #?(:clj (AtomicInteger. 0) :cljs 0)
      (m/signal i/combine
        (if (= site (peer-site (frame-peer (slot-frame slot))))
          flow (i/latest-concat (i/fixed input)))))))

(defn update-inc [m k]
  (assoc m k (inc (m k 0))))

(defn define-slot [^Slot slot mt expr]
  (let [^Frame frame (.-frame slot)
        id (.-id slot)
        site (if-some [site (let [cdef (frame-cdef frame)
                                  nodes (.-nodes cdef)
                                  id (- -1 id)]
                              (if (= id (count nodes))
                                (.-result cdef) (nodes id)))]
               site (frame-site frame))
        signal (if (instance? Slot expr)
                 (slot-signal expr)
                 (let [pexpr (peephole expr)
                       iexpr (->is pexpr)]
                   (make-signal slot site mt
                     (persistent! (deps iexpr conj! (transient #{})))
                     iexpr)))]
    (aset ^objects (.-nodes frame) (- -1 id) signal) nil))

(defn node
  "Returns the signal node id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (->Slot frame (- -1 id)))

(defn call
  "Returns the call site id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (->Slot frame id))

(defn define-node
  "Defines signals node id for given frame."
  [^Frame frame id mt expr]
  (define-slot (node frame id) mt expr))

(defn slot-id
  "Returns the id of given slot."
  [^Slot slot]
  (.-id slot))

(defn frame-slot [^Frame frame]
  (.-slot frame))

(defn frame-result-slot [^Frame frame]
  (let [^objects nodes (.-nodes frame)]
    (node frame (dec (alength nodes)))))

(defn frame-result-signal [^Frame frame]
  (let [^objects nodes (.-nodes frame)]
    (aget nodes (dec (alength nodes)))))

(defn call? [^Slot slot]
  (not (neg? (.-id slot))))

(defn socket-unacked-push [^objects socket callback]
  (let [^objects buffer (aget socket socket-slot-unacked-buffer)
        cap (alength buffer)
        push (aget socket socket-slot-unacked-tail)]
    (if (nil? (aget buffer push))
      (do (aset socket socket-slot-unacked-tail (rem (unchecked-inc push) cap))
          (aset buffer push callback))
      (let [c (bit-shift-left cap 1)
            b (object-array c)
            s (- cap push)]
        (a/acopy buffer push b 0 s)
        (a/acopy buffer 0 b s push)
        (aset socket socket-slot-unacked-buffer b)
        (aset socket socket-slot-unacked-head (+))
        (aset socket socket-slot-unacked-tail (rem (unchecked-inc cap) c))
        (aset b cap callback)))))

(defn socket-unacked-pull [^objects socket]
  (let [^objects buffer (aget socket socket-slot-unacked-buffer)
        cap (alength buffer)
        pull (aget socket socket-slot-unacked-head)
        callback (aget buffer pull)]
    (when (nil? callback) (throw (ex-info "Unexpected ack." {})))
    (aset socket socket-slot-unacked-head
      (rem (unchecked-inc pull) cap))
    (aset buffer pull nil) callback))

(defn signal-local? [^Signal signal]
  (= (signal-site signal) (peer-site (frame-peer (slot-frame (signal-slot signal))))))

(defn slot-path [slot]
  (into [] (comp (take-while some?) (map slot-id)) (iterate (comp frame-slot slot-frame) slot)))

(def check (comp #{[]} slot-path))

(defn call-state [^longs call-store prefix]
  (let [p (bit-shift-left prefix 2)]
    (loop [i 0
           n 0]
      (if (< i 4)
        (recur (unchecked-inc i)
          (if (pos? (aget call-store (bit-or p i)))
            (bit-or n (bit-shift-left 1 i)) n)) n))))

(defn session-spawn [^objects socket signal ^longs store ^longs request ^longs pending]
  (let [session (object-array session-slots)
        event (if (= (signal-site signal) (peer-site (frame-peer (aget socket socket-slot-root))))
                (doto (object-array event-slots)
                  (aset event-slot-type :output)
                  (aset event-slot-target session))
                (m/store d/combine (d/empty-diff 0)))]
    #_
    (when (check (signal-slot signal))
      (prn :session-spawn))
    (aset socket socket-slot-sessions (assoc (aget socket socket-slot-sessions) (signal-slot signal) session))
    (aset session session-slot-socket socket)
    (aset session session-slot-signal signal)
    (aset session session-slot-store store)
    (aset session session-slot-request request)
    (aset session session-slot-pending pending)
    (aset session session-slot-state (call-state request 0))
    (aset session session-slot-buffer (object-array 1))
    (aset session session-slot-size (+))
    (aset session session-slot-event event)
    (aset session session-slot-updated session)
    (if (signal-local? signal)
      (do (aset socket socket-slot-pending
            (inc (aget socket socket-slot-pending)))
          (aset event event-slot-process
            ((fs/flow (signal-flow signal))
             #(socket-step socket event)
             #(do (aset session session-slot-event nil)
                  (socket-step socket event)))))
      ((.-input signal) (ts/flow event)))
    session))

(defn session-check-create ^objects [^objects socket ^Signal signal]
  (let [slot (signal-slot signal)]
    (if-some [session (get (aget socket socket-slot-sessions) slot)]
      session (session-spawn socket signal
                (when (call? slot) (a/long-array 8))
                (a/long-array 4) (a/long-array 2)))))

(declare session-update-request)

(defn bump [n d]
  (-> n
    (unchecked-dec)
    (unchecked-add d)
    (unchecked-add d)))

(defn binary [flags n]
  (apply str
    (map (fn [i]
           (case (bit-and flags (bit-shift-left 1 i))
             0 "0" "1")) (range (dec n) -1 -1))))

;; flags - up? ack? local? from-local?
(defn walk [^objects socket flags ^Signal signal]
  (if (call? (signal-slot signal))
    (let [^objects session (session-check-create socket signal)
          ^objects buffer (aget session session-slot-buffer)
          ^longs store (aget session session-slot-store)
          index (bit-and flags 2r0111)
          prev (aget store index)
          curr (bump prev (bit-shift-right (bit-and flags 2r1000) 3))]
      (aset store index curr)
      (when-not (= (zero? prev) (zero? curr))
        (dotimes [i (aget session session-slot-size)]
          (let [^objects item (aget buffer i)
                frame (aget item item-slot-current)]
            (when (or (zero? (bit-and flags 2r0100)) (not (signal-local? signal))
                    (contains? (aget socket socket-slot-dangling-frames) frame))
              (walk socket flags (slot-signal (frame-result-slot frame)))))))
      (session-update-request session (bit-shift-right flags 1)))
    (if (= (zero? (bit-and flags 2r0001)) (signal-local? signal))
      (session-update-request (session-check-create socket signal) (bit-shift-right flags 1))
      (reduce run (partial walk socket flags) (signal-deps signal)))))

(defn peer-next-id [^Peer peer]
  #?(:clj (.incrementAndGet ^AtomicInteger (.-next-id peer))
     :cljs (set! (.-next-id peer) (inc (.-next-id peer)))))

(defn make-frame [^objects peer ^Slot slot id site ctor]
  (let [[key idx _ _] ctor
        cdef (peer-cdef peer key idx)
        nodec (count (.-nodes cdef))
        callc (count (.-calls cdef))
        frame (->Frame peer slot id site ctor
                (object-array (inc nodec)) (object-array callc) nil)
        built ((.-build cdef) frame)]
    (define-slot (->Slot frame (- -1 nodec)) {} built) frame))

(defn pick ^long [^long n ^long i]
  (bit-shift-right (bit-and n (bit-shift-left 1 i)) i))

;; flags - up? ack? local?
(defn session-update-request [^objects session flags]
  (let [^Signal signal (aget session session-slot-signal)
        ^objects socket (aget session session-slot-socket)
        ^longs request (aget session session-slot-request)
        index (bit-and 2r011 flags)
        prev (aget request index)
        curr (aset request index (bump prev (pick flags 2)))]
    #_
    (when (check (signal-slot (aget session session-slot-signal)))
      (prn :session-update-request (binary flags 3) prev '-> curr))
    (when-not (= (zero? prev) (zero? curr))
      (when (identical? session (aget session session-slot-updated))
        (aset session session-slot-updated (aget socket socket-slot-updated))
        (aset socket socket-slot-updated session))
      (reduce run (partial walk socket
                    (bit-or (bit-shift-left flags 1)
                      (if (signal-local? signal) 1 0)))
        (signal-deps signal)))))

(defn frame-call-store [^objects socket frame]
  (aget (session-check-create socket
          (slot-signal (frame-slot frame))) session-slot-store))

;; flags - up? ack?
(defn propagate-call-state [^objects socket flags state signal]
  (let [prefix (bit-shift-left flags 2)]
    (dotimes [i 4]
      (when (bit-test state i)
        (walk socket (bit-or prefix i) signal)))))

(defn socket-local-frame-up [^objects socket frame]
  #_(prn :local-frame-up frame)
  (let [signal (slot-signal (frame-result-slot frame))]
    (propagate-call-state socket 2r10 (call-state (frame-call-store socket frame) 0) signal)
    (aset socket socket-slot-message-frames
      (conj! (aget socket socket-slot-message-frames) frame))))

(defn socket-local-frame-down [^objects socket frame]
  #_(prn :local-frame-down frame)
  (let [frame-state (aget socket socket-slot-dangling-frames)
        signal (slot-signal (frame-result-slot frame))]
    (propagate-call-state socket 2r00 (call-state (frame-call-store socket frame) 0) signal)
    (aset socket socket-slot-message-frames
      (conj! (aget socket socket-slot-message-frames) frame))
    (aset socket socket-slot-dangling-frames
      (assoc! frame-state frame
        (bit-or (call-state (frame-call-store socket frame) 1)
          (if (contains? frame-state frame) 2r10000 0))))))

(defn socket-local-frame-ack [^objects socket frame]
  #_(prn :local-frame-ack frame)
  (let [frame-state (aget socket socket-slot-dangling-frames)
        signal (slot-signal (frame-result-slot frame))]
    (doto socket
      (aset socket-slot-dangling-frames
        (if-some [state (get frame-state frame)]
          (let [state- (bit-xor state 2r10000)]
            (if (zero? (bit-and state 2r10000))
              (do (propagate-call-state socket 2r11 state signal)
                  (assoc! frame-state frame state-))
              (do (propagate-call-state socket 2r01 state- signal)
                  (dissoc! frame-state frame))))
          (do (propagate-call-state socket 2r11 (call-state (frame-call-store socket frame) 1) signal)
              (assoc! frame-state frame nil)))))))

(defn socket-remote-frame-up [^objects socket frame]
  #_(prn :remote-frame-up frame)
  (let [signal (slot-signal (frame-result-slot frame))]
    (propagate-call-state socket 2r10 (call-state (frame-call-store socket frame) 0) signal)
    (propagate-call-state socket 2r11 (call-state (frame-call-store socket frame) 1) signal)))

(defn socket-remote-frame-down [^objects socket frame]
  #_(prn :remote-frame-down frame)
  (let [signal (slot-signal (frame-result-slot frame))]
    (propagate-call-state socket 2r00 (call-state (frame-call-store socket frame) 0) signal)
    (propagate-call-state socket 2r01 (call-state (frame-call-store socket frame) 1) signal)))

(defn output-item-dispose [^objects session pos]
  (let [^objects socket (aget session session-slot-socket)
        ^objects buffer (aget session session-slot-buffer)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)
        ^objects item (aget buffer pos)
        x (aget item item-slot-current)]
    (aset item item-slot-position nil)
    (aset buffer pos nil)
    (when-some [event (aget item item-slot-event)]
      (event-cancel event))
    (when (call? slot) (socket-local-frame-down socket x))))

(defn input-item-spawn [^objects session pos init]
  (let [^objects socket (aget session session-slot-socket)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)
        ^objects buffer (aget session session-slot-buffer)
        ^objects item (object-array item-slots)]
    (when (call? slot) (socket-remote-frame-up socket init))
    (aset item item-slot-parent session)
    (aset item item-slot-position pos)
    (aset item item-slot-current init)
    (aset buffer pos item)
    (aset item item-slot-event (m/store init))))

(defn input-change-at [^objects session i x]
  (let [^objects socket (aget session session-slot-socket)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)
        ^objects buffer (aget session session-slot-buffer)
        ^objects item (aget buffer i)
        p (aget item item-slot-current)]
    (when (call? slot)
      (socket-remote-frame-up socket x)
      (socket-remote-frame-down socket p))
    (aset item item-slot-current x)
    ((aget item item-slot-event) x)
    session))

(defn input-remove-last [^objects session]
  (let [pos (unchecked-dec (aget session session-slot-size))
        ^objects socket (aget session session-slot-socket)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)
        ^objects buffer (aget session session-slot-buffer)
        ^objects item (aget buffer pos)
        x (aget item item-slot-current)]
    (when (call? slot) (socket-remote-frame-down socket x))
    (aset buffer pos nil)
    (aset session session-slot-size pos)))

(defn input-freeze-at [^objects session i]
  (let [^objects buffer (aget session session-slot-buffer)
        ^objects item (aget buffer i)]
    ((aget item item-slot-event)))
  session)

(defn call-at
  ([^objects a i] ((aget a i)) a)
  ([^objects a i x] ((aget a i) x) a)
  ([^objects a i x y] ((aget a i) x y) a)
  ([^objects a i x y & zs] (apply (aget a i) x y zs) a))

(defn session-ensure-capacity [^objects session cap]
  (let [^objects buffer (aget session session-slot-buffer)
        size (alength buffer)]
    (if (< size cap)
      (let [b (object-array
                (loop [i size]
                  (let [i (bit-shift-left i 1)]
                    (if (< i cap) (recur i) i))))]
        (a/acopy buffer 0 b 0 size)
        (aset session session-slot-buffer b))
      buffer)))

(defn move-item [^objects buffer ^objects item pos]
  (aset item item-slot-position pos)
  (aset buffer pos item) buffer)

(defn apply-cycle [^objects buffer cycle]
  (let [i (nth cycle 0)]
    (move-item buffer (aget buffer i)
      (loop [i i
             k 1]
        (let [j (nth cycle k)
              k (unchecked-inc k)]
          (move-item buffer (aget buffer j) i)
          (if (< k (count cycle))
            (recur j k) j))))))

(defn active ^long [^long pending ^long request]
  (if (< pending 2) request 0))

(defn current-pending [^longs pending diff i]
  (-> (aget pending i)
    (unchecked-add (pick diff i))
    (unchecked-subtract (pick diff (bit-or i 2)))))

(defn session-active ^long [^objects session]
  (let [pending (aget session session-slot-pending)
        curr (call-state (aget session session-slot-request) 0)
        diff (bit-xor (aget session session-slot-state) curr)]
    (bit-or
      (active (current-pending pending diff 0) (pick curr 0))
      (active (current-pending pending diff 1) (pick curr 1)))))

(defn input-append [^objects socket [slot diff]]
  (when-some [^objects session (get (aget socket socket-slot-sessions) slot)]
    (when-not (zero? (session-active session))
      (let [{:keys [grow degree shrink permutation change freeze]} diff
            ip (i/inverse permutation)
            ^objects buffer (session-ensure-capacity session degree)]
        #_
        (when-not (= (- degree grow) (aget session session-slot-size))
          (prn :diff-corruption (slot-path slot)))
        (loop [i 0
               append (transient [])
               change (transient change)]
          (if (< i grow)
            (let [pos (aget session session-slot-size)
                  j (ip pos pos)]
              (aset session session-slot-size (unchecked-inc pos))
              (recur (unchecked-inc i)
                (conj! append (input-item-spawn session pos (get change j)))
                (dissoc! change j)))
            (let [append (persistent! append)
                  change (persistent! change)]
              (p/decompose apply-cycle buffer permutation)
              (reduce-kv input-change-at session change)
              (reduce input-freeze-at session freeze)
              (dotimes [_ shrink] (input-remove-last session))
              ((aget session session-slot-event)
               (d/diff append degree shrink ip))))))))
  socket)

(defn input-freeze [^objects socket ^Slot slot]
  (when-some [^objects session (get (aget socket socket-slot-sessions) slot)]
    (when-not (zero? (session-active session))
      (let [^objects buffer (aget session session-slot-buffer)]
        (loop [i 0]
          (when (< i (alength buffer))
            (when-some [^objects item (aget buffer i)]
              ((aget item item-slot-event))
              (recur (unchecked-inc i))))))
      ((aget session session-slot-event))))
  socket)

(defn socket-cancel [^objects socket]
  (event-cancel (aget socket socket-slot-message))
  (event-cancel (aget socket socket-slot-request)))

(defn ->unserializable-msg [slots d]
  (let [mt* (mapv (comp signal-meta slot-signal) slots)
        has-mt* (filterv ::lang/line mt*)
        msg (str (when (seq has-mt*)
                   (str "[unserializable] Possible values (if let-bound search for their usage):\n"
                     (str/join "\n" (eduction (map clean-msg) (distinct) has-mt*))
                     (when (not= (count mt*) (count has-mt*))
                       (str "\nThe value list is incomplete."))
                     "\n"))
              "Value: " d)]
    msg))

(defn spawn-output-item [^objects session flow]
  (let [^objects socket (aget session session-slot-socket)
        ^objects buffer (aget session session-slot-buffer)
        item (object-array item-slots)
        event (object-array event-slots)
        p (unchecked-inc (aget socket socket-slot-pending))
        pos (aget session session-slot-size)]
    (aset session session-slot-size (unchecked-inc pos))
    (aset event event-slot-type :item)
    (aset event event-slot-target item)
    (aset buffer pos item)
    (aset socket socket-slot-pending p)
    (aset item item-slot-parent session)
    (aset item item-slot-current item)
    (aset item item-slot-position pos)
    (aset item item-slot-event event)
    (aset event event-slot-process
      (flow #(socket-step socket event)
        #(do (aset item item-slot-event nil)
             (socket-step socket event))))
    (when (or (== p (aget socket socket-slot-pending)) (nil? (aget item item-slot-event)))
      (throw (new #?(:clj Error :cljs js/Error) "Uninitialized flow.")))
    session))

(defn session-cancel [^objects session]
  (let [^Signal signal (aget session session-slot-signal)]
    #_
    (when (check (signal-slot signal))
      (prn :session-cancel))
    (if (signal-local? signal)
      (do (when-some [event (aget session session-slot-event)]
            (event-cancel event))
          (dotimes [i (aget session session-slot-size)]
            (output-item-dispose session i))
          (aset session session-slot-size nil))
      ((.-input signal) (i/fixed)))))

(defn session-detach [^objects session]
  (let [^objects socket (aget session session-slot-socket)]
    (aset socket socket-slot-sessions
      (dissoc (aget socket socket-slot-sessions)
        (signal-slot (aget session session-slot-signal))))))

(defn session-delay [^objects session ^objects event]
  (set-sibling nil event (aget session session-slot-delayed))
  (aset session session-slot-delayed event))

(defn session-retry [^objects session]
  (when-some [event (aget session session-slot-delayed)]
    (aset session session-slot-delayed nil)
    (socket-consume (aget session session-slot-socket) event)))

;; flags - ack? local?
(defn session-request-toggle [^objects socket ^Slot slot flags]
  (let [signal (slot-signal slot)
        requested (aget socket socket-slot-requested)
        m (bit-shift-left 1 flags)
        x (get requested slot 0)
        y (bit-xor x m)]
    (aset socket socket-slot-requested
      (if (zero? y)
        (dissoc! requested slot)
        (assoc! requested slot y)))
    (walk socket
      (-> (bit-and flags 1)
        (bit-or (bit-shift-left flags 1))
        (bit-or (bit-shift-left (bit-shift-right (bit-and y m) flags) 3)))
      signal)))

(defn socket-request-toggle-ack [^objects socket slot]
  (doto socket
    (session-request-toggle slot 2r11)))

(defn socket-request-toggle-local [^objects socket slot]
  (doto socket
    (session-request-toggle slot 2r01)))

(defn socket-request-toggle-remote [^objects socket slot]
  (doto socket
    (session-request-toggle slot 2r00)
    (session-request-toggle slot 2r10)))

(defn pure-ack? [[_ r c d f]]
  (-> (count c)
    (unchecked-add (count d))
    (unchecked-add (count f))
    (unchecked-add (count r))
    (zero?)))

(def empty-msg [0 #{} [] [] []])

(defn socket-request [^objects socket r]
  (reduce socket-request-toggle-local socket r)
  (aset socket socket-slot-message-request
    (reduce toggle! (aget socket socket-slot-message-request) r)))

(defn half-state ^long [^long pending ^long request]
  (bit-or
    (bit-shift-left
      (bit-xor (bit-and pending 1) request) 1)
    (if (zero? pending) 0 1)))

(defn update-pending [^longs pending diff i]
  (aset pending i (current-pending pending diff i)))

(defn socket-output [^objects socket ^objects event]
  (let [ps (aget event event-slot-process)
        diffs (aget socket socket-slot-message-diffs)
        ^objects session (aget event event-slot-target)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)]
    (if (nil? (aget session session-slot-size))
      (if (nil? (aget session session-slot-event))
        (socket-terminate socket event)
        (try @ps (catch #?(:clj Throwable :cljs :default) _)))
      (if (zero? (session-active session))
        (session-delay session event)
        (if (nil? (aget session session-slot-event))
          (do (socket-terminate socket event)
              (aset socket socket-slot-message-freeze
                (conj! (aget socket socket-slot-message-freeze) slot)))
          (let [diff @ps]
            (when-not (d/empty-diff? diff)
              (let [append (d/append diff)
                    degree (d/degree diff)
                    shrink (d/shrink diff)
                    permutation (p/inverse (d/permutation diff))
                    ^objects buffer (session-ensure-capacity session degree)
                    d {:grow        (count append)
                       :shrink      shrink
                       :degree      degree
                       :permutation permutation
                       :change      {}
                       :freeze      #{}}]
                (reduce spawn-output-item session append)
                (p/decompose apply-cycle buffer permutation)
                (dotimes [_ shrink]
                  (let [pos (unchecked-dec (aget session session-slot-size))]
                    (aset session session-slot-size pos)
                    (output-item-dispose session pos)))
                (if-some [p (get diffs slot)]
                  (aset socket socket-slot-message-diffs
                    (assoc! diffs slot (sd/combine p d)))
                  (do (aset socket socket-slot-message-change
                        (conj! (aget socket socket-slot-message-change) slot))
                      (aset socket socket-slot-message-diffs
                        (assoc! diffs slot d))))))))))))

(defn socket-item [^objects socket ^objects event]
  (let [ps (aget event event-slot-process)
        diffs (aget socket socket-slot-message-diffs)
        ^objects item (aget event event-slot-target)
        ^objects session (aget item item-slot-parent)
        ^Signal signal (aget session session-slot-signal)
        ^Slot slot (.-slot signal)]
    (if-some [pos (aget item item-slot-position)]
      (if (zero? (session-active session))
        (session-delay session event)
        (if (nil? (aget item item-slot-event))
          (do (socket-terminate socket event)
              (if-some [d (get diffs slot)]
                (aset socket socket-slot-message-diffs
                  (assoc! diffs slot (update d :freeze conj pos)))
                (do (aset socket socket-slot-message-change
                      (conj! (aget socket socket-slot-message-change) slot))
                    (aset socket socket-slot-message-diffs
                      (assoc! diffs slot {:grow 0 :shrink 0 :degree (aget session session-slot-size)
                                          :permutation {} :change {} :freeze #{pos}})))))
          (let [p (aget item item-slot-current)
                x (aset item item-slot-current @ps)]
            (when-not (= p x)
              (when (call? slot)
                (socket-local-frame-up socket x)
                (when-not (identical? p item)
                  (socket-local-frame-down socket p)))
              (if-some [d (get diffs slot)]
                (aset socket socket-slot-message-diffs
                  (assoc! diffs slot (update d :change assoc pos x)))
                (do (aset socket socket-slot-message-change
                      (conj! (aget socket socket-slot-message-change) slot))
                    (aset socket socket-slot-message-diffs
                      (assoc! diffs slot {:grow 0 :shrink 0 :degree (aget session session-slot-size)
                                          :permutation {} :change {pos x} :freeze #{}}))))))))
      (if (nil? (aget item item-slot-event))
        (socket-terminate socket event)
        (try @ps (catch #?(:clj Throwable :cljs :default) _))))))

(defn session-upkeep [^objects session]
  (let [^longs pending (aget session session-slot-pending)
        prev-state (aget session session-slot-state)
        curr-state (aset session session-slot-state (call-state (aget session session-slot-request) 0))
        diff-state (bit-xor prev-state curr-state)
        prev-remote (half-state (aget pending 0) (pick prev-state 0))
        prev-local (half-state (aget pending 1) (pick prev-state 1))
        curr-remote (half-state (update-pending pending diff-state 0) (pick curr-state 0))
        curr-local (half-state (update-pending pending diff-state 1) (pick curr-state 1))]
    #_
    (when (check (signal-slot (aget session session-slot-signal)))
      (prn :session-upkeep
        prev-state curr-state diff-state
        [:local prev-local '-> curr-local]
        [:remote prev-remote '-> curr-remote]))
    (if (and (zero? curr-local) (zero? curr-remote))
      (do (session-cancel session)
          (session-detach session))
      (when (or (and (< curr-remote 2) (< curr-local prev-local))
              (and (< curr-local 2) (< curr-remote prev-remote)))
        (session-cancel session)
        (session-detach session)
        (session-spawn
          (aget session session-slot-socket)
          (aget session session-slot-signal)
          (aget session session-slot-store)
          (aget session session-slot-request)
          (aget session session-slot-pending))))))

(defn socket-upkeep [^objects socket]
  (loop [^objects session (aget socket socket-slot-updated)]
    (if (nil? session)
      (doto socket (aset socket-slot-updated nil))
      (let [next (aget session session-slot-updated)]
        (aset session session-slot-updated session)
        (when (signal-local? (aget session session-slot-signal))
          (session-retry session))
        (session-upkeep session)
        (recur next)))))

(defn socket-message [^objects socket msg]
  (let [msg ((aget socket socket-slot-reader) msg)
        [acks request change diffs freeze] msg]
    #_(apply prn (socket-site socket) '<- msg)
    (when-not (pure-ack? msg)
      (aset socket socket-slot-message-acks
        (inc (aget socket socket-slot-message-acks))))
    (dotimes [_ acks]
      (let [callback (socket-unacked-pull socket)]
        (reduce socket-request-toggle-ack socket (peek callback))
        (reduce socket-local-frame-ack socket (pop callback))))
    (reduce socket-request-toggle-remote socket request)
    (reduce input-append socket (map vector change diffs))
    (reduce input-freeze socket freeze)
    (socket-upkeep socket)))

(defn socket-emit [^objects socket msg]
  #_(apply prn (socket-site socket) '-> msg)
  (try ((aget socket socket-slot-writer) msg)
       (catch #?(:clj Throwable :cljs :default) e
         (if-some [ed (cond (::unserializable (ex-data e)) (ex-data e)
                            (::unserializable (ex-data (ex-cause e))) (ex-data (ex-cause e)))]
           (let [[_ _ slots _ _] msg
                 msg (->unserializable-msg slots (:v ed))]
             #?(:clj (log/debug msg))
             (throw (ex-info msg ed)))
           (throw e)))))

(defn socket-message-reset [^objects socket]
  (aset socket socket-slot-message-acks (+))
  (aset socket socket-slot-message-change (transient []))
  (aset socket socket-slot-message-diffs (transient {}))
  (aset socket socket-slot-message-freeze (transient []))
  (aset socket socket-slot-message-request (transient #{}))
  (aset socket socket-slot-message-frames (transient [])))

(defn socket-transfer [^objects socket]
  (when (pos? (aget socket socket-slot-pending))
    (when-some [event (get-sync socket socket-slot-sync)]
      (loop [event event]
        (cas-sync socket socket-slot-sync event nil
          (socket-consume socket event)
          (recur (get-sync socket socket-slot-sync))))))
  (socket-claim socket)
  (try (while (some? (aget socket socket-slot-head))
         (let [^objects event (socket-dequeue socket)]
           (case (aget event event-slot-type)
             :message (reduce socket-message socket @(aget event event-slot-process))
             :request (socket-request socket @(aget event event-slot-process))
             :output (socket-output socket event)
             :item (socket-item socket event))))
       (let [acks (aget socket socket-slot-message-acks)
             change (persistent! (aget socket socket-slot-message-change))
             diffs (persistent! (aget socket socket-slot-message-diffs))
             freeze (persistent! (aget socket socket-slot-message-freeze))
             request (persistent! (aget socket socket-slot-message-request))
             callback (persistent! (conj! (aget socket socket-slot-message-frames) request))
             msg [acks request change (into [] (map diffs) change) freeze]]
         (socket-message-reset socket)
         (socket-upkeep socket)
         (when-not (= empty-msg msg)
           (when-not (pure-ack? msg)
             (socket-unacked-push socket callback))
           (socket-emit socket msg)))
       (catch #?(:clj Throwable :cljs :default) e
         #_(pst e)
         (aset socket socket-slot-step nil)
         (socket-upkeep socket)
         (socket-cancel socket)
         (reduce run session-cancel (vals (aget socket socket-slot-sessions)))
         (aset socket socket-slot-sessions nil)
         (throw e))
       (finally
         (socket-yield socket)
         (socket-ready socket))))

(deftype Socket [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (socket-cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (socket-transfer state)))

(defn socket-writer [opts ^objects socket]
  (let [handlers (merge
                   (::t/write-handlers opts {})
                   {Slot    (t/write-handler
                              (fn [_] "slot")
                              (fn [^Slot slot]
                                [(.-frame slot) (.-id slot)]))
                    Frame   (t/write-handler
                              (fn [_] "frame")
                              (fn [^Frame frame]
                                (let [slot (.-slot frame)
                                      id (.-id frame)
                                      shared (aget socket socket-slot-shared)]
                                  [slot id
                                   (when-not (nil? slot)
                                     (when-not (contains? shared [slot id])
                                       (aset socket socket-slot-shared
                                         (assoc shared [slot id] frame))
                                       (.-ctor frame)))])))
                    Ap      (t/write-handler
                              (fn [_] "ap")
                              (fn [^Ap ap]
                                (.-inputs ap)))
                    Varargs (t/write-handler
                              (fn [_] "varargs")
                              (fn [^Varargs va] [(.-map? va) (.-inputs va)]))
                    ;; must wrap payload in vector, cf https://github.com/cognitect/transit-cljs/issues/23
                    Pure    (t/write-handler
                              (fn [_] "pure")
                              (fn [^Pure pure]
                                [(.-value pure)]))
                    Join    (t/write-handler
                              (fn [_] "join")
                              (fn [^Join join]
                                [(.-input join)]))
                    Id    (t/write-handler
                              (fn [_] "id")
                              (fn [^Id id]
                                [(.-x id)]))
                    Unbound (t/write-handler
                              (fn [_] "unbound")
                              (fn [^Unbound unbound]
                                [(.-key unbound)]))

                    ;; Workaround for unserializable values in e/for.
                    ;; It's OK to pass them as `nil` because they aren't used on the remote peer?
                    ;; Then why send them, you ask? Tech debt that is todo and harder to implement.
                    #?(:clj missionary.impl.Propagator$Publisher :cljs missionary.impl.Propagator/Publisher)
                    (t/write-handler (fn [_] "_") (fn [_])) ; _ is transit's nil tag
                    #?@(:clj [clojure.lang.PersistentQueue (t/write-handler (fn [_] "_") (fn [_]))])

                    ;; without this one can get `nil` or even `{}` on the other side, not an unserializable crash, ???
                    #?(:clj Object :cljs js/Object)
                    (t/write-handler
                      (fn [v] (throw (ex-info "unserializable" {:v v, ::unserializable true})))
                      (fn [v] (throw (ex-info "unserializable" {:v v, ::unserializable true}))))
                    })
        default (t/write-handler
                  (fn [v] (throw (ex-info "unserializable" {:v v, ::unserializable true})))
                  (fn [v] (throw (ex-info "unserializable" {:v v, ::unserializable true}))))]
    #?(:clj  (let [out (ByteArrayOutputStream.)
                   writer (t/writer out :json {:handlers handlers :default-handler default})]
               (fn [value] (.reset out) (t/write writer value) (.toString out)))
       :cljs (let [writer (t/writer :json {:handlers (assoc handlers :default default)})]
               (fn [value] (t/write writer value))))))

(defn socket-reader [opts ^Frame root ^objects socket]
  (let [opts
        {:handlers (merge
                     (::t/read-handlers opts {})
                     {"slot"           (t/read-handler
                                         (fn [[frame id]]
                                           (->Slot frame id)))
                      "frame"          (t/read-handler
                                         (fn [[slot id ctor]]
                                           (let [shared (aget socket socket-slot-shared)]
                                             (if (nil? ctor)
                                               (if (nil? slot)
                                                 root (get shared [slot id]))
                                               (let [frame (make-frame (frame-peer root) slot id (signal-site (slot-signal slot)) ctor)]
                                                 (aset socket socket-slot-shared
                                                   (assoc shared [slot id] frame)) frame)))))
                      "join"           (t/read-handler
                                         (fn [[input]]
                                           (->Join {} input nil)))
                      "id"             (t/read-handler
                                         (fn [[x]]
                                           (->Id x)))
                      "ap"             (t/read-handler
                                         (fn [inputs]
                                           (->Ap {} inputs nil)))
                      "varargs"        (t/read-handler
                                         (fn [[map? inputs]]
                                           (apply ->varargs map? inputs)))
                      "pure"           (t/read-handler
                                         (fn [[value]]
                                           (->Pure {} value nil)))
                      "unbound"        (t/read-handler
                                         (fn [[key]]
                                           (->Unbound key nil)))
                      "unserializable" (t/read-handler
                                         (fn [_]
                                           (->Failure :unserializable)))})}]
    #?(:clj (fn [^String s] (t/read (t/reader (ByteArrayInputStream. (.getBytes s)) :json opts)))
       :cljs (let [reader (t/reader :json opts)]
               (fn [string] (t/read reader string))))))

(defn slot-toggle [^Slot slot]
  (let [^Peer peer (.-peer (slot-frame slot))]
    ((.-request peer) #{slot})))

(defn signal-up [^Signal signal]
  (when (zero? #?(:clj  (.getAndIncrement ^AtomicInteger (.-request signal))
                  :cljs (let [n (.-request signal)] (set! (.-request signal) (inc n)) n)))
    (slot-toggle (signal-slot signal))))

(defn signal-down [^Signal signal]
  (when (zero? #?(:clj  (.decrementAndGet ^AtomicInteger (.-request signal))
                  :cljs (let [n (dec (.-request signal))] (set! (.-request signal) n) n)))
    (slot-toggle (signal-slot signal))))

(defn incseq
  ([expr]
   (m/sample {}
     (m/observe
       (fn [!] (! nil)
         (deps expr run signal-up)
         #(deps expr run signal-down)))
     expr))
  ;; compat, TODO change compiler
  ([_ expr] (incseq expr)))

(defn create-call [^Slot slot site expr]
  (let [^Frame parent (.-frame slot)
        ^Peer peer (.-peer parent)
        pexpr (peephole expr)]
    (make-signal slot site {}
      (persistent! (deps pexpr conj! (transient #{})))
      (i/latest-product (fn [ctor] (make-frame peer slot (peer-next-id peer) site ctor)) pexpr))))

(defn define-call
  "Defines call site id for given frame."
  [^Frame frame id expr]
  (let [^objects tags (.-tags frame)]
    (aset tags id (create-call (->Slot frame id)
                    (if-some [site ((.-calls (frame-cdef frame)) id)]
                      site (frame-site frame)) expr)) nil))

(defn lookup
  "Returns the value associated with given key in the dynamic environment of given frame."
  {:tag Expr}
  ([^Frame frame key]
   (lookup frame key (->Unbound key nil)))
  ([^Frame frame key nf]
   (loop [frame frame]
     (let [[_ _ _ env] (frame-ctor frame)]
       (if-some [s (env key)]
         s (if-some [^Slot slot (.-slot frame)]
             (recur (.-frame slot)) nf))))))

(defn ctor
  "Returns the constructor for cdef coordinates key and idx, with given free variables."
  [key idx & frees] [key idx (vec frees) {}])

(defn free
  "Returns the free variable id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (let [[_ _ free _] (frame-ctor frame)]
    (free id)))

(defn make-root [site defs main args]
  (let [peer (->Peer site defs (m/store sym-diff (sym-diff))
               #?(:clj (AtomicInteger. 0) :cljs 0))]
    (->> args
      (eduction (map pure))
      (apply dispatch "<root>" ((defs main)))
      (make-frame peer nil 0 :client))))

(defn root-socket [^Frame root opts subject]
  (m/eduction (remove nil?)
    (m/stream
      (fn [step done]
        (let [^Peer peer (frame-peer root)
              socket (object-array socket-slots)]
          (aset socket socket-slot-root root)
          (aset socket socket-slot-step step)
          (aset socket socket-slot-done done)
          (aset socket socket-slot-pending (+))
          (aset socket socket-slot-shared {})
          (aset socket socket-slot-sessions {})
          (aset socket socket-slot-dangling-frames (transient {}))
          (aset socket socket-slot-requested (transient {}))
          (aset socket socket-slot-writer (socket-writer opts socket))
          (aset socket socket-slot-reader (socket-reader opts root socket))
          (aset socket socket-slot-unacked-buffer (object-array 1))
          (aset socket socket-slot-unacked-head (+))
          (aset socket socket-slot-unacked-tail (+))
          (aset socket socket-slot-sync #?(:clj (AtomicReference. nil) :cljs nil))
          (socket-message-reset socket)
          (socket-claim socket)
          (aset socket socket-slot-message (socket-spawn socket :message (m/relieve into (m/zip vector (m/observe subject)))))
          (aset socket socket-slot-request (socket-spawn socket :request (.-request peer)))
          (socket-yield socket)
          (step) (->Socket socket))))))

#?(:clj
   (defmethod print-method Tag [^Tag tag ^Writer w]
     (.write w "#Tag[")
     (print-method (.-frame tag) w)
     (.write w " ")
     (print-method (.-index tag) w)
     (.write w "]"))
   :cljs
   (extend-protocol IPrintWithWriter
     Tag
     (-pr-writer [tag w o]
       (-write w "#Tag[")
       (-pr-writer (.-frame tag) w o)
       (-write w " ")
       (-write w (.-index tag))
       (-write w "]"))))

#?(:clj
   (defmethod print-method Slot [^Slot slot ^Writer w]
     (.write w "#Slot[")
     (print-method (.-frame slot) w)
     (.write w " ")
     (print-method (.-id slot) w)
     (.write w "]"))
   :cljs
   (extend-protocol IPrintWithWriter
     Slot
     (-pr-writer [slot w o]
       (-write w "#Slot[")
       (-pr-writer (.-frame slot) w o)
       (-write w " ")
       (-write w (.-id slot))
       (-write w "]"))))

#?(:clj
   (defmethod print-method Frame [^Frame frame ^Writer w]
     (.write w "#Frame[")
     (when-some [[x & xs] (seq (frame-path frame))]
       (print-method x w)
       (loop [xs xs]
         (when-some [[x & xs] xs]
           (.write w " ")
           (print-method x w)
           (recur xs))))
     (.write w "]"))
   :cljs
   (extend-protocol IPrintWithWriter
     Frame
     (-pr-writer [frame w o]
       (-write w "#Frame[")
       (when-some [[x & xs] (seq (frame-path frame))]
         (-write w x)
         (loop [xs xs]
           (when-some [[x & xs] xs]
             (-write w " ")
             (-write w x)
             (recur xs))))
       (-write w "]"))))

(defn get-destructure-map [gmap]
  (if (seq? gmap)
    (if (next gmap)
      (apply array-map gmap)
      (if (seq gmap) (first gmap) {}))
    gmap))

(defn cannot-resolve [& args] (throw (ex-info "definition called on a peer that doesn't support it" {:args args})))
(defn cannot-resolve-fn [sym]
  (throw (ex-info (str "I cannot resolve [" sym "], maybe it's only defined on the other peer?") {:sym sym})))

(defn tracing [info v] (print "[o_o]" info "=>> ") (prn v) v)
(defn case-default-required [] (throw (ex-info "case default required" {})))

(defn ->defs [mp]
  (loop [ret {}, left mp]
    (if-some [[k f] (first left)]
      (cond (ret k)
            (recur ret (dissoc left k))

            #?(:clj (instance? clojure.lang.Var$Unbound f) :cljs (undefined? f))
            (recur (assoc ret k nil) (dissoc left k))

            :else
            (recur (assoc ret k f) (merge (dissoc left k) (f :get :deps)))
            )
      #_(if (ret k)
        (recur ret (dissoc left k))
        (recur (assoc ret k f) (merge (dissoc left k) (when f (try (f :get :deps) (catch #?(:clj Throwable :cljs :default) _))))))
      ret)))

(defn frame-result [frame]
  (incseq (frame-result-slot frame)))

(defn sink [flow]
  (m/reduce (constantly nil) flow))

(defn rush [& tasks]
  (m/absolve (apply m/race (map m/attempt tasks))))

(defn boot-client-hot [defs main opts subject handler]
  (let [root (make-root :client defs main nil)]
    (rush (sink (frame-result root))
      (handler (root-socket root opts subject)))))

(defn boot-client-cold [defs main opts subject handler]
  (handler
    (m/ap
      (let [root (make-root :client defs main nil)
            socket (root-socket root opts subject)]
        (m/amb= (m/?> socket)
          (do (m/? (sink (frame-result root)))
              (m/amb)))))))

(defn do!*
  ([])
  ([a] a)
  ([_ b] b)
  ([_ _ c] c)
  ([_ _ _ d] d)
  ([_ _ _ _ e] e)
  ([_ _ _ _ _ f] f)
  ([_ _ _ _ _ _ g] g)
  ([_ _ _ _ _ _ _ & more] (last more)))

(defmacro do!
  "An internal optimization of `do`. Take any number of arguments and always return the last one.
  Strict constraint: all arguments except the last one must be exactly of size
  1 (incseq-wise). Any argument of size 0 will collapse the last argument to
  amb. Any argument of size > 1 will cause a cartesian product of the last
  argument. E.g. don't do this:
  (e/as-vec (do! (e/amb 1 2) 3)) := [3 3]

  The last argument can be of any size. "
  ([])
  ([a] a)
  ([a & bs] `(do!* ~a ~@bs)))

#?(:clj (defn all-vars "Return an eduction of all clojure vars in classpath." [] (->> (all-ns) (eduction (map ns-interns) (map vals) cat))))
#?(:clj (defn exported-evars "Return an eduction of all exported Electric fn vars" [] (eduction (filter #(:hyperfiddle.electric3/export (meta %))) (all-vars))))
