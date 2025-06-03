(ns hyperfiddle.electric.impl.runtime3
  (:refer-clojure :exclude [resolve *e])
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            #?(:cljs missionary.impl.Propagator)
            [clojure.pprint]
            #?(:clj [clojure.tools.logging :as log])
            [contrib.debug :as dbg]
            [contrib.data :as cd]
            [hyperfiddle.electric.impl.lang3 :as-alias lang]
            [cognitect.transit :as t]
            [hyperfiddle.incseq.diff-impl :as d]
            [contrib.assert :as ca]
            [clojure.string :as str]
            [hyperfiddle.electric.impl.dynamic-local :as dl]
            [hyperfiddle.electric.impl.missionary-util :as mu])
  (:import missionary.Cancelled
           #?(:clj missionary.impl.Propagator$Publisher)
           #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream Writer))
           #?(:clj (java.util.concurrent.locks ReentrantLock))))

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

(defn run
  ([f] (f) f)
  ([f x] (f x) f)
  ([f x y] (f x y) f)
  ([f x y & zs] (apply f x y zs) f))

(def peer-slot-busy 0)
(def peer-slot-root 1)                                      ;; the root frame
(def peer-slot-site 2)                                      ;; :client or :server
(def peer-slot-defs 3)
(def peer-slot-remote 4)
(def peer-slot-channel-ready 5)
(def peer-slots 6)

(def remote-slot-peer 0)
(def remote-slot-input 1)
(def remote-slot-channel 2)
(def remote-slot-inputs 3)                                  ;; hash map of remote ports currently pushed to local peer, indexed by port slot
(def remote-slot-outputs 4)                                 ;; hash map of local port subscriptions pushed to remote peer, indexed by port slot
(def remote-slot-ready 5)
(def remote-slot-current-event 6)
(def remote-slot-acks 7)
(def remote-slot-pos-request 8)
(def remote-slot-neg-request 9)
(def remote-slot-events 10)
(def remote-slots 11)

(def event-slot-prev 0)                                     ;; prev element in the DLCL
(def event-slot-next 1)                                     ;; next element in the DLCL
(def event-slot-inputs 2)
(def event-slot-outputs 3)
(def event-slots 4)

(def output-slot-remote 0)
(def output-slot-port 1)
(def output-slot-request-local 2)
(def output-slot-request-remote 3)
(def output-slot-pending 4)                                 ;; count of unacked toggles
(def output-slot-process 5)
(def output-slot-frozen 6)
(def output-slot-ready 7)
(def output-slots 8)

(def channel-slot-remote 0)
(def channel-slot-process 1)
(def channel-slot-busy 2)
(def channel-slot-over 3)
(def channel-slot-step 4)
(def channel-slot-done 5)
(def channel-slot-alive 6)
(def channel-slot-ready 7)
(def channel-slot-shared 8)
(def channel-slot-reader 9)
(def channel-slot-writer 10)
(def channel-slots 11)

(def port-slot-slot 0)
(def port-slot-site 1)
(def port-slot-deps 2)
(def port-slot-meta 3)
(def port-slot-flow 4)
(def port-slots 5)

(def input-slot-remote 0)                                   ;; this is only required for peer access
(def input-slot-port 1)
(def input-slot-diff 2)
(def input-slot-frozen 3)
(def input-slot-subs 4)
(def input-slot-request-local 5)
(def input-slot-request-remote 6)
(def input-slot-pending 7)
(def input-slots 8)

(def input-sub-slot-input 0)
(def input-sub-slot-step 1)
(def input-sub-slot-done 2)
(def input-sub-slot-prev 3)
(def input-sub-slot-next 4)
(def input-sub-slot-diff 5)
(def input-sub-slots 6)

(def call-slot-port 0)
(def call-slot-rank 1)
(def call-slots 2)

;; Pure | Ap | Join | Slot
(defprotocol Expr
  (deps [_ rf r site]) ;; emits ports
  (t [_])
  (peephole [_])
  (pp [_]))                                               ;; returns incseq

(extend-protocol Expr
  #?(:clj Object :cljs default) (t [_]) (peephole [this] this) (deps [_ _ r _] r) (pp [x] x)
  nil                           (t [_]) (peephole [this] this) (deps [_ _ r _] r) (pp [x] x))

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
  (deps [_ _ r _] r)
  (t [_] ::fixed)
  (peephole [this] (let [cf2 (peephole cf)] (if (= cf cf2) this (new Fixed cf2 nil))))
  (pp [_] (list 'Fixed (pp cf)))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((i/fixed cf) step done)))

(defn ->fixed [cf] (->Fixed cf nil))

(defn expr-deps [rf r site expr]
  (deps expr rf r site))

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
  (deps [_ _ r _] r)
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
  (deps [_ _ r _] r)
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
  (deps [_ _ r _] r)
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
  (deps [_ _ r _] r)
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
  (deps [_ _ r _] r)
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
           (println (ex-message clean-ex))
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
  (deps [_ rf r site]
    (reduce (fn [r x] (deps x rf r site)) r inputs))
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
  (deps [_ rf r site]
    (reduce (fn [r x] (deps x rf r site)) r inputs))
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
  (deps [_ rf r site]
    (reduce (fn [r x] (deps x rf r site)) r inputs))
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
  (deps [_ rf r site] (deps input rf r site))
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
  (m/latest (constantly (d/empty-diff 0)) incseq))

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
  (deps [_ _ r _] r)
  (t [_] ::unbound)
  (peephole [this] this)
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (step) (->Failer done (error (str "Unbound electric var lookup - " (pr-str key))))))

(deftype Cdef [frees nodes calls result build])

(def cdef ->Cdef)

(defprotocol PSlot
  (slot-port [slot]))

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

(defn peer-defs [^objects peer]
  (aget peer peer-slot-defs))

(defn peer-site [^objects peer]
  (aget peer peer-slot-site))

(defn peer-resolve [^objects peer key]
  (let [defs (peer-defs peer)]
    (when-not (contains? defs key) (throw (error (str (pr-str key) " not defined"))))
    (defs key)))

(defn peer-cdef
  "Returns the cdef of given constructor."
  {:tag Cdef}
  [^objects peer key idx]
  ((peer-resolve peer key) idx))

(defn port-flow [^objects port]
  (aget port port-slot-flow))

(defn port-deps [rf r ^objects port]
  (reduce-kv
    (fn [r port n]
      (reduce rf r (repeat n port)))
    r (aget port port-slot-deps)))

(declare incseq frame-result-slot)

(deftype Frame [peer slot rank site ctor ^objects nodes ^objects tags
                ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (-> (hash Frame)
                          (hash-combine (hash peer))
                          (hash-combine (hash slot))
                          (hash-combine (hash rank))))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Frame other)
      (= peer (.-peer ^Frame other))
      (= slot (.-slot ^Frame other))
      (= rank (.-rank ^Frame other))))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((incseq this (frame-result-slot this)) step done)))

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

(declare port-slot)

(defn enter [^objects peer]
  #?(:clj (let [^ReentrantLock r (aget peer peer-slot-busy)]
            (if (.isHeldByCurrentThread r)
              true (do (.lock r) false)))
     :cljs (if (aget peer peer-slot-busy)
             true (do (aset peer peer-slot-busy true) false))))

(defn exit [^objects peer busy]
  (when-not busy
    (let [c (aget peer peer-slot-channel-ready)]
      (aset peer peer-slot-channel-ready nil)
      #?(:clj  (.unlock ^ReentrantLock (aget peer peer-slot-busy))
         :cljs (aset peer peer-slot-busy false))
      (loop [^objects chan c]
        (when-not (nil? chan)
          (let [c (aget chan channel-slot-ready)]
            (aset chan channel-slot-ready nil)
            ((if-some [step (aget chan channel-slot-step)]
               step (aget chan channel-slot-done)))
            (recur c)))))))

(defn channel-notify [^objects channel]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
    (aset peer peer-slot-channel-ready channel)))

(defn channel-output-event [^objects channel]
  (when (identical? channel (aget channel channel-slot-ready))
    (channel-notify channel)))

(defn channel-terminated [^objects channel]
  (when (zero? (aset channel channel-slot-alive
                 (dec (aget channel channel-slot-alive))))
    (when (identical? channel (aget channel channel-slot-ready))
      (aset channel channel-slot-step nil)
      (channel-notify channel))))

(defn port-site [^objects port]
  (aget port port-slot-site))

(defn input-sub-cancel [^objects sub]
  (let [^objects input (aget sub input-sub-slot-input)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (if-some [^objects prv (aget sub input-sub-slot-prev)]
      (do (aset input input-slot-subs
            (when-not (identical? prv sub)
              (let [^objects nxt (aget sub input-sub-slot-next)]
                (aset prv input-sub-slot-next nxt)
                (aset nxt input-sub-slot-prev prv))))
          (aset sub input-sub-slot-prev nil)
          (aset sub input-sub-slot-next nil)
          (if (nil? (aget sub input-sub-slot-diff))
            (let [step (aget sub input-sub-slot-step)]
              (exit peer busy) (step))
            (do (aset sub input-sub-slot-diff nil)
                (exit peer busy))))
      (exit peer busy))))

(defn input-sub-transfer [^objects sub]
  (let [^objects input (aget sub input-sub-slot-input)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (if-some [diff (aget sub input-sub-slot-diff)]
      (do (aset sub input-sub-slot-diff nil)
          (if (nil? (aget sub input-sub-slot-prev))
            (let [done (aget sub input-sub-slot-done)]
              (aset sub input-sub-slot-step nil)
              (exit peer busy) (done))
            (exit peer busy)) diff)
      (let [done (aget sub input-sub-slot-done)]
        (aset sub input-sub-slot-step nil)
        (exit peer busy) (done)
        (throw (Cancelled. "Remote port cancelled."))))))

(deftype InputSub [sub]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (input-sub-cancel sub))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (input-sub-transfer sub)))

(deftype Slot [^Frame frame id]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [this]
    (hash (slot-port this)))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [this that]
    (and (instance? Slot that)
      (= (slot-port this) (slot-port that))))
  Expr
  (deps [this rf r site]
    (let [port (slot-port this)]
      (if (= site (port-site port))
        (port-deps rf r port)
        (rf r port))))
  (t [_] ::slot)
  (peephole [this] this)
  (pp [_] (list 'Slot id))
  PSlot
  (slot-port ^objects [slot]
    (let [id (.-id slot)
          ^Frame frame (.-frame slot)]
      (if (neg? id)
        (aget ^objects (.-nodes frame) (- -1 id))
        (aget ^objects (aget ^objects (.-tags frame) id) call-slot-port))))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((port-flow (slot-port this)) step done)))

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

(defn port-slot
  {:tag Slot}
  [^objects port]
  (aget port port-slot-slot))

(defn frame-path [^Frame frame]
  (loop [^Frame frame frame
         path ()]
    (if-some [^Slot slot (.-slot frame)]
      (recur (.-frame slot)
        (conj path [(.-id slot) (.-rank ^Frame frame)]))
      (vec path))))

(defn port-coordinates [^objects port]
  (let [slot (port-slot port)]
    [(frame-path (.-frame slot)) (.-id slot)]))

(defn slot-frame
  "Returns the frame of given slot."
  {:tag Frame}
  [^Slot slot]
  (.-frame slot))

(defn input-check-create [port]
  (let [slot (port-slot port)
        ^objects peer (frame-peer (slot-frame slot))
        ^objects remote (aget peer peer-slot-remote)]
    (if-some [^objects input (get (aget remote remote-slot-inputs) slot)]
      input (let [input (object-array input-slots)]
              (aset input input-slot-remote remote)
              (aset input input-slot-port port)
              (aset input input-slot-request-local (identity 0))
              (aset input input-slot-request-remote (identity 0))
              (aset input input-slot-pending (identity 0))
              (aset input input-slot-diff (i/empty-diff 0))
              (aset input input-slot-frozen false)
              (aset remote remote-slot-inputs
                (assoc (aget remote remote-slot-inputs) slot input))
              input))))

(defn input-sub [^objects port]
  (mu/wrap-incseq `input-sub
    (fn [step done]
      (let [^Slot slot (port-slot port)
            ^objects peer (frame-peer (.-frame slot))
            busy (enter peer)
            ^objects input (input-check-create port)
            sub (object-array input-sub-slots)]
        (aset sub input-sub-slot-input input)
        (aset sub input-sub-slot-step step)
        (aset sub input-sub-slot-done done)
        (aset sub input-sub-slot-diff (aget input input-slot-diff))
        (when-not (aget input input-slot-frozen)
          (if-some [^objects prv (aget input input-slot-subs)]
            (let [^objects nxt (aget prv input-sub-slot-next)]
              (aset prv input-sub-slot-next sub)
              (aset nxt input-sub-slot-prev sub)
              (aset sub input-sub-slot-prev prv)
              (aset sub input-sub-slot-next nxt))
            (do (aset input input-slot-subs sub)
                (aset sub input-sub-slot-prev sub)
                (aset sub input-sub-slot-next sub))))
        (exit peer busy) (step) (->InputSub sub)))))

(defn make-port [^Slot slot site mt deps flow]
  (let [port (object-array port-slots)
        peer (frame-peer (.-frame slot))]
    (aset port port-slot-slot slot)
    (aset port port-slot-site site)
    (aset port port-slot-deps deps)
    (aset port port-slot-meta mt)
    (aset port port-slot-flow
      (if (= site (peer-site peer))
        (m/signal i/combine flow)
        (input-sub port)))
    port))

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
        port (if (instance? Slot expr)
               (slot-port expr)
               (let [pexpr (peephole expr)
                     iexpr (->is pexpr)]
                 (make-port slot site mt
                   (deps iexpr update-inc {} site)
                   iexpr)))]
    (aset ^objects (.-nodes frame) (- -1 id) port) nil))

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

(defn make-frame [^objects peer ^Slot slot rank site ctor]
  (let [[key idx _ _] ctor
        cdef (peer-cdef peer key idx)
        nodec (count (.-nodes cdef))
        callc (count (.-calls cdef))
        frame (->Frame peer slot rank site ctor
                (object-array (inc nodec)) (object-array callc) nil)
        built ((.-build cdef) frame)]
    (define-slot (->Slot frame (- -1 nodec)) {} built) frame))

(defn input-dispose [^objects input]
  (let [^objects remote (aget input input-slot-remote)]
    (aset remote remote-slot-inputs
      (dissoc (aget remote remote-slot-inputs)
        (port-slot (aget input input-slot-port))))))

(defn output-enqueue [^objects output]
  (let [^objects remote (aget output output-slot-remote)
        ^objects channel (aget remote remote-slot-channel)]
    (aset output output-slot-ready (aget remote remote-slot-ready))
    (aset remote remote-slot-ready output)
    (channel-output-event channel)))

(defn channel-output-sub [^objects channel ^objects output]
  (aset channel channel-slot-alive
    (inc (aget channel channel-slot-alive)))
  (aset output output-slot-process
    ((port-flow (aget output output-slot-port))
     #(let [^objects remote (aget output output-slot-remote)
            ^objects peer (aget remote remote-slot-peer)
            busy (enter peer)]
        (if (nil? (aget output output-slot-port))
          (try @(aget output output-slot-process)
               (catch #?(:clj Throwable :cljs :default) _))
          (output-enqueue output))
        (exit peer busy))
     #(let [^objects remote (aget output output-slot-remote)
            ^objects peer (aget remote remote-slot-peer)
            busy (enter peer)]
        (aset output output-slot-frozen true)
        (if (nil? (aget output output-slot-port))
          (channel-terminated channel)
          (output-enqueue output))
        (exit peer busy))))
  channel)

(defn output-dispose [^objects output]
  (let [^objects remote (aget output output-slot-remote)]
    (aset remote remote-slot-outputs
      (dissoc (aget remote remote-slot-outputs)
        (port-slot (aget output output-slot-port))))
    (aset output output-slot-port nil)
    ((aget output output-slot-process))))

(defn reset-diff [n]
  {:grow        0,
   :degree      n,
   :shrink      n,
   :permutation {},
   :change      {},
   :freeze      #{}})

(defn input-reset [^objects input]
  (when-some [^objects sub (aget input input-slot-subs)]
    (loop [^objects s sub]
      (if-some [{:keys [grow degree]} (aget s input-sub-slot-diff)]
        (aset s input-sub-slot-diff (reset-diff (- degree grow)))
        (let [step (aget s input-sub-slot-step)]
          (aset s input-sub-slot-diff (reset-diff (:degree (aget input input-slot-diff))))
          (step)))
      (let [n (aget s input-sub-slot-next)]
        (when-not (identical? n sub) (recur n)))))
  (aset input input-slot-diff (i/empty-diff 0)))

(defn output-check-create [^objects port]
  (let [slot (port-slot port)
        ^objects peer (frame-peer (slot-frame slot))
        ^objects remote (aget peer peer-slot-remote)
        outputs (aget remote remote-slot-outputs)]
    (if-some [output (get outputs slot)]
      output (let [output (object-array output-slots)]
               (aset output output-slot-remote remote)
               (aset output output-slot-port port)
               (aset output output-slot-request-local (identity 0))
               (aset output output-slot-request-remote (identity 0))
               (aset output output-slot-pending (identity 0))
               (aset output output-slot-frozen false)
               (aset remote remote-slot-outputs (assoc outputs slot output))
               (when-some [channel (aget remote remote-slot-channel)]
                 (channel-output-sub channel output))
               output))))

(defn output-reset [^objects output]
  (let [^objects remote (aget output output-slot-remote)
        port (aget output output-slot-port)
        o (object-array output-slots)]
    (aset o output-slot-remote remote)
    (aset o output-slot-port port)
    (aset o output-slot-request-local (aget output output-slot-request-local))
    (aset o output-slot-request-remote (aget output output-slot-request-remote))
    (aset o output-slot-pending (aget output output-slot-pending))
    (aset o output-slot-frozen false)
    (aset output output-slot-port nil)
    ((aget output output-slot-process))
    (aset remote remote-slot-outputs
      (assoc (aget remote remote-slot-outputs)
        (port-slot port) o))
    (when-some [channel (aget remote remote-slot-channel)]
      (channel-output-sub channel o))))

(defn zero ^long [^long r ^long x]
  (if (zero? r) 0 x))

(defn input-state ^long [^objects input]
  (-> (bit-and (aget input input-slot-pending) 2r001)
    (bit-xor (zero (aget input input-slot-request-local) 2r001))
    (bit-or (zero (aget input input-slot-request-remote) 2r010))
    (bit-or (zero (aget input input-slot-pending) 2r100))))

(defn output-state ^long [^objects output]
  (-> (bit-and (aget output output-slot-pending) 2r001)
    (bit-xor (zero (aget output output-slot-request-local) 2r001))
    (bit-or (zero (aget output output-slot-request-remote) 2r010))
    (bit-or (zero (aget output output-slot-pending) 2r100))))

(defn input-ack [^objects remote slot]
  (let [^objects input (get (aget remote remote-slot-inputs) slot)]
    (aset input input-slot-pending (dec (aget input input-slot-pending)))
    (case (input-state input)
      2r000 (input-dispose input)
      2r100 (input-reset input)
      nil)
    remote))

(defn output-ack [^objects remote slot]
  (let [^objects output (get (aget remote remote-slot-outputs) slot)]
    (aset output output-slot-pending (dec (aget output output-slot-pending)))
    (when (identical? output (aget output output-slot-ready))
      (output-enqueue output))
    (case (output-state output)
      2r000 (output-dispose output)
      2r100 (output-reset output)
      nil)
    remote))

(defn remote-ack [^objects remote]
  (let [^objects tail (aget remote remote-slot-current-event)
        ^objects event (aget tail event-slot-next)
        ^objects head (aget event event-slot-next)]
    (when (identical? tail event) (throw (ex-info "Unexpected ack." {})))
    (aset tail event-slot-next head)
    (aset head event-slot-prev tail)
    (reduce input-ack remote (aget event event-slot-inputs))
    (reduce output-ack remote (aget event event-slot-outputs))))

(defn remote-change [^objects remote ^Slot slot diff]
  (when-some [^objects input (get (aget remote remote-slot-inputs) slot)]
    (when-not (== 2r100 (input-state input))
      (aset input input-slot-diff (i/combine (aget input input-slot-diff) diff))
      (when-some [^objects sub (aget input input-slot-subs)]
        (loop [^objects s sub]
          (if-some [prev (aget s input-sub-slot-diff)]
            (aset s input-sub-slot-diff (i/combine prev diff))
            (let [step (aget s input-sub-slot-step)]
              (aset s input-sub-slot-diff diff)
              ;; TODO this can nullify slot-next
              (step)))
          (let [n (aget s input-sub-slot-next)]
            (when-not (identical? n sub) (recur n)))))))
  remote)

(defn remote-freeze [^objects remote ^Slot slot]
  (when-some [^objects input (get (aget remote remote-slot-inputs) slot)]
    (when-not (== 2r100 (input-state input))
      (aset input input-slot-frozen true)
      (when-some [^objects sub (aget input input-slot-subs)]
        (aset input input-slot-subs nil)
        (loop [^objects s sub]
          (when (nil? (aget s input-sub-slot-diff))
            (let [done (aget s input-sub-slot-done)]
              (aset s input-sub-slot-step nil) (done)))
          (let [n (aget s input-sub-slot-next)]
            (aset s input-sub-slot-next nil)
            (aset s input-sub-slot-prev nil)
            (when-not (identical? n sub) (recur n)))))))
  remote)

(defn remote-inc-request [^objects remote slot]
  (let [pos-request (aget remote remote-slot-pos-request)
        neg-request (aget remote remote-slot-neg-request)]
    (if-some [r (get pos-request slot)]
      (aset remote remote-slot-pos-request
        (assoc! pos-request slot (inc r)))
      (if-some [r (get neg-request slot)]
        (if (= 1 r)
          (aset remote remote-slot-neg-request
            (dissoc! neg-request slot))
          (aset remote remote-slot-neg-request
            (assoc! neg-request slot (dec r))))
        (aset remote remote-slot-pos-request
          (assoc! pos-request slot 1))))))

(defn remote-dec-request [^objects remote slot]
  (let [pos-request (aget remote remote-slot-pos-request)
        neg-request (aget remote remote-slot-neg-request)]
    (if-some [r (get neg-request slot)]
      (aset remote remote-slot-neg-request
        (assoc! neg-request slot (inc r)))
      (if-some [r (get pos-request slot)]
        (if (= 1 r)
          (aset remote remote-slot-pos-request
            (dissoc! pos-request slot))
          (aset remote remote-slot-pos-request
            (assoc! pos-request slot (dec r))))
        (aset remote remote-slot-neg-request
          (assoc! neg-request slot 1))))))

(defn input-crash [^objects input]
  (aset input input-slot-request-remote (identity 0))
  (aset input input-slot-pending (identity 0))
  (if (zero? (aget input input-slot-request-local))
    (input-dispose input)
    (do ;; TODO do not request transitive deps
        (remote-inc-request (aget input input-slot-remote)
          (port-slot (aget input input-slot-port)))
        (input-reset input))))

(defn output-crash [^objects output]
  (aset output output-slot-request-remote (identity 0))
  (aset output output-slot-pending (identity 0))
  (if (zero? (aget output output-slot-request-local))
    (output-dispose output)
    (do ;; TODO do not request transitive deps
        (remote-inc-request (aget output output-slot-remote)
          (port-slot (aget output output-slot-port)))
        (output-reset output))))

(defn channel-cancel [^objects channel]
  ((aget channel channel-slot-process)))

(defn channel-crash [^objects channel]
  (let [^objects remote (aget channel channel-slot-remote)]
    (aset remote remote-slot-channel nil)
    (aset remote remote-slot-acks (identity 0))
    (aset remote remote-slot-pos-request (transient {}))
    (aset remote remote-slot-neg-request (transient {}))
    (let [^objects tail (aget remote remote-slot-current-event)]
      (aset tail event-slot-prev tail)
      (aset tail event-slot-next tail))
    (reduce run input-crash (vals (aget remote remote-slot-inputs)))
    (reduce run output-crash (vals (aget remote remote-slot-outputs)))
    (loop []
      (when-some [^objects output (aget remote remote-slot-ready)]
        (aset remote remote-slot-ready (aget output output-slot-ready))
        (aset output output-slot-ready nil)
        (if (aget output output-slot-frozen)
          (aset channel channel-slot-alive
            (dec (aget channel channel-slot-alive)))
          (try @(aget output output-slot-process)
               (catch #?(:clj Throwable :cljs :default) _)))
        (recur)))
    (channel-cancel channel)))

(defn output-local-toggle [^objects output]
  (let [^objects remote (aget output output-slot-remote)
        ^objects event (aget remote remote-slot-current-event)
        slot (port-slot (aget output output-slot-port))
        outputs (aget event event-slot-outputs)]
    (aset event event-slot-outputs
      ((if (contains? outputs slot)
         (do (output-ack remote slot) disj!)
         (do (aset output output-slot-pending
               (inc (aget output output-slot-pending)))
             conj!)) outputs slot))))

(defn input-local-toggle [^objects input]
  (let [^objects remote (aget input input-slot-remote)
        ^objects event (aget remote remote-slot-current-event)
        slot (port-slot (aget input input-slot-port))
        inputs (aget event event-slot-inputs)]
    (aset event event-slot-inputs
      ((if (contains? inputs slot)
         (do (input-ack remote slot) disj!)
         (do (aset input input-slot-pending
               (inc (aget input input-slot-pending)))
             conj!)) inputs slot))))

(declare input-remote-up input-remote-down input-local-up input-local-down)

(defn output-local-up [^objects port]
  (let [^objects output (output-check-create port)
        request (aget output output-slot-request-local)]
    (aset output output-slot-request-local (inc request))
    (when (zero? request)
      (output-local-toggle output)
      (port-deps run input-local-up port))))

(defn output-local-down [^objects port]
  (let [^objects output (output-check-create port)
        request (dec (aget output output-slot-request-local))]
    (aset output output-slot-request-local request)
    (when (zero? request)
      (output-local-toggle output)
      (port-deps run input-local-down port))))

(defn input-local-up [^objects port]
  (let [^objects input (input-check-create port)
        request (aget input input-slot-request-local)]
    (aset input input-slot-request-local (inc request))
    (when (zero? request)
      (input-local-toggle input)
      (port-deps run output-local-up port))))

(defn input-local-down [^objects port]
  (let [^objects input (input-check-create port)
        request (dec (aget input input-slot-request-local))]
    (aset input input-slot-request-local request)
    (when (zero? request)
      (input-local-toggle input)
      (port-deps run output-local-down port))))

(defn output-remote-down [^objects port]
  (let [^objects output (output-check-create port)
        request (dec (aget output output-slot-request-remote))]
    (aset output output-slot-request-remote request)
    (when (identical? output (aget output output-slot-ready))
      (output-enqueue output))
    (case (output-state output)
      2r000 (output-dispose output)
      nil)
    (when (zero? request)
      (port-deps run input-remote-down port))))

(defn output-remote-up [^objects port]
  (let [^objects output (output-check-create port)
        request (aget output output-slot-request-remote)]
    (aset output output-slot-request-remote (inc request))
    (when (identical? output (aget output output-slot-ready))
      (output-enqueue output))
    (when (zero? request)
      (port-deps run input-remote-up port))))

(defn input-remote-down [^objects port]
  (let [^objects input (input-check-create port)
        request (dec (aget input input-slot-request-remote))]
    (aset input input-slot-request-remote request)
    (case (input-state input)
      2r000 (input-dispose input)
      nil)
    (when (zero? request)
      (port-deps run output-remote-down port))))

(defn input-remote-up [^objects port]
  (let [^objects input (input-check-create port)
        request (aget input input-slot-request-remote)]
    (aset input input-slot-request-remote (inc request))
    (when (zero? request)
      (when (zero? (aget input input-slot-request-local))
        (when (even? (aget input input-slot-pending))
          ;; spawn input
          ))
      (port-deps run output-remote-up port))))

(defn output-update-pos-request [slot d]
  (dotimes [_ d]
    (output-remote-up (slot-port slot))))

(defn output-update-neg-request [slot d]
  (dotimes [_ d]
    (output-remote-down (slot-port slot))))

(defn ->unserializable-msg [port* d]
  (let [mt* (mapv #(aget ^objects % port-slot-meta) (persistent! port*))
        has-mt* (filterv ::lang/line mt*)
        msg (str (when (seq has-mt*)
                   (str "[unserializable] Possible values (if let-bound search for their usage):\n"
                     (str/join "\n" (eduction (map clean-msg) (distinct) has-mt*))
                     (when (not= (count mt*) (count has-mt*))
                       (str "\nThe value list is incomplete."))
                     "\n"))
              "Value: " d)]
    msg))

(defn channel-transfer-exit [^objects channel busy]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (if (zero? (aget channel channel-slot-alive))
      (do (aset channel channel-slot-step nil)
          (channel-notify channel))
      (aset channel channel-slot-ready channel))
    (exit peer busy)))

(defn channel-transfer [^objects channel]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (when-not (identical? channel (aget remote remote-slot-channel))
      (let [e (aget channel channel-slot-shared)]
        (aset channel channel-slot-shared nil)
        (channel-transfer-exit channel busy)
        (throw e)))
    (try (loop [port* (transient #{})
                change (transient {})
                freeze (transient #{})]
           (if-some [^objects output (aget remote remote-slot-ready)]
             (let [ps (aget output output-slot-process)]
               (aset remote remote-slot-ready (aget output output-slot-ready))
               (case (output-state output)
                 2r101 (do (aset output output-slot-ready output)
                           (recur port* change freeze))
                 (do (aset output output-slot-ready nil)
                     (if-some [port (aget output output-slot-port)]
                       (let [slot (port-slot port)]
                         (if (aget output output-slot-frozen)
                           (do (aset channel channel-slot-alive
                                 (dec (aget channel channel-slot-alive)))
                               (recur port* change (conj! freeze slot)))
                           (recur (conj! port* port)
                             (let [diff @ps]
                               (if (i/empty-diff? diff)
                                 change (assoc! change
                                          slot (if-some [p (get change slot)]
                                                 (i/combine p diff) diff)))) freeze)))
                       (do (try @ps (catch #?(:clj Throwable :cljs :default) _))
                           (recur port* change freeze))))))
             (let [change (persistent! change)
                   freeze (persistent! freeze)
                   acks (aget remote remote-slot-acks)
                   pos-request (persistent! (aget remote remote-slot-pos-request))
                   neg-request (persistent! (aget remote remote-slot-neg-request))
                   changeset (-> (unchecked-add (count change) (count freeze))
                               (unchecked-add (count pos-request))
                               (unchecked-add (count neg-request)))]
               (aset remote remote-slot-acks (identity 0))
               (aset remote remote-slot-pos-request (transient {}))
               (aset remote remote-slot-neg-request (transient {}))
               (when (pos? changeset)
                 (let [^objects event (aget remote remote-slot-current-event)
                       ^objects head (aget event event-slot-next)
                       ^objects tail (object-array event-slots)]
                   (aset event event-slot-inputs
                     (persistent! (aget event event-slot-inputs)))
                   (aset event event-slot-outputs
                     (persistent! (aget event event-slot-outputs)))
                   (aset remote remote-slot-current-event tail)
                   (aset event event-slot-next tail)
                   (aset head event-slot-prev tail)
                   (aset tail event-slot-next head)
                   (aset tail event-slot-prev event)
                   (aset tail event-slot-inputs (transient #{}))
                   (aset tail event-slot-outputs (transient #{}))))
               (when (pos? (unchecked-add acks changeset))
                 (try ((aget channel channel-slot-writer) [acks pos-request neg-request change freeze])
                      (catch #?(:clj Throwable :cljs :default) e
                        (if-some [ed (cond (::unserializable (ex-data e)) (ex-data e)
                                           (::unserializable (ex-data (ex-cause e))) (ex-data (ex-cause e)))]
                          (let [msg (->unserializable-msg port* (:v ed))]
                            #?(:clj (log/debug msg))
                            (throw (ex-info msg ed)))
                          (throw e))))))))
         (catch #?(:clj Throwable :cljs :default) e
           (channel-crash channel)
           (throw e))
         (finally (channel-transfer-exit channel busy)))))

(defn channel-ready [^objects channel busy]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (while (aset channel channel-slot-busy
             (not (aget channel channel-slot-busy)))
      (if (aget channel channel-slot-over)
        (channel-terminated channel)
        (if (identical? channel (aget remote remote-slot-channel))
          (try
            (run! #(let [[acks pos-request neg-request change freeze]
                         ((aget channel channel-slot-reader) %)]
                    (dotimes [_ acks] (remote-ack remote))
                    (reduce-kv run output-update-pos-request pos-request)
                    (reduce-kv run output-update-neg-request neg-request)
                    (reduce-kv remote-change remote change)
                    (reduce remote-freeze remote freeze)
                    (when (pos? (-> (unchecked-add (count change) (count freeze))
                                  (unchecked-add (count pos-request))
                                  (unchecked-add (count neg-request))))
                      (aset remote remote-slot-acks
                        (inc (aget remote remote-slot-acks)))
                      (channel-output-event channel)))
              @(aget channel channel-slot-process))
            (catch #?(:clj Throwable :cljs :default) e
              (channel-crash channel)
              (aset channel channel-slot-shared e)
              (channel-output-event channel)))
          (try @(aget channel channel-slot-process)
               (catch #?(:clj Throwable :cljs :default) _)))))
    (exit peer busy)))

(deftype Channel [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (channel-cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (channel-transfer state)))

(defn channel-writer [opts ^objects channel]
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
                                      rank (.-rank frame)
                                      shared (aget channel channel-slot-shared)]
                                  [slot rank
                                   (when-not (nil? slot)
                                     (when-not (contains? shared [slot rank])
                                       (aset channel channel-slot-shared
                                         (assoc shared [slot rank] frame))
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
                    })
        default (t/write-handler
                  (fn [v] (throw (ex-info "unserializable" {:v v, ::unserializable true})))
                  (fn [_]))]
    #?(:clj  (let [out (ByteArrayOutputStream.)
                   writer (t/writer out :json {:handlers handlers :default-handler default})]
               (fn [value] (.reset out) (t/write writer value) (.toString out)))
       :cljs (let [writer (t/writer :json {:handlers (assoc handlers :default default)})]
               (fn [value] (t/write writer value))))))

(defn channel-reader [opts ^objects channel]
  (let [opts
        {:handlers (merge
                     (::t/read-handlers opts {})
                     {"slot"           (t/read-handler
                                         (fn [[frame id]]
                                           (->Slot frame id)))
                      "frame"          (t/read-handler
                                         (fn [[slot rank ctor]]
                                           (let [^objects remote (aget channel channel-slot-remote)
                                                 ^objects peer (aget remote remote-slot-peer)
                                                 shared (aget channel channel-slot-shared)]
                                             (if (nil? ctor)
                                               (if (nil? slot)
                                                 (aget peer peer-slot-root)
                                                 (get shared [slot rank]))
                                               (let [frame (make-frame peer slot rank (port-site (slot-port slot)) ctor)]
                                                 (aset channel channel-slot-shared
                                                   (assoc shared [slot rank] frame)) frame)))))
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

(defn peer-events [^objects peer]
  (let [^objects remote (aget peer peer-slot-remote)]
    (aget remote remote-slot-events)))

(defn peer-root [^objects peer]
  (aget peer peer-slot-root))

(defn input-attach [^objects port]
  (let [^objects input (input-check-create port)
        ^objects remote (aget input input-slot-remote)]
    (input-local-up port)
    (remote-inc-request remote (port-slot port))
    (when-some [^objects channel (aget remote remote-slot-channel)]
      (channel-output-event channel))))

(defn input-detach [^objects port]
  (let [^objects input (input-check-create port)
        ^objects remote (aget input input-slot-remote)]
    (input-local-down port)
    (remote-dec-request remote (port-slot port))
    (when-some [^objects channel (aget remote remote-slot-channel)]
      (channel-output-event channel))))

(defn attach-deps [peer expr]
  (let [busy (enter peer)]
    (deps expr run input-attach (peer-site peer))
    (exit peer busy)))

(defn detach-deps [peer expr]
  (let [busy (enter peer)]
    (deps expr run input-detach (peer-site peer))
    (exit peer busy)))

(defn incseq [^Frame frame expr]
  (let [peer (frame-peer frame)]
    (mu/wrap-incseq `incseq
      (m/sample {}
        (m/observe
          (fn [!] (! nil)
            (attach-deps peer expr)
            #(detach-deps peer expr)))
        expr))))

(defn frame-result-slot [^Frame frame]
  (let [^objects nodes (.-nodes frame)]
    (node frame (dec (alength nodes)))))

(defn apply-cycle [^objects buffer cycle]
  (let [i (nth cycle 0)
        x (aget buffer i)
        j (loop [i i
                 k 1]
            (let [j (nth cycle k)
                  y (aget buffer j)
                  k (unchecked-inc-int k)]
              (aset buffer i y)
              (if (< k (count cycle))
                (recur j k) j)))]
    (aset buffer j x) buffer))

(defn create-call [^Slot slot site expr]
  (let [^Frame parent (.-frame slot)
        ^objects peer (.-peer parent)
        call (object-array call-slots)
        expr (ap {}
               (pure (fn [ctor]
                       (let [rank (aget call call-slot-rank)
                             frame (make-frame peer slot rank site ctor)]
                         (aset call call-slot-rank (inc rank)) frame)))
               expr)
        pexpr (peephole expr)]
    #_(if (not= expr pexpr)
      (do
        (prn 'call-before) (clojure.pprint/pprint (as-stats expr))
        (prn 'call-after) (clojure.pprint/pprint (as-stats pexpr)))
      (do (prn 'call) (clojure.pprint/pprint (as-stats pexpr))))
    (aset call call-slot-port
      (make-port slot site {}
        (deps pexpr update-inc {} site)
        pexpr
        #_(i/latest-product
          (fn [ctor]
            (let [rank (aget call call-slot-rank)
                  frame (make-frame peer slot rank site ctor)]
              (aset call call-slot-rank (inc rank)) frame))
            expr)))
    (aset call call-slot-rank (identity 0))
    call))

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

(defn batch [sg init batch-end flow]
  (m/ap
    (let [!state (atom init)]
      (swap! !state sg (m/?< flow))
      (try
        (m/? batch-end)
        (first (reset-vals! !state init))
        (catch Cancelled _ (m/amb))))))

#?(:cljs
   (defn next-animation-frame []
     (fn [s f]
       (let [raf (.requestAnimationFrame js/window s)]
         (fn [] (.cancelAnimationFrame js/window raf)
           (f (Cancelled.)))))))

(defn make-peer* [site opts events defs main args]
  (let [^objects peer (object-array peer-slots)
        ^objects remote (object-array remote-slots)
        events (m/stream events)]
    (aset peer peer-slot-busy #?(:clj (ReentrantLock.) :cljs false))
    (aset peer peer-slot-remote remote)
    (aset peer peer-slot-site site)
    (aset peer peer-slot-defs defs)
    (aset remote remote-slot-peer peer)
    (aset remote remote-slot-inputs {})
    (aset remote remote-slot-outputs {})
    (aset remote remote-slot-acks (identity 0))
    (aset remote remote-slot-pos-request (transient {}))
    (aset remote remote-slot-neg-request (transient {}))
    (aset remote remote-slot-current-event
      (let [e (object-array event-slots)]
        (aset e event-slot-prev e)
        (aset e event-slot-next e)
        (aset e event-slot-inputs (transient #{}))
        (aset e event-slot-outputs (transient #{}))
        e))
    (aset peer peer-slot-root
      (->> args
        (eduction (map pure))
        (apply dispatch "<root>" ((defs main)))
        (make-frame peer nil 0 :client)))
    (aset remote remote-slot-events
      (mu/wrap-uninitialized `remote-slot-events-stream-eduction
        (m/eduction (remove nil?)
          (m/stream
            (mu/wrap-uninitialized `remote-slot-events
              (fn [step done]
                (let [busy (enter peer)
                      ^objects remote (aget peer peer-slot-remote)]
                  (if (nil? (aget remote remote-slot-channel))
                    (let [channel (object-array channel-slots)]
                      (aset remote remote-slot-channel channel)
                      (aset channel channel-slot-remote remote)
                      (aset channel channel-slot-step step)
                      (aset channel channel-slot-done done)
                      (aset channel channel-slot-busy true)
                      (aset channel channel-slot-over false)
                      (aset channel channel-slot-alive (identity 1))
                      (aset channel channel-slot-shared {})
                      (aset channel channel-slot-writer (channel-writer opts channel))
                      (aset channel channel-slot-reader (channel-reader opts channel))
                      (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
                      (aset peer peer-slot-channel-ready channel)
                      (aset channel channel-slot-process
                        (events
                          #(let [^objects remote (aget channel channel-slot-remote)]
                             (channel-ready channel (enter (aget remote remote-slot-peer))))
                          #(let [^objects remote (aget channel channel-slot-remote)]
                             (aset channel channel-slot-over true)
                             (channel-ready channel (enter (aget remote remote-slot-peer))))))
                      (reduce channel-output-sub channel (vals (aget remote remote-slot-outputs)))
                      (channel-ready channel busy)
                      (->Channel channel))
                    (do (exit peer busy) (step)
                        (->Failer done (error "Can't connect - remote already up.")))))))))))
    peer))

(defn make-peer
  "Returns a new peer instance for given site, from given definitions and main key and optional extra arguments to the
entrypoint.
"
  [site opts subject defs main args]
  (make-peer* site opts (batch conj [] (m/sleep 1) (m/observe subject)) defs main args))

(defn subject-at [^objects arr slot]
  (fn [!] (aset arr slot !) #(aset arr slot nil)))

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

(defn peer-sink [peer]
  (m/reduce (constantly nil) (peer-root peer)))

(defn peer-boot [peer handler]
  (m/reduce (comp reduced {}) nil
    (m/ap
      (m/amb= (m/? (handler (peer-events peer)))
        (m/? (peer-sink peer))))))

(defn boot-client-hot [defs main opts subject handler]
  (peer-boot (make-peer :client opts subject defs main nil) handler))

(defn boot-client-cold [defs main opts subject handler]
  (handler
    (m/ap
      (let [peer (make-peer :client opts subject defs main nil)]
        (m/amb= (m/?> (peer-events peer))
          (do (m/? (peer-sink peer))
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
