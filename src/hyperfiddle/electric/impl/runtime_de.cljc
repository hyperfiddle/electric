(ns hyperfiddle.electric.impl.runtime-de
  (:refer-clojure :exclude [resolve])
  (:require [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.arrays-impl :as a]
            [contrib.assert :as ca]
            [contrib.debug]
            [missionary.core :as m]
            [cognitect.transit :as t])
  (:import missionary.Cancelled
           #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream Writer))
           #?(:clj (java.util.concurrent.locks ReentrantLock))))

#?(:clj (set! *warn-on-reflection* true))

(def reclaim
  "Returns a fresh object. When the object is reclaimed by GC, provided function is called with no argument."
  #?(:cljs
     (let [registry (js/FinalizationRegistry. #(%))]
       (fn [f] (let [obj (js-obj)] (.register registry obj f) obj)))
     :clj (fn [f] (reify Object (finalize [_] (f))))))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (.error js/console e)))

(def peer-slot-busy 0)
(def peer-slot-root 1)
(def peer-slot-site 2)
(def peer-slot-defs 3)
(def peer-slot-remote 4)
(def peer-slot-sub-ready 7)
(def peer-slot-channel-ready 8)
(def peer-slots 9)

(def remote-slot-peer 0)
(def remote-slot-events 1)
(def remote-slot-channel 2)
(def remote-slot-inputs 3)                                  ;; hash map of remote ports currently pushed to local peer, indexed by port slot
(def remote-slot-outputs 4)                                 ;; hash map of local port subscriptions pushed to remote peer, indexed by port slot
(def remote-slots 5)

(def output-slot-remote 0)
(def output-slot-port 1)
(def output-slot-sub 2)
(def output-slot-requested 3)
(def output-slot-refcount 4)
(def output-slots 5)

(def output-sub-slot-output 0)
(def output-sub-slot-process 1)
(def output-sub-slot-ready 2)
(def output-sub-slots 3)

(def channel-slot-remote 0)
(def channel-slot-events 1)
(def channel-slot-process 2)
(def channel-slot-busy 3)
(def channel-slot-over 4)
(def channel-slot-sub-ready 5)
(def channel-slot-step 6)
(def channel-slot-done 7)
(def channel-slot-alive 8)
(def channel-slot-freeze 9)
(def channel-slot-acks 10)
(def channel-slot-toggle 11)
(def channel-slot-ready 12)
(def channel-slot-shared 13)
(def channel-slot-reader-opts 14)
(def channel-slot-writer-opts 15)
(def channel-slots 16)

(def port-slot-slot 0)
(def port-slot-site 1)
(def port-slot-deps 2)
(def port-slot-flow 3)
(def port-slots 4)

(def input-slot-remote 0)                                   ;; this is only required for peer access
(def input-slot-port 1)
(def input-slot-diff 2)
(def input-slot-frozen 3)
(def input-slot-subs 4)
(def input-slot-refcount 5)
(def input-slot-requested 6)
(def input-slots 7)

(def input-sub-slot-input 0)
(def input-sub-slot-step 1)
(def input-sub-slot-done 2)
(def input-sub-slot-prev 3)
(def input-sub-slot-next 4)
(def input-sub-slot-diff 5)
(def input-sub-slot-ready 6)
(def input-sub-slots 7)

(def call-slot-port 0)
(def call-slot-rank 1)
(def call-slots 2)

;; Pure | Ap | Join | Slot
(defprotocol Expr
  (deps [_ rf r site])                                      ;; emits ports
  (flow [_]))                                               ;; returns incseq

(defn expr-deps [rf r site expr]
  (deps expr rf r site))

(deftype Failure [info])

(defn failure-info [^Failure f]
  (.-info f))

(defn failure? [x]
  (instance? Failure x))

(defn invariant [x] (m/cp x))

(deftype Pure [value
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
  (flow [_]
    (if (failure? value)
      (m/latest #(throw (ex-info "Illegal access." {:info (failure-info value)})))
      (i/fixed (invariant value)))))

(defn pure "
-> (EXPR VOID)
T -> (EXPR T)
T T -> (EXPR T)
T T T -> (EXPR T)
" [value]
  (->Pure value nil))

(defn invoke
  ([f] (f))
  ([f a] (f a))
  ([f a b] (f a b))
  ([f a b c] (f a b c))
  ([f a b c d] (f a b c d))
  ([f a b c d & es] (apply f a b c d es)))

;; TODO the runtime swallows exceptions somewhere
;; maybe in latest-product, not sure.
;; investigate and remove this afterwards
(defn invoke-print-throws [& args]
  (try (apply invoke args)
       (catch #?(:clj Throwable :cljs :default) e (#?(:clj prn :cljs js/console.error) e))))

(deftype Ap [inputs
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
  (flow [_]
    (apply i/latest-product invoke-print-throws (map flow inputs))))

(defn ap "
(EXPR (-> T)) -> (EXPR T)
(EXPR (A -> T)) (EXPR A) -> (EXPR T)
(EXPR (A B -> T)) (EXPR A) (EXPR B) -> (EXPR T)
(EXPR (A B C -> T)) (EXPR A) (EXPR B) (EXPR C) -> (EXPR T)
" [& inputs]
  (->Ap inputs nil))

(deftype Join [input ^:unsynchronized-mutable ^:mutable hash-memo]
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
  (flow [_] (i/latest-concat (flow input))))

(defn join "
(EXPR (IS T)) -> (EXPR T)
" [input] (->Join input nil))

(def effect "
-> (EXPR VOID)
(IS T) -> (EXPR T)
(IS T) (IS T) -> (EXPR T)
(IS T) (IS T) (IS T) -> (EXPR T)
" (comp join pure))

(def fixed-signals "
-> (IS VOID)
(CF T) -> (IS T)
(CF T) (CF T) -> (IS T)
(CF T) (CF T) (CF T) -> (IS T)
" (comp (partial m/signal i/combine) i/fixed))

(defn drain "
(IS T) -> (IS VOID)
" [incseq]
  (let [signal (m/signal i/combine incseq)]
    (m/ap
      (m/amb (i/empty-diff 0)
        (do (m/?> signal) (m/amb))))))

(defn error [^String msg]
  #?(:clj (Error. msg)
     :cljs (js/Error. msg)))

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
  (flow [_]
    (fn [step done]
      (step) (->Failer done (error (str "Unbound electric var lookup - " (pr-str key)))))))

(deftype Cdef [frees nodes calls result build])

(def cdef ->Cdef)

(declare slot-port)

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

(defn arity-mismatch [nm arity]
  (throw (error (str nm ": wrong number of args (" arity ")"))))

(defn get-variadic [nm F arity]
  (if-some [[fixed map? ctor] (F -1)]
    (if (< arity fixed)
      (arity-mismatch nm arity)
      [fixed map? ctor])
    (arity-mismatch nm arity)))

(defn varargs [map?]
  (if map?
    (fn [& args]
      (loop [args args
             m nil]
        (if-some [[k & args] args]
          (if-some [[v & args] args]
            (recur args (assoc m k v))
            (merge m k)) m)))
    (fn [& args] args)))

(defn dispatch [nm F & args]
  (let [arity (count args)]
    (if-some [ctor (F arity)]
      (apply bind-args (bind-self ctor) args)
      (let [[fixed map? ctor] (get-variadic nm F arity)]
        (bind (apply bind-args (bind-self ctor) (take fixed args))
          fixed (apply ap (pure (varargs map?)) (drop fixed args)))))))

(defn peer-defs [^objects peer]
  (aget peer peer-slot-defs))

(defn peer-site [^objects peer]
  (aget peer peer-slot-site))

(defn peer-root [^objects peer key]
  (let [defs (peer-defs peer)]
    (when-not (contains? defs key) (throw (error (str (pr-str key) " not defined"))))
    (defs key)))

(defn peer-cdef
  "Returns the cdef of given constructor."
  {:tag Cdef}
  [^objects peer key idx]
  ((peer-root peer key) idx))

(defn port-flow [^objects port]
  (aget port port-slot-flow))

(defn port-deps [rf r ^objects port]
  (reduce-kv
    (fn [r port n]
      (reduce rf r (repeat n port)))
    r (aget port port-slot-deps)))

(declare frame-result)

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
    ((port-flow (frame-result this)) step done)))

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
  ((peer-root (.-peer frame) key)))

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
    (let [s (aget peer peer-slot-sub-ready)
          c (aget peer peer-slot-channel-ready)]
      (aset peer peer-slot-sub-ready nil)
      (aset peer peer-slot-channel-ready nil)
      #?(:clj  (.unlock ^ReentrantLock (aget peer peer-slot-busy))
         :cljs (aset peer peer-slot-busy false))
      (loop [^objects sub s]
        (when-not (nil? sub)
          (let [s (aget sub input-sub-slot-ready)]
            (aset sub input-sub-slot-ready nil)
            ((if-some [step (aget sub input-sub-slot-step)]
               step (aget sub input-sub-slot-done)))
            (recur s))))
      (loop [^objects chan c]
        (when-not (nil? chan)
          (let [c (aget chan channel-slot-ready)]
            (aset chan channel-slot-ready nil)
            ((if-some [step (aget chan channel-slot-step)]
               step (aget chan channel-slot-done)))
            (recur c)))))))

(defn channel-output-event [^objects channel]
  (when (identical? channel (aget channel channel-slot-ready))
    (let [^objects remote (aget channel channel-slot-remote)
          ^objects peer (aget remote remote-slot-peer)]
      (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
      (aset peer peer-slot-channel-ready channel))))

(defn channel-terminated [^objects channel]
  (when (zero? (aset channel channel-slot-alive
                 (dec (aget channel channel-slot-alive))))
    (when (identical? channel (aget channel channel-slot-ready))
      (aset channel channel-slot-step nil)
      (let [^objects remote (aget channel channel-slot-remote)
            ^objects peer (aget remote remote-slot-peer)]
        (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
        (aset peer peer-slot-channel-ready channel)))))

(defn port-site [^objects port]
  (aget port port-slot-site))

(defn input-sub-cancel [^objects sub]
  (let [^objects input (aget sub input-sub-slot-input)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (when-some [^objects prv (aget sub input-sub-slot-prev)]
      (aset input input-slot-subs
        (when-not (identical? prv sub)
          (let [^objects nxt (aget sub input-sub-slot-next)]
            (aset prv input-sub-slot-next nxt)
            (aset nxt input-sub-slot-prev prv))))
      (aset sub input-sub-slot-prev nil)
      (aset sub input-sub-slot-next nil)
      (if (nil? (aget sub input-sub-slot-diff))
        (do (aset sub input-sub-slot-ready (aget peer peer-slot-sub-ready))
            (aset peer peer-slot-sub-ready sub))
        (aset sub input-sub-slot-diff nil)))
    (exit peer busy)))

(defn input-sub-transfer [^objects sub]
  (let [^objects input (aget sub input-sub-slot-input)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (if-some [diff (aget sub input-sub-slot-diff)]
      (do (aset sub input-sub-slot-diff nil)
          (if (nil? (aget sub input-sub-slot-prev))
            (do (aset sub input-sub-slot-step nil)
                (aset sub input-sub-slot-ready (aget peer peer-slot-sub-ready))
                (aset peer peer-slot-sub-ready sub))
            (aset sub input-sub-slot-ready sub))
          (exit peer busy) diff)
      (do (aset sub input-sub-slot-step nil)
          (aset sub input-sub-slot-ready (aget peer peer-slot-sub-ready))
          (aset peer peer-slot-sub-ready sub)
          (exit peer busy)
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
  (flow [this]
    (port-flow (slot-port this))))

(defn port-slot
  {:tag Slot}
  [^objects port]
  (aget port port-slot-slot))

(defn slot-port
  {:tag 'objects}
  [^Slot slot]
  (let [id (.-id slot)
        ^Frame frame (.-frame slot)]
    (if (neg? id)
      (aget ^objects (.-nodes frame) (- -1 id))
      (aget ^objects (aget ^objects (.-tags frame) id) call-slot-port))))

(defn frame-path [^Frame frame]
  (loop [^Frame frame frame
         path ()]
    (if-some [^Slot slot (.-slot frame)]
      (recur (.-frame slot)
        (conj path [(.-id slot) (.-rank ^Frame frame)]))
      (vec path))))

(defn input-check-create [^objects remote port]
  (let [slot (port-slot port)]
    (if-some [^objects input (get (aget remote remote-slot-inputs) slot)]
      input (let [input (object-array input-slots)]
              (aset input input-slot-remote remote)
              (aset input input-slot-port port)
              (aset input input-slot-requested (identity 0))
              (aset input input-slot-refcount (identity 0))
              (aset input input-slot-diff (i/empty-diff 0))
              (aset input input-slot-frozen false)
              (aset remote remote-slot-inputs
                (assoc (aget remote remote-slot-inputs) slot input))
              input))))

(defn input-sub [^objects port]
  (fn [step done]
    (let [^Slot slot (port-slot port)
          ^objects peer (frame-peer (.-frame slot))
          busy (enter peer)
          ^objects remote (aget peer peer-slot-remote)
          ^objects input (input-check-create remote port)
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
            (aset sub input-sub-slot-prev nxt))
          (do (aset input input-slot-subs sub)
              (aset sub input-sub-slot-prev sub)
              (aset sub input-sub-slot-next sub))))
      (aset sub input-sub-slot-ready (aget peer peer-slot-sub-ready))
      (aset peer peer-slot-sub-ready sub)
      (exit peer busy)
      (->InputSub sub))))

(defn make-port [^Slot slot site deps flow]
  (let [port (object-array port-slots)
        peer (frame-peer (.-frame slot))]
    (aset port port-slot-slot slot)
    (aset port port-slot-site site)
    (aset port port-slot-deps deps)
    (aset port port-slot-flow
      (if (= site (peer-site peer))
        (m/signal i/combine flow)
        (input-sub port)))
    port))

(defn update-inc [m k]
  (assoc m k (inc (m k 0))))

(defn define-slot [^Slot slot expr]
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
               (make-port slot site
                 (deps expr update-inc {} site)
                 (flow expr)))]
    (aset ^objects (.-nodes frame) (- -1 id) port) nil))

(defn make-frame [^objects peer ^Slot slot rank site ctor]
  (let [[key idx _ _] ctor
        cdef (peer-cdef peer key idx)
        nodec (count (.-nodes cdef))
        callc (count (.-calls cdef))
        frame (->Frame peer slot rank site ctor
                (object-array (inc nodec)) (object-array callc) nil)]
    (define-slot (->Slot frame (- -1 nodec)) ((.-build cdef) frame)) frame))

(defn decode [^String s opts]
  #?(:clj (t/read (t/reader (ByteArrayInputStream. (.getBytes s)) :json opts))
     :cljs (t/read (t/reader :json opts) s)))

(defn encode [value opts]
  #?(:clj
     (let [out (ByteArrayOutputStream.)
           writer (t/writer out :json opts)]
       (t/write writer value)
       (.toString out))
     :cljs
     (t/write (t/writer :json opts) value)))

(defn remote-ack [^objects remote]
  ;; TODO
  )

(defn remote-port-tap [^objects remote ^objects port]
  (let [^objects input (input-check-create remote port)]
    (aset input input-slot-refcount (inc (aget input input-slot-refcount))))
  remote)

(defn remote-port-untap [^objects remote ^objects port]
  (let [slot (port-slot port)
        ^objects inputs (aget remote remote-slot-inputs)
        ^objects input (get inputs slot)
        refcount (dec (aget input input-slot-refcount))]
    (aset input input-slot-refcount refcount)
    (when (zero? refcount)
      (aset remote remote-slot-inputs (dissoc inputs slot))))
  remote)

(defn channel-output-sub [^objects channel ^objects output]
  (let [sub (object-array output-sub-slots)]
    (aset channel channel-slot-alive
      (inc (aget channel channel-slot-alive)))
    (aset output output-slot-sub sub)
    (aset sub output-sub-slot-ready sub)
    (aset sub output-sub-slot-output output)
    (aset sub output-sub-slot-process
      ((port-flow (aget output output-slot-port))
       #(let [^objects remote (aget output output-slot-remote)
              ^objects peer (aget remote remote-slot-peer)
              busy (enter peer)]
          (if (identical? sub (aget output output-slot-sub))
            (do (aset sub output-sub-slot-ready (aget channel channel-slot-sub-ready))
                (aset channel channel-slot-sub-ready sub)
                (channel-output-event channel))
            (try @(aget sub output-sub-slot-process)
                 (catch #?(:clj Throwable :cljs :default) _)))
          (exit peer busy))
       #(let [^objects remote (aget output output-slot-remote)
              ^objects peer (aget remote remote-slot-peer)
              busy (enter peer)]
          (if (identical? sub (aget output output-slot-sub))
            (do (aset channel channel-slot-alive
                  (dec (aget channel channel-slot-alive)))
                (aset channel channel-slot-freeze
                  (conj (aget channel channel-slot-freeze)
                    (port-slot (aget output output-slot-port))))
                (channel-output-event channel))
            (channel-terminated channel))
          (exit peer busy))))
    channel))

(defn output-check-create [^objects remote ^objects local-port]
  (let [slot (port-slot local-port)
        outputs (aget remote remote-slot-outputs)]
    (if-some [output (get outputs slot)]
      output (let [output (object-array output-slots)]
               (aset output output-slot-remote remote)
               (aset output output-slot-port local-port)
               (aset output output-slot-refcount (identity 0))
               (aset output output-slot-requested false)
               (aset remote remote-slot-outputs
                 (assoc (aget remote remote-slot-outputs) slot output))
               (when-some [channel (aget remote remote-slot-channel)]
                 (channel-output-sub channel output))
               output))))

(defn output-reset [^objects output]
  (when-some [^objects sub (aget output output-slot-sub)]
    (let [^objects remote (aget output output-slot-remote)]
      (aset output output-slot-sub nil)
      (aset remote remote-slot-outputs
        (dissoc (aget remote remote-slot-outputs)
          (port-slot (aget output output-slot-port))))
      ((aget sub output-sub-slot-process)))))

(defn reset-diff [n]
  {:grow        0,
   :degree      n,
   :shrink      n,
   :permutation {},
   :change      {},
   :freeze      #{}})

(defn input-reset [^objects input]
  (let [^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        rd (reset-diff (:degree (aget input input-slot-diff)))]
    (aset input input-slot-diff (i/empty-diff 0))
    (when-some [^objects sub (aget input input-slot-subs)]
      (loop [^objects s sub]
        (if-some [{:keys [grow degree]} (aget s input-sub-slot-diff)]
          (aset s input-sub-slot-diff (reset-diff (- degree grow)))
          (do (aset s input-sub-slot-diff rd)
              (when (identical? s (aget s input-sub-slot-ready))
                (aset s input-sub-slot-ready (aget peer peer-slot-sub-ready))
                (aset peer peer-slot-sub-ready s))))
        (let [n (aget sub input-sub-slot-next)]
          (when-not (identical? n sub) (recur n)))))))

(defn remote-change [^objects remote ^Slot slot diff]
  (let [^objects input (get (aget remote remote-slot-inputs) slot)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (aset input input-slot-diff (i/combine (aget input input-slot-diff) diff))
    (when-some [^objects sub (aget input input-slot-subs)]
      (loop [^objects s sub]
        (if-some [prev (aget s input-sub-slot-diff)]
          (aset s input-sub-slot-diff (i/combine prev diff))
          (do (aset s input-sub-slot-diff diff)
              (when (identical? s (aget s input-sub-slot-ready))
                (aset s input-sub-slot-ready (aget peer peer-slot-sub-ready))
                (aset peer peer-slot-sub-ready s))))
        (let [n (aget sub input-sub-slot-next)]
          (when-not (identical? n sub) (recur n))))))
  remote)

(defn remote-freeze [^objects remote ^Slot slot]
  (let [^objects input (get (aget remote remote-slot-inputs) slot)
        ^objects remote (aget input input-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (aset input input-slot-frozen true)
    (when-some [^objects sub (aget input input-slot-subs)]
      (aset input input-slot-subs nil)
      (loop [^objects s sub]
        (when (nil? (aget s input-sub-slot-diff))
          (aset s input-sub-slot-step nil)
          (aset s input-sub-slot-ready (aget peer peer-slot-sub-ready))
          (aset peer peer-slot-sub-ready s))
        (let [n (aget s input-sub-slot-next)]
          (aset s input-sub-slot-next nil)
          (aset s input-sub-slot-prev nil)
          (when-not (identical? n sub) (recur n))))))
  remote)

(defn channel-crash [^objects channel]
  (let [^objects remote (aget channel channel-slot-remote)]
    (aset remote remote-slot-channel nil)
    (run! input-reset (vals (aget remote remote-slot-inputs)))
    (run! output-reset (vals (aget remote remote-slot-outputs)))
    (loop []
      (when-some [^objects sub (aget channel channel-slot-sub-ready)]
        (aset channel channel-slot-sub-ready (aget sub output-sub-slot-ready))
        (aset sub output-sub-slot-ready sub)
        (try @(aget sub output-sub-slot-process)
             (catch #?(:clj Throwable :cljs :default) _))
        (recur)))))

(defn channel-cancel [^objects channel]
  ((aget channel channel-slot-process)))

(defn channel-transfer [^objects channel]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)
        busy (enter peer)]
    (try (if (identical? channel (aget remote remote-slot-channel))
           (loop [change {}]
             (if-some [^objects sub (aget channel channel-slot-sub-ready)]
               (let [^objects output (aget sub output-sub-slot-output)
                     ^objects port (aget output output-slot-port)
                     ps (aget sub output-sub-slot-process)
                     slot (port-slot port)]
                 (aset channel channel-slot-sub-ready (aget sub output-sub-slot-ready))
                 (aset sub output-sub-slot-ready sub)
                 (recur (if (identical? sub (aget output output-slot-sub))
                          (assoc change slot (let [diff @ps]
                                               (if-some [p (change slot)]
                                                 (i/combine p diff) diff)))
                          (do (try @ps (catch #?(:clj Throwable :cljs :default) _)) change))))
               (let [acks (aget channel channel-slot-acks)
                     toggle (aget channel channel-slot-toggle)
                     freeze (aget channel channel-slot-freeze)]
                 (aset channel channel-slot-acks (identity 0))
                 (aset channel channel-slot-toggle #{})
                 (aset channel channel-slot-freeze #{})
                 (encode [acks toggle change freeze]
                   (aget channel channel-slot-writer-opts)))))
           (let [e (aget channel channel-slot-sub-ready)]
             (aset channel channel-slot-sub-ready nil)
             (throw e)))
         (catch #?(:clj Throwable :cljs :default) e
           (channel-crash channel)
           (throw e))
         (finally
           (if (zero? (aget channel channel-slot-alive))
             (do (aset channel channel-slot-step nil)
                 (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
                 (aset peer peer-slot-channel-ready channel))
             (aset channel channel-slot-ready channel))
           (exit peer busy)))))

(defn remote-toggle [^objects remote ^Slot slot]
  (let [^objects local-port (slot-port slot)
        ^objects output (output-check-create remote local-port)
        requested (aget output output-slot-requested)]
    (aset output output-slot-requested (not requested))
    (port-deps (if requested remote-port-untap remote-port-tap) remote local-port)
    (when requested
      (when (zero? (aget output output-slot-refcount))
        (output-reset output))))
  remote)

(defn channel-ready [^objects channel busy]
  (let [^objects remote (aget channel channel-slot-remote)
        ^objects peer (aget remote remote-slot-peer)]
    (while (aset channel channel-slot-busy
             (not (aget channel channel-slot-busy)))
      (if (aget channel channel-slot-over)
        (channel-terminated channel)
        (if (identical? channel (aget remote remote-slot-channel))
          (try
            (let [[acks toggle change freeze]
                  (decode @(aget channel channel-slot-process)
                    (aget channel channel-slot-reader-opts))]
              (dotimes [_ acks] (remote-ack remote))
              (reduce remote-toggle remote toggle)
              (reduce-kv remote-change remote change)
              (reduce remote-freeze remote freeze)
              (when (pos? (+ (count toggle) (count change) (count freeze)))
                (aset channel channel-slot-acks
                  (inc (aget channel channel-slot-acks)))
                (channel-output-event channel)))
            (catch #?(:clj Throwable :cljs :default) e
              (channel-crash channel)
              (aset channel channel-slot-sub-ready e)
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

(defn channel-writer-opts [^objects channel]
  (let [handlers {Slot    (t/write-handler
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
                  ;; must wrap payload in vector, cf https://github.com/cognitect/transit-cljs/issues/23
                  Pure    (t/write-handler
                            (fn [_] "pure")
                            (fn [^Pure pure]
                              [(.-value pure)]))
                  Join    (t/write-handler
                            (fn [_] "join")
                            (fn [^Join join]
                              [(.-input join)]))
                  Unbound (t/write-handler
                            (fn [_] "unbound")
                            (fn [^Unbound unbound]
                              [(.-key unbound)]))}
        default (t/write-handler
                  (fn [v] (prn :unserializable v) "unserializable")
                  (fn [_]))]
    #?(:clj  {:handlers handlers :default-handler default}
       :cljs {:handlers (assoc handlers :default default)})))

(defn channel-reader-opts [^objects channel]
  {:handlers {"slot"           (t/read-handler
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
                                   (->Join input nil)))
              "ap"             (t/read-handler
                                 (fn [inputs]
                                   (->Ap inputs nil)))
              "pure"           (t/read-handler
                                 (fn [[value]]
                                   (->Pure value nil)))
              "unbound"        (t/read-handler
                                 (fn [[key]]
                                   (->Unbound key nil)))
              "unserializable" (t/read-handler
                                 (fn [_]
                                   (->Failure :unserializable)))}})

(defn remote-handler [^objects remote]
  (fn [events]
    (fn [step done]
      (let [^objects peer (aget remote remote-slot-peer)
            busy (enter peer)]
        (if (nil? (aget remote remote-slot-channel))
          (let [channel (object-array channel-slots)]
            (aset remote remote-slot-channel channel)
            (aset channel channel-slot-remote remote)
            (aset channel channel-slot-step step)
            (aset channel channel-slot-done done)
            (aset channel channel-slot-acks (identity 0))
            (aset channel channel-slot-toggle
              (into #{}
                (comp
                  (filter #(aget ^objects % input-slot-requested))
                  (map #(port-slot (aget ^objects % input-slot-port))))
                (vals (aget remote remote-slot-inputs))))
            (aset channel channel-slot-freeze #{})
            (aset channel channel-slot-busy true)
            (aset channel channel-slot-over false)
            (aset channel channel-slot-events events)
            (aset channel channel-slot-alive (identity 1))
            (aset channel channel-slot-shared {})
            (aset channel channel-slot-writer-opts (channel-writer-opts channel))
            (aset channel channel-slot-reader-opts (channel-reader-opts channel))
            (aset channel channel-slot-ready (aget peer peer-slot-channel-ready))
            (aset peer peer-slot-channel-ready channel)
            (aset channel channel-slot-process
              ((aget remote remote-slot-events)
               #(let [^objects remote (aget channel channel-slot-remote)]
                  (channel-ready channel (enter (aget remote remote-slot-peer))))
               #(let [^objects remote (aget channel channel-slot-remote)]
                  (aset channel channel-slot-over true)
                  (channel-ready channel (enter (aget remote remote-slot-peer))))))
            (reduce channel-output-sub channel (vals (aget remote remote-slot-outputs)))
            (channel-ready channel busy)
            (->Channel channel))
          (do (exit peer busy) (step)
              (->Failer done (error "Can't connect - remote already up."))))))))

(defn local-port-tap [^objects remote ^objects local-port]
  (let [^objects output (output-check-create remote local-port)]
    (aset output output-slot-refcount (inc (aget output output-slot-refcount)))
    remote))

(defn local-port-untap [^objects remote ^objects local-port]
  (let [^objects output (get (aget remote remote-slot-outputs) (port-slot local-port))
        refcount (dec (aget output output-slot-refcount))]
    (aset output output-slot-refcount refcount)
    (when (zero? refcount)
      (when-not (aget output output-slot-requested)
        (output-reset output)))
    remote))

(defn port-coordinates [^objects port]
  (let [slot (port-slot port)]
    [(frame-path (.-frame slot)) (.-id slot)]))

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
  [^Frame frame id expr]
  (define-slot (node frame id) expr))

(defn slot-frame
  "Returns the frame of given slot."
  {:tag Frame}
  [^Slot slot]
  (.-frame slot))

(defn slot-id
  "Returns the id of given slot."
  [^Slot slot]
  (.-id slot))

(defn frame-slot [^Frame frame]
  (.-slot frame))

(defn remote-toggle-event [^objects remote slot]
  (when-some [^objects channel (aget remote remote-slot-channel)]
    (let [toggle (aget channel channel-slot-toggle)]
      (aset channel channel-slot-toggle
        ((if (contains? toggle slot) disj conj)
         toggle slot)))
    (channel-output-event channel)))

(defn port-attach [_ ^objects remote-port]
  (let [slot (port-slot remote-port)
        ^objects peer (frame-peer (slot-frame slot))
        ^objects remote (aget peer peer-slot-remote)
        ^objects input (input-check-create remote remote-port)
        requested (aget input input-slot-requested)]
    (aset input input-slot-requested (inc requested))
    (when (zero? requested)
      (when (zero? (aget input input-slot-refcount))
        (remote-toggle-event remote slot)))
    (port-deps local-port-tap remote remote-port)))

(defn port-detach [_ ^objects remote-port]
  (let [slot (port-slot remote-port)
        ^objects peer (frame-peer (slot-frame slot))
        ^objects remote (aget peer peer-slot-remote)
        ^objects input (get (aget remote remote-slot-inputs) slot)
        requested (dec (aget input input-slot-requested))]
    (aset input input-slot-requested requested)
    (when (zero? requested)
      (when (zero? (aget input input-slot-refcount))
        (aset remote remote-slot-inputs
          (dissoc (aget remote remote-slot-inputs) slot))
        (remote-toggle-event remote slot)))
    (port-deps local-port-untap remote remote-port)))

(deftype Incseq [site expr]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (deps expr port-attach nil site)
    ((flow expr) step #(do (deps expr port-detach nil site) (done)))))

(defn incseq-expr [^Incseq incseq]
  (.-expr incseq))

(defn incseq [^Frame frame expr]
  (->Incseq (peer-site (frame-peer frame)) expr))

(defn frame-result [^Frame frame]
  (let [^objects nodes (.-nodes frame)]
    (aget nodes (dec (alength nodes)))))

(defn frame-up [^Frame frame]
  (deps (port-slot (frame-result frame)) port-attach nil
    (port-site (slot-port (.-slot frame)))))

(defn frame-down [^Frame frame]
  (deps (port-slot (frame-result frame)) port-detach nil
    (port-site (slot-port (.-slot frame)))))

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

(deftype CallPs [^objects call ps ^:unsynchronized-mutable ^:mutable ^objects buffer]
  IFn
  (#?(:clj invoke :cljs -invoke) [_] (ps))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (let [{:keys [grow degree shrink permutation change freeze]} @ps
          ^objects port (aget call call-slot-port)
          ^Slot slot (port-slot port)
          ^Frame parent (.-frame slot)
          ^objects peer (.-peer parent)
          site (port-site (slot-port slot))
          size-after (- degree shrink)
          ^objects buffer (let [cap (alength buffer)]
                            (if (< degree cap)
                              buffer (let [b (object-array (loop [cap cap]
                                                             (let [cap (bit-shift-left cap 1)]
                                                               (if (< degree cap)
                                                                 cap (recur cap)))))]
                                       #?(:clj  (System/arraycopy buffer 0 b 0 cap)
                                          :cljs (dotimes [i cap] (aset b i (aget buffer i))))
                                       (set! buffer b))))]
      (reduce apply-cycle buffer (i/decompose permutation))
      (dotimes [i shrink]
        (let [j (+ size-after i)]
          (frame-down (aget buffer j))
          (aset buffer j nil)))
      {:grow        grow
       :degree      degree
       :shrink      shrink
       :permutation permutation
       :freeze      freeze
       :change      (reduce-kv (fn [change i ctor]
                                 (when-some [frame (aget buffer i)] (frame-down frame))
                                 (let [rank (aget call call-slot-rank)
                                       frame (make-frame peer slot rank site ctor)]
                                   (aset buffer i frame)
                                   (aset call call-slot-rank (inc rank))
                                   (frame-up frame)
                                   (assoc change i frame)))
                      {} change)})))

(defn create-call [slot site expr]
  (let [call (object-array call-slots)]
    (aset call call-slot-port
      (make-port slot site
        (deps expr update-inc {} site)
        (fn [step done]
          (->CallPs call
            ((flow expr) step done)
            (object-array 1)))))
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

(defn make-remote [^objects peer]
  (let [^objects remote (object-array remote-slots)]
    (aset remote remote-slot-peer peer)
    (aset remote remote-slot-inputs {})
    (aset remote remote-slot-outputs {})
    (aset remote remote-slot-events
      (m/stream
        (m/observe
          (fn [!]
            (let [^objects channel (aget remote remote-slot-channel)
                  events (aget channel channel-slot-events)]
              (aset channel channel-slot-events nil)
              (events !))))))
    remote))

(defn make-peer "
Returns a new peer instance for given site, from given definitions and main key and optional extra arguments to the
entrypoint.
" [site defs main args]
  (let [^objects peer (object-array peer-slots)
        ^objects remote (make-remote peer)]
    (aset peer peer-slot-busy #?(:clj (ReentrantLock.) :cljs false))
    (aset peer peer-slot-site site)
    (aset peer peer-slot-defs defs)
    (aset peer peer-slot-remote remote)
    (aset peer peer-slot-root
      (->> args
        (eduction (map pure))
        (apply dispatch "<root>" ((defs main)))
        (make-frame peer nil 0 :client))) peer))

(defn peer-result [^objects peer]
  (let [^Frame root (aget peer peer-slot-root)
        ^objects nodes (.-nodes root)
        ^objects result (aget nodes (dec (alength nodes)))]
    (fn [step done]
      (port-deps port-attach nil result)
      ((port-flow result) step
       #(do (port-deps port-detach nil result) (done))))))

(defn peer-remote [^objects peer]
  (aget peer peer-slot-remote))

(defn peer-consume-result [^objects peer remote-connector]
  (m/reduce (comp reduced {}) nil
    (m/ap
      (m/amb= (m/? (remote-connector (remote-handler (peer-remote peer))))
        (m/amb (do (m/?> (peer-result peer)) (m/amb)) nil)))))

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

(defn tracing [info v] (print "[o_o]" info "=>> ") (prn v) v)

(defn ->defs [mp]
  (loop [ret {}, left mp]
    (if-some [[k f] (first left)]
      (if (ret k)
        (recur ret (dissoc left k))
        (recur (assoc ret k f) (merge (dissoc left k) (f :get :deps))))
      ret)))

(defn client "
Allocates a new client peer and returns a task consuming its return value using given connector as its server
communication channel. `connector` must be a function taking the remote handler as an argument and returning
a task managing the lifecycle of the channel.

The remote handler is a function taking a subject and returning a flow. The flow emits outgoing events and reads
incoming events on the subject.
" [connector defs main & args]
  (peer-consume-result (make-peer :client defs main args) connector))

(defn server "
Allocates a new server peer and returns its remote handler.
" [defs main & args]
  (remote-handler (peer-remote (make-peer :server defs main args))))