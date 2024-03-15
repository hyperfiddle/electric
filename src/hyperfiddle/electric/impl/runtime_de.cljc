(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [cognitect.transit :as t])
  (:import missionary.Cancelled
           #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream))))

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

(def peer-slot-root 0)
(def peer-slot-reader-opts 1)
(def peer-slot-writer-opts 2)
(def peer-slot-input-process 3)
(def peer-slot-input-busy 4)
(def peer-slot-output-pending 5)
(def peer-slot-output-acks 6)
(def peer-slot-result 7)
(def peer-slots 8)

(def peer-queue-tap 0)
(def peer-queue-untap 1)
(def peer-queue-toggle 2)
(def peer-queue-ready 3)
(def peer-queues 4)

(def port-slot-slot 0)
(def port-slot-site 1)
(def port-slot-deps 2)
(def port-slot-flow 3)
(def port-slot-requested 4)
(def port-slot-refcount 5)
(def port-slot-process 6)
(def port-slots 7)

(declare peer-cancel peer-transfer)

(deftype Peer [site defs step done queues pushes state]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (peer-cancel this))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (peer-transfer this)))

;; Pure | Ap | Join | Slot
(defprotocol Expr
  (deps [_ site])                                           ;; returns {Port multiplicity}
  (flow [_]))                                               ;; returns incseq

(deftype Failure [info])

(defn failure-info [^Failure f]
  (.-info f))

(defn failure? [x]
  (instance? Failure x))

(defn invariant [x] (m/cp x))

(defn incseq "
(EXPR T) -> (IS T)
" [expr] (flow expr))

(deftype Pure [values
               ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash Pure)
            (hash-ordered-coll values)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Pure other)
      (= values (.-values ^Pure other))))
  Expr
  (deps [_ _] {})
  (flow [_]
    (if-some [error (reduce (comp reduced {})
                      nil (eduction (filter failure?) values))]
      (m/latest #(throw (ex-info "Illegal access." {:info (failure-info error)})))
      (apply i/fixed (map invariant values))))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(defn pure "
-> (EXPR VOID)
T -> (EXPR T)
T T -> (EXPR T)
T T T -> (EXPR T)
" [& values]
  (->Pure values nil))

(defn invoke
  ([f] (f))
  ([f a] (f a))
  ([f a b] (f a b))
  ([f a b c] (f a b c))
  ([f a b c d] (f a b c d))
  ([f a b c d & es] (apply f a b c d es)))

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
  (deps [_ site]
    (reduce (fn [r x] (merge-with + r (deps x site))) {} inputs))
  (flow [_]
    (apply i/latest-product invoke (map flow inputs)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

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
  (deps [_ site] (deps input site))
  (flow [_] (i/latest-concat (flow input)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

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

(deftype Unbound [k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (step) (->Failer done (error (str "Unbound electric var lookup - " (pr-str k))))))

(deftype Cdef [frees nodes calls result build])

(def cdef ->Cdef)

(declare slot-port)

(deftype Ctor [peer key idx ^objects free env
               ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (loop [h (-> (hash peer)
                   (hash-combine (hash key))
                   (hash-combine (hash idx))
                   (hash-combine (hash env)))
               i 0]
          (if (== i (alength free))
            (set! hash-memo h)
            (recur (hash-combine h (hash (slot-port (aget free i))))
              (inc i))))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Ctor other)
      (= peer (.-peer ^Ctor other))
      (= key (.-key ^Ctor other))
      (= idx (.-idx ^Ctor other))
      (= env (.-env ^Ctor other))
      (let [n (alength free)
            ^objects f (.-free ^Ctor other)]
        (if (== n (alength f))
          (loop [i 0]
            (if (== i n)
              true (if (= (slot-port (aget free i)) (slot-port (aget f i)))
                     (recur (inc i)) false))) false)))))

(defn bind "
(CTOR T) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) (VAR B) (EXPR B) -> (CTOR T)
(CTOR T) (VAR A) (EXPR A) (VAR B) (EXPR B) (VAR C) (EXPR C) -> (CTOR T)
" ([^Ctor ctor] ctor)
  ([^Ctor ctor k v]
   (->Ctor (.-peer ctor) (.-key ctor) (.-idx ctor) (.-free ctor)
     (assoc (.-env ctor) k v) nil))
  ([^Ctor ctor k v & kvs]
   (->Ctor (.-peer ctor) (.-key ctor) (.-idx ctor) (.-free ctor)
     (apply assoc (.-env ctor) k v kvs) nil)))

(defn bind-args [^Ctor ctor & args]
  (reduce (partial apply bind) ctor (eduction (map-indexed vector) args)))

(defn ctor-peer
  "Returns the peer of given constructor."
  {:tag Peer}
  [^Ctor ctor]
  (.-peer ctor))

(defn ctor-cdef
  "Returns the cdef of given constructor."
  {:tag Cdef}
  [^Ctor ctor]
  (((.-defs (ctor-peer ctor)) (.-key ctor)) (.-idx ctor)))

(declare result)

(defn port-flow [^objects port]
  (aget port port-slot-flow))

(defn port-deps [^objects port]
  (aget port port-slot-deps))

(deftype Frame [parent call-id rank site ctor
                ^ints ranks ^objects children ^objects ports
                ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (-> (hash parent)
            (hash-combine (hash call-id))
            (hash-combine (hash rank))))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Frame other)
      (= parent (.-parent ^Frame other))
      (= call-id (.-call-id ^Frame other))
      (= rank (.-rank ^Frame other))))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((port-flow (result this)) step done)))

(defn frame-ctor
  "Returns the constructor of given frame."
  {:tag Ctor}
  [^Frame frame]
  (.-ctor frame))

(defn frame-peer
  "Returns the peer of given frame."
  {:tag Peer}
  [^Frame frame]
  (ctor-peer (frame-ctor frame)))

(defn frame-cdef
  "Returns the cdef of given frame."
  {:tag Cdef}
  [^Frame frame]
  (ctor-cdef (frame-ctor frame)))

(defn frame-parent
  "Returns the parent frame of given frame if not root, nil otherwise."
  {:tag Frame}
  [^Frame frame]
  (.-parent frame))

(defn frame-call-id
  "Returns the call id of given frame."
  [^Frame frame]
  (.-call-id frame))

(defn frame-call-count
  "Returns the call count of given frame."
  [^Frame frame]
  (count (.-calls (frame-cdef frame))))

(defn frame-site
  "Returns the site of given frame."
  [^Frame frame]
  (.-site frame))

(defn peer-push [^Peer peer offset item]
  (let [^objects state (.-state peer)
        ^objects queues (.-queues peer)
        ^ints pushes (.-pushes peer)
        ^objects queue (aget queues offset)
        push (aget pushes offset)
        cap (alength queue)
        step (.-step peer)]
    (aset pushes offset
      (if (nil? (aget queue push))
        (do (aset queue push item)
            (rem (unchecked-inc-int push) cap))
        (let [c (bit-shift-left cap 1)
              q (object-array c)]
          (aset queues offset q)
          (i/acopy queue push q push
            (unchecked-subtract-int cap push))
          (i/acopy queue 0 q cap push)
          (let [p (unchecked-add-int push cap)]
            (aset q p item)
            (rem (unchecked-inc-int p) c)))))
    (when (aget state peer-slot-output-pending)
      (aset state peer-slot-output-pending false)
      (step))))

(defn frame-child
  {:tag Frame}
  [^Frame frame [call-id rank]]
  (let [^objects children (.-children frame)]
    (get (aget children call-id) rank)))

(defn peer-frame
  {:tag Frame}
  [^Peer peer path]
  (let [^objects state (.-state peer)]
    (reduce frame-child (aget state peer-slot-root) path)))

(defn port-process [^objects port]
  (aget port port-slot-process))

(defn port-site [^objects port]
  (aget port port-slot-site))

(declare port-slot port-attach port-detach)

(deftype Remote [port step done ^:unsynchronized-mutable ^:mutable diff]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (port-detach port)
    (if (nil? diff)
      (step) (set! diff nil)))
  (#?(:clj invoke :cljs -invoke) [_ value]
    (if (nil? value)
      (do (port-detach port)
          (when (nil? diff) (done)))
      (if-some [prev diff]
        (set! diff (i/combine prev value))
        (do (set! diff value) (step)))))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (if-some [value diff]
      (do (set! diff nil)
          (when-not (identical? this (port-process port))
            (done)) value)
      (do (done) (throw (Cancelled. "Remote port cancelled."))))))

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
  (deps [this site]
    (let [port (slot-port this)]
      (if (= site (port-site port))
        (port-deps port)
        {port 1})))
  (flow [this]
    (port-flow (slot-port this)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(defn port-slot
  {:tag Slot}
  [^objects port]
  (aget port port-slot-slot))

(defn port-attach [^objects port ps]
  (aset port port-slot-process ps)
  (peer-push (frame-peer (.-frame (port-slot port))) peer-queue-tap port))

(defn port-detach [^objects port]
  (aset port port-slot-process nil)
  (peer-push (frame-peer (.-frame (port-slot port))) peer-queue-untap port))

(defn frame-port
  {:tag 'objects}
  [^Frame frame id]
  (let [^objects ports (.-ports frame)]
    (aget ports id)))

(defn slot-port
  {:tag 'objects}
  [^Slot slot]
  (frame-port (.-frame slot) (.-id slot)))

(defn port-ready [^objects port]
  (peer-push (frame-peer (.-frame (port-slot port))) peer-queue-ready port))

(defn define-slot [^Frame frame id expr]
  (let [^objects ports (.-ports frame)]
    (when-not (nil? (aget ports id))
      (throw (error "Can't redefine slot.")))
    (aset ports id
      (if (instance? Slot expr)
        (slot-port expr)
        (let [cdef (ctor-cdef (frame-ctor frame))
              nodes (.-nodes cdef)
              nodec (count nodes)
              site (if-some [site (if (< id nodec)
                                    (nodes id)
                                    (let [id (+ id nodec)
                                          calls (.-calls cdef)
                                          callc (count calls)]
                                      (if (< id callc)
                                        (calls id)
                                        (.-result cdef))))]
                     site (frame-site frame))
              port (object-array port-slots)]
          (aset port port-slot-slot (->Slot frame id))
          (aset port port-slot-site site)
          (aset port port-slot-deps (deps expr site))
          (aset port port-slot-flow
            (m/signal i/combine
              (if (= site (.-site (frame-peer frame)))
                (flow expr)
                (fn [step done]
                  (let [ps (->Remote port step done (i/empty-diff 0))]
                    (port-attach port ps) (step) ps)))))
          (aset port port-slot-refcount (identity 0))
          (aset port port-slot-requested (identity 0))
          port))) nil))

(defn make-frame [^Frame parent call-id rank ctor]
  (let [cdef (ctor-cdef ctor)
        callc (count (.-calls cdef))
        id (+ (count (.-nodes cdef)) callc)
        frame (->Frame parent call-id rank (if (nil? parent) :client (frame-site parent)) ctor
                (int-array (inc callc)) (object-array callc) (object-array (inc id)) nil)]
    (define-slot frame id ((.-build cdef) frame)) frame))

(defn peer-cancel [^Peer peer]
  #_(prn :TODO-cancel))

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

(defn frame-path [^Frame frame]
  (loop [^Frame frame frame
         path ()]
    (if-some [parent (.-parent frame)]
      (recur parent
        (conj path
          [(.-call-id ^Frame frame)
           (.-rank ^Frame frame)]))
      path)))

(defn enable [^objects port]
  (aset port port-slot-process
    ((port-flow port)
     #(port-ready port)
     #(do (aset port port-slot-process nil)
          (port-ready port)))))

(defn disable [^objects port]
  (when-some [ps (port-process port)]
    (aset port port-slot-process nil)
    (ps)))

(defn local-port-tap [_ ^objects port n]
  (let [prev (aget port port-slot-refcount)]
    (aset port port-slot-refcount (+ prev n))
    (when (zero? prev)
      (when (zero? (aget port port-slot-requested))
        (enable port)))))

(defn local-port-untap [_ ^objects port n]
  (let [curr (- (aget port port-slot-refcount) n)]
    (aset port port-slot-refcount curr)
    (when (zero? curr)
      (when (zero? (aget port port-slot-requested))
        (disable port)))))

(defn remote-port-tap [_ ^objects port n]
  (aset port port-slot-refcount
    (+ (aget port port-slot-refcount) n)))

(defn remote-port-untap [_ ^objects port n]
  (aset port port-slot-refcount
    (- (aget port port-slot-refcount) n)))

(defn peer-transfer [^Peer peer]
  (let [^objects state (.-state peer)
        ^objects queues (.-queues peer)
        ^ints pushes (.-pushes peer)]
    (loop [toggle #{}
           change {}
           freeze #{}
           tap-pull 0
           untap-pull 0
           toggle-pull 0
           change-pull 0]
      (let [^objects tap-queue (aget queues peer-queue-tap)
            ^objects untap-queue (aget queues peer-queue-untap)
            ^objects toggle-queue (aget queues peer-queue-toggle)
            ^objects ready-queue (aget queues peer-queue-ready)]
        (if-some [^objects remote-port (aget tap-queue tap-pull)]
          (let [prev (aget remote-port port-slot-requested)]
            (aset tap-queue tap-pull nil)
            (aset remote-port port-slot-requested (inc prev))
            (reduce-kv local-port-tap nil (port-deps remote-port))
            (recur (if (zero? (+ prev (aget remote-port port-slot-refcount)))
                     (conj toggle (port-slot remote-port)) toggle) change freeze
              (rem (unchecked-inc-int tap-pull)
                (alength tap-queue)) untap-pull toggle-pull change-pull))
          (if-some [^objects remote-port (aget untap-queue untap-pull)]
            (let [curr (dec (aget remote-port port-slot-requested))]
              (aset untap-queue untap-pull nil)
              (aset remote-port port-slot-requested curr)
              (run! local-port-untap (port-deps remote-port))
              (recur (if (zero? (+ curr (aget remote-port port-slot-requested)))
                       (conj toggle (port-slot remote-port)) toggle) change freeze
                tap-pull (rem (unchecked-inc-int untap-pull)
                           (alength untap-queue)) toggle-pull change-pull))
            (if-some [^objects local-port (aget toggle-queue toggle-pull)]
              (let [deps (port-deps local-port)]
                (aset toggle-queue toggle-pull nil)
                (if (zero? (aget local-port port-slot-requested))
                  (do (aset local-port port-slot-requested (identity 1))
                      (run! remote-port-tap deps)
                      (when (zero? (aget local-port port-slot-refcount))
                        (enable local-port)))
                  (do (aset local-port port-slot-requested (identity 0))
                      (run! remote-port-untap deps)
                      (when (zero? (aget local-port port-slot-refcount))
                        (disable local-port))))
                (recur toggle change freeze tap-pull untap-pull
                  (rem (unchecked-inc-int toggle-pull)
                    (alength toggle-queue)) change-pull))
              (if-some [^objects local-port (aget ready-queue change-pull)]
                (let [slot (port-slot local-port)]
                  (aset ready-queue change-pull nil)
                  (if-some [ps (port-process local-port)]
                    (let [diff @ps]
                      (recur toggle (assoc change
                                      slot (if-some [p (change slot)]
                                             (i/combine p diff) diff))
                        freeze tap-pull untap-pull toggle-pull
                        (rem (unchecked-inc-int change-pull)
                          (alength ready-queue))))
                    (recur toggle change (conj freeze slot)
                      tap-pull untap-pull toggle-pull
                      (rem (unchecked-inc-int change-pull)
                        (alength ready-queue)))))
                (let [acks (aget state peer-slot-output-acks)]
                  (aset state peer-slot-output-acks (identity 0))
                  (aset state peer-slot-output-pending true)
                  (aset pushes peer-queue-tap 0)
                  (aset pushes peer-queue-untap 0)
                  (aset pushes peer-queue-toggle 0)
                  (aset pushes peer-queue-ready 0)
                  (encode [acks toggle change freeze]
                    (aget state peer-slot-writer-opts)))))))))))

(defn frame-shared? [^Frame frame]
  (if-some [^Frame parent (.-parent frame)]
    (let [rank (.-rank frame)
          call-id (.-call-id frame)
          ^objects children (.-children parent)]
      (contains? (aget children call-id) rank)) true))

(defn frame-share [^Frame frame]
  (let [rank (.-rank frame)
        call-id (.-call-id frame)
        ^Frame parent (.-parent frame)
        ^objects children (.-children parent)]
    (aset children call-id
      (assoc (aget children call-id)
        rank frame))))

(defn frame-unshare [^Frame frame]
  (let [rank (.-rank frame)
        call-id (.-call-id frame)
        ^Frame parent (.-parent frame)
        ^objects children (.-children parent)]
    (aset children call-id
      (dissoc (aget children call-id)
        rank frame))))

(defn peer-ack [^Peer peer]
  ;; TODO
  )

(defn peer-toggle [^Peer peer ^Slot slot]
  (peer-push peer peer-queue-toggle (slot-port slot)) peer)

(defn peer-change [^Peer peer ^Slot slot diff]
  ((port-process (slot-port slot)) diff) peer)

(defn peer-freeze [^Peer peer ^Slot slot]
  ((port-process (slot-port slot)) nil) peer)

(defn peer-input-ready [^Peer peer]
  (let [^objects state (.-state peer)
        step (.-step peer)]
    (loop []
      (when (aset state peer-slot-input-busy
              (not (aget state peer-slot-input-busy)))
        (let [[acks toggle change freeze]
              (decode
                (try @(aget state peer-slot-input-process)
                     (catch #?(:clj Throwable :cljs :default) e
                       (pst e)
                       ;; TODO
                       ))
                (aget state peer-slot-reader-opts))]
          (dotimes [_ acks] (peer-ack peer))
          (reduce peer-toggle peer toggle)
          (reduce-kv peer-change peer change)
          (reduce peer-freeze peer freeze)
          (when (pos? (+ (count toggle) (count change) (count freeze)))
            (aset state peer-slot-output-acks
              (inc (aget state peer-slot-output-acks)))
            (when (aget state peer-slot-output-pending)
              (aset state peer-slot-output-pending false)
              (step)))
          (recur))))))

(defn peer-result-diff [^Peer peer diff]
  #_(prn :TODO-result-diff diff)
  peer)

(defn peer-result-success [^Peer peer]
  #_(prn :TODO-result-success))

(defn define-node
  "Defines signals node id for given frame."
  [^Frame frame id expr]
  (define-slot frame id expr))

(defn define-call
  "Defines call site id for given frame."
  [^Frame frame id expr]
  (define-slot frame (+ id (count (.-nodes (frame-cdef frame))))
    (reify Expr
      (deps [_ site] (deps expr site))
      (flow [_]
        (i/latest-product
          (fn [ctor]
            (when-not (instance? Ctor ctor)
              (throw (error (str "Not a constructor - " (pr-str ctor)))))
            (when-not (identical? (frame-peer frame) (ctor-peer ctor))
              (throw (error "Can't call foreign constructor.")))
            (let [^ints ranks (.-ranks frame)
                  rank (aget ranks id)]
              (aset ranks id (inc rank))
              (make-frame frame id rank ctor)))
          (flow expr))))))

(defn define-free
  "Defines free variable id for given constructor."
  [^Ctor ctor id ^Slot slot]
  (let [^objects free (.-free ctor)]
    (when-not (nil? (aget free id))
      (throw (error "Can't redefine free variable.")))
    (aset free id slot) nil))

(defn lookup
  "Returns the value associated with given key in the dynamic environment of given frame."
  {:tag Expr}
  ([^Frame frame key]
   (lookup frame key (->Unbound key)))
  ([^Frame frame key nf]
   (loop [frame frame]
     (if-some [s ((.-env (frame-ctor frame)) key)]
       s (if-some [p (frame-parent frame)]
           (recur p) nf)))))

(defn make-ctor
  "Returns a fresh constructor for cdef coordinates key and idx."
  {:tag Ctor}
  [^Frame frame key idx & frees]
  (let [^Peer peer (frame-peer frame)
        ^Cdef cdef (((.-defs peer) key) idx)
        ctor (->Ctor peer key idx (object-array (.-frees cdef)) {} nil)]
    (run! (partial apply define-free ctor)
      (eduction (map-indexed vector) frees))
    ctor))

(defn node
  "Returns the signal node id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (->Slot frame id))

(defn call
  "Returns the call site id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (->Slot frame (+ (count (.-nodes (frame-cdef frame))) id)))

(defn free
  "Returns the free variable id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (let [^objects free (.-free (frame-ctor frame))]
    (aget free id)))

(defn result
  "Returns the result of given frame."
  {:tag 'objects}
  [^Frame frame]
  (let [^objects ports (.-ports frame)
        ^Cdef cdef (frame-cdef frame)]
    (aget ports (+ (count (.-nodes cdef)) (count (.-calls cdef))))))

(defn peer "
Returns a peer definition from given definitions and main key.
" [events site defs main & args]
  (fn [step done]
    (let [state (object-array peer-slots)
          peer (->Peer site defs step done
                 (doto (object-array peer-queues)
                   (aset peer-queue-tap (object-array 1))
                   (aset peer-queue-untap (object-array 1))
                   (aset peer-queue-toggle (object-array 1))
                   (aset peer-queue-ready (object-array 1)))
                 (int-array peer-queues) state)
          input (m/stream (m/observe events))
          root (->> args
                 (apply bind-args (->Ctor peer main 0 (object-array 0) {} nil))
                 (make-frame nil 0 0))]
      (aset state peer-slot-writer-opts
        {:default-handler (t/write-handler
                            (fn [_] "unserializable")
                            (fn [_] (comment TODO fetch port info)))
         :handlers {Ctor  (t/write-handler
                            (fn [_] "ctor")
                            (fn [^Ctor ctor]
                              (assert (identical? peer (.-peer ctor)))
                              (list* (.-key ctor) (.-idx ctor) (.-env ctor) (.-free ctor))))
                    Slot  (t/write-handler
                            (fn [_] "slot")
                            (fn [^Slot slot]
                              [(.-frame slot) (.-id slot)]))
                    Frame (t/write-handler
                            (fn [_] "frame")
                            (fn [^Frame frame]
                              [(frame-path frame)
                               (when-not (frame-shared? frame)
                                 (frame-share frame)
                                 (.-ctor frame))]))
                    Join (t/write-handler
                           (fn [_] "join")
                           (fn [^Join join]
                             (.-input join)))
                    Ap (t/write-handler
                         (fn [_] "ap")
                         (fn [^Ap ap]
                           (.-inputs ap)))
                    Pure (t/write-handler
                           (fn [_] "pure")
                           (fn [^Pure pure]
                             (.-values pure)))}})
      (aset state peer-slot-reader-opts
        {:handlers {"ctor"           (t/read-handler
                                       (fn [[key idx env & free]]
                                         (->Ctor peer key idx (object-array free) env nil)))
                    "slot"           (t/read-handler
                                       (fn [[frame id]]
                                         (->Slot frame id)))
                    "frame"          (t/read-handler
                                       (fn [[path ctor]]
                                         (if (nil? ctor)
                                           (peer-frame peer path)
                                           (let [[call rank] (peek path)
                                                 parent (peer-frame peer (pop path))
                                                 frame (make-frame parent call rank ctor)]
                                             (frame-share frame) frame))))
                    "join"           (t/read-handler
                                       (fn [input]
                                         (->Join input nil)))
                    "ap"             (t/read-handler
                                       (fn [inputs]
                                         (->Ap inputs nil)))
                    "pure"           (t/read-handler
                                       (fn [values]
                                         (->Pure values nil)))
                    "unserializable" (t/read-handler
                                       (fn [_]
                                         (->Failure :unserializable)))}})
      (aset state peer-slot-output-acks (identity 0))
      (aset state peer-slot-output-pending true)
      (aset state peer-slot-input-busy true)
      (aset state peer-slot-input-process
        (input #(peer-input-ready peer) done))
      (aset state peer-slot-root root)
      (when (= site :client)
        (aset state peer-slot-result
          ((m/reduce peer-result-diff peer
             (m/signal i/combine (port-flow (result root))))
           peer-result-success pst)))
      (peer-input-ready peer) peer)))

;; local only
(defn root-frame [defs main]
  (->> (bind-args (->Ctor (->Peer :client defs nil nil nil nil nil)
                    main 0 (object-array 0) {} nil))
    (make-frame nil 0 0)
    (m/signal i/combine)))

#?(:clj
   (def arg-sym
     (map (comp symbol
            (partial intern *ns*)
            (fn [i]
              (with-meta (symbol (str "%" i))
                {::type ::node})))
       (range))))

(defn get-destructure-map [gmap]
  (if (seq? gmap)
    (if (next gmap)
      (apply array-map gmap)
      (if (seq gmap) (first gmap) {}))
    gmap))
