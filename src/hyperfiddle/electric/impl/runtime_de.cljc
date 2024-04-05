(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [cognitect.transit :as t])
  (:import missionary.Cancelled
           #?(:clj (clojure.lang IFn IDeref))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream Writer))))

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
(def port-slot-state 7)
(def port-slots 8)

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

(defn port-flow [^objects port]
  (aget port port-slot-flow))

(defn port-deps [^objects port]
  (aget port port-slot-deps))

(deftype Frame [slot rank site ctor
                ^ints ranks ^objects children
                ^objects calls ^objects nodes
                ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo (hash-combine (hash slot) (hash rank)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Frame other)
      (= slot (.-slot ^Frame other))
      (= rank (.-rank ^Frame other))))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((port-flow (aget nodes (dec (alength nodes)))) step done)))

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

(declare port-slot)

(deftype Remote [port step done]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (aset ^objects port port-slot-process nil)
    (if (nil? (aget ^objects port port-slot-state))
      (step) (aset ^objects port port-slot-state nil)))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (if-some [value (aget ^objects port port-slot-state)]
      (do (aset ^objects port port-slot-state nil)
          (when-not (identical? this (port-process port))
            (done)) value)
      (do (done) (throw (Cancelled. "Remote port cancelled."))))))

(defn port-freeze [^objects port]
  (aset port port-slot-process nil)
  (when (nil? (aget port port-slot-state))
    (when-some [^Remote ps (aget port port-slot-process)]
      ((.-done ps)))))

(defn port-change [^objects port value]
  (if-some [prev (aget port port-slot-state)]
    (aset port port-slot-state (i/combine prev value))
    (do (aset port port-slot-state value)
        (when-some [^Remote ps (aget port port-slot-process)]
          ((.-step ps))))))

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

(defn slot-port
  {:tag 'objects}
  [^Slot slot]
  (let [id (.-id slot)
        ^Frame frame (.-frame slot)]
    (if (neg? id)
      (aget ^objects (.-nodes frame) (- -1 id))
      (aget ^objects (.-calls frame) id))))

(defn port-ready [^objects port]
  (peer-push (frame-peer (.-frame (port-slot port))) peer-queue-ready port))

(defn frame-path [^Frame frame]
  (loop [^Frame frame frame
         path ()]
    (if-some [^Slot slot (.-slot frame)]
      (recur (.-frame slot)
        (conj path [(.-id slot) (.-rank ^Frame frame)]))
      (vec path))))

(defn define-slot [^Slot slot expr]
  (let [^Frame frame (.-frame slot)
        id (.-id slot)
        site (if-some [site (let [cdef (ctor-cdef (frame-ctor frame))
                                  nodes (.-nodes cdef)
                                  calls (.-calls cdef)]
                              (if (neg? id)
                                (let [id (- -1 id)]
                                  (if (= id (count nodes))
                                    (.-result cdef) (nodes id)))
                                (calls id)))]
               site (frame-site frame))
        local? (= site (.-site (frame-peer frame)))
        port (if (instance? Slot expr)
               (slot-port expr)
               (let [port (object-array port-slots)]
                 (aset port port-slot-slot slot)
                 (aset port port-slot-site site)
                 (aset port port-slot-deps (deps expr site))
                 (aset port port-slot-flow
                   (m/signal i/combine
                     (if local?
                       (flow expr)
                       (fn [step done]
                         (let [ps (->Remote port step done)]
                           (aset port port-slot-process ps)
                           (step) ps)))))
                 (aset port port-slot-refcount (identity 0))
                 (aset port port-slot-requested (identity 0))
                 (aset port port-slot-state (if local? false (i/empty-diff 0)))
                 port))]
    (if (neg? id)
      (aset ^objects (.-nodes frame) (- -1 id) port)
      (aset ^objects (.-calls frame) id port)) nil))

(defn make-frame [^Slot slot rank site ctor]
  (let [cdef (ctor-cdef ctor)
        callc (count (.-calls cdef))
        nodec (count (.-nodes cdef))
        frame (->Frame slot rank site ctor
                (int-array (inc callc)) (object-array callc) (object-array callc)
                (object-array (inc nodec)) nil)]
    (define-slot (->Slot frame (- -1 nodec)) ((.-build cdef) frame)) frame))

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

(defn enable [^objects port]
  (aset port port-slot-process
    ((port-flow port)
     #(port-ready port)
     #(do (aset port port-slot-state true)
          (port-ready port)))))

(defn disable [^objects port]
  (let [ps (port-process port)]
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

(defn port-coordinates [^objects port]
  (let [slot (port-slot port)]
    [(frame-path (.-frame slot)) (.-id slot)]))

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
          (do (aset tap-queue tap-pull nil)
              (let [prev (aget remote-port port-slot-requested)]
                (aset remote-port port-slot-requested (inc prev))
                (reduce-kv local-port-tap nil (port-deps remote-port))
                (recur (if (zero? (+ prev (aget remote-port port-slot-refcount)))
                         (conj toggle (port-slot remote-port)) toggle) change freeze
                  (rem (unchecked-inc-int tap-pull)
                    (alength tap-queue)) untap-pull toggle-pull change-pull)))
          (if-some [^objects remote-port (aget untap-queue untap-pull)]
            (do (aset untap-queue untap-pull nil)
                (let [curr (dec (aget remote-port port-slot-requested))]
                  (aset remote-port port-slot-requested curr)
                  (reduce-kv local-port-untap nil (port-deps remote-port))
                  (recur (if (zero? (+ curr (aget remote-port port-slot-refcount)))
                           (conj toggle (port-slot remote-port)) toggle) change freeze
                    tap-pull (rem (unchecked-inc-int untap-pull)
                               (alength untap-queue)) toggle-pull change-pull)))
            (if-some [^objects local-port (aget toggle-queue toggle-pull)]
              (do (aset toggle-queue toggle-pull nil)
                  (if (zero? (aget local-port port-slot-requested))
                    (do (aset local-port port-slot-requested (identity 1))
                        (reduce-kv remote-port-tap nil (port-deps local-port))
                        (when (zero? (aget local-port port-slot-refcount))
                          (enable local-port)))
                    (do (aset local-port port-slot-requested (identity 0))
                        (reduce-kv remote-port-untap nil (port-deps local-port))
                        (when (zero? (aget local-port port-slot-refcount))
                          (disable local-port))))
                (recur toggle change freeze tap-pull untap-pull
                  (rem (unchecked-inc-int toggle-pull)
                    (alength toggle-queue)) change-pull))
              (if-some [^objects local-port (aget ready-queue change-pull)]
                (do (aset ready-queue change-pull nil)
                    (if-some [ps (port-process local-port)]
                      (if (aget local-port port-slot-state)
                        (recur toggle change (conj freeze (port-slot local-port))
                          tap-pull untap-pull toggle-pull
                          (rem (unchecked-inc-int change-pull)
                            (alength ready-queue)))
                        (let [diff @ps
                              slot (port-slot local-port)]
                          (recur toggle (assoc change
                                          slot (if-some [p (change slot)]
                                                 (i/combine p diff) diff))
                            freeze tap-pull untap-pull toggle-pull
                            (rem (unchecked-inc-int change-pull)
                              (alength ready-queue)))))
                      (recur toggle change freeze tap-pull untap-pull toggle-pull change-pull)))
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
  (if-some [^Slot slot (.-slot frame)]
    (let [^Frame parent (.-frame slot)
          ^objects children (.-children parent)]
      (contains? (aget children (.-id slot)) (.-rank frame))) true))

(defn frame-share [^Frame frame]
  (let [^Slot slot (.-slot frame)
        ^Frame parent (.-frame slot)
        ^objects children (.-children parent)
        id (.-id slot)]
    (aset children id (assoc (aget children id) (.-rank frame) frame))))

(defn frame-unshare [^Frame frame]
  (let [^Slot slot (.-slot frame)
        ^Frame parent (.-frame slot)
        ^objects children (.-children parent)
        id (.-id slot)]
    (aset children id (dissoc (aget children id) (.-rank frame) frame))))

(defn peer-ack [^Peer peer]
  ;; TODO
  )

(defn peer-toggle [^Peer peer ^Slot slot]
  (peer-push peer peer-queue-toggle (slot-port slot)) peer)

(defn peer-change [^Peer peer ^Slot slot diff]
  (port-change (slot-port slot) diff) peer)

(defn peer-freeze [^Peer peer ^Slot slot]
  (port-freeze (slot-port slot)) peer)

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

(defn port-attach [^Peer peer ^objects port n]
  (dotimes [_ n] (peer-push peer peer-queue-tap port)) peer)

(defn port-detach [^Peer peer ^objects port n]
  (dotimes [_ n] (peer-push peer peer-queue-untap port)) peer)

(defn frame-up [^Frame frame]
  (let [^objects nodes (.-nodes frame)
        result (aget nodes (dec (alength nodes)))
        site (port-site (slot-port (.-slot frame)))]
    (reduce-kv port-attach (frame-peer frame)
      (deps (port-slot result) site))
    (port-flow result)))

(defn frame-down [^Frame frame]
  (let [^objects nodes (.-nodes frame)
        result (aget nodes (dec (alength nodes)))
        site (port-site (slot-port (.-slot frame)))]
    (reduce-kv port-detach (frame-peer frame)
      (deps (port-slot result) site))))

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

(def call-slot-slot 0)
(def call-slot-buffer 1)
(def call-slots 2)

(defn call-transfer [^objects state {:keys [grow degree shrink permutation change freeze]}]
  (let [^Slot slot (aget state call-slot-slot)
        ^Frame parent (.-frame slot)
        id (.-id slot)
        ^ints ranks (.-ranks parent)
        site (port-site (slot-port slot))
        size-after (- degree shrink)
        ^objects buffer (let [^objects buffer (aget state call-slot-buffer)
                              cap (alength buffer)]
                          (if (< degree cap)
                            buffer (let [b (object-array (loop [cap cap]
                                                           (let [cap (bit-shift-left cap 1)]
                                                             (if (< degree cap)
                                                               cap (recur cap)))))]
                                     #?(:clj  (System/arraycopy buffer 0 b 0 cap)
                                        :cljs (dotimes [i cap] (aset b i (aget buffer i))))
                                     (aset state call-slot-buffer b))))]
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
                               (when-not (instance? Ctor ctor)
                                 (throw (error (str "Not a constructor - " (pr-str ctor)))))
                               (when-not (identical? (frame-peer parent) (ctor-peer ctor))
                                 (throw (error "Can't call foreign constructor.")))
                               (when-some [frame (aget buffer i)] (frame-down frame))
                               (let [rank (aget ranks id)
                                     frame (make-frame slot rank site ctor)]
                                 (aset buffer i frame)
                                 (aset ranks id (inc rank))
                                 (assoc change i (frame-up frame))))
                    {} change)}))

(deftype Call [expr slot]
  Expr
  (deps [_ site] (deps expr site))
  (flow [_]
    (fn [step done]
      (let [state (doto (object-array call-slots)
                    (aset call-slot-slot slot)
                    (aset call-slot-buffer (object-array 1)))
            ps ((flow expr) step done)]
        (reify
          IFn
          (#?(:clj invoke :cljs -invoke) [_] (ps))
          IDeref
          (#?(:clj deref :cljs -deref) [_] (call-transfer state @ps)))))))

(defn define-call
  "Defines call site id for given frame."
  [^Frame frame id expr]
  (let [slot (call frame id)]
    (define-slot slot (->Call expr slot))))

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
       s (if-some [^Slot slot (.-slot frame)]
           (recur (.-frame slot)) nf)))))

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

(defn free
  "Returns the free variable id for given frame."
  {:tag Slot}
  [^Frame frame id]
  (let [^objects free (.-free (frame-ctor frame))]
    (aget free id)))

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
          ^Frame root (->> args
                        (apply bind-args (->Ctor peer main 0 (object-array 0) {} nil))
                        (make-frame nil 0 :client))]
      (aset state peer-slot-writer-opts
        {:default-handler (t/write-handler
                            (fn [_] "unserializable")
                            (fn [_] (comment TODO fetch port info)))
         :handlers        {Ctor  (t/write-handler
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
                           Join  (t/write-handler
                                   (fn [_] "join")
                                   (fn [^Join join]
                                     (.-input join)))
                           Ap    (t/write-handler
                                   (fn [_] "ap")
                                   (fn [^Ap ap]
                                     (.-inputs ap)))
                           Pure  (t/write-handler
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
                                           (let [[id rank] (peek path)
                                                 parent (peer-frame peer (pop path))
                                                 slot (call parent id)
                                                 site (port-site (slot-port slot))
                                                 frame (make-frame slot rank site ctor)]
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
        (let [^objects nodes (.-nodes root)
              result (aget nodes (dec (alength nodes)))]
          (reduce-kv port-attach peer (deps (port-slot result) site))
          (aset state peer-slot-result
            ((m/reduce peer-result-diff peer (port-flow result))
             peer-result-success pst))))
      (peer-input-ready peer) peer)))

#?(:clj
   (defmethod print-method Slot [^Slot slot ^Writer w]
     (.write w "#Slot[")
     (print-method (.-frame slot) w)
     (.write w " ")
     (print-method (.-id slot) w)
     (.write w "]")))

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
     (.write w "]")))

;; local only
(defn root-frame [defs main]
  (->> (bind-args (->Ctor (->Peer :client defs nil nil nil nil nil)
                    main 0 (object-array 0) {} nil))
    (make-frame nil 0 :client)
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

(defn cannot-resolve [& args] (throw (ex-info "definition called on a peer that doesn't support it" {:args args})))

(defn tracing [o dot] (prn '[o_o] o '=>> dot) dot)
