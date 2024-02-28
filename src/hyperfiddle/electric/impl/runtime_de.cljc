(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [contrib.assert :as ca])
  (:import #?(:clj (clojure.lang IFn IDeref))
           missionary.Cancelled))

(set! *warn-on-reflection* true)

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (.error js/console e)))

(def peer-slot-root 0)
(def peer-slot-input-process 1)
(def peer-slot-input-busy 2)
(def peer-slot-output-pending 9)
(def peer-slot-result 10)
(def peer-slots 11)

(declare peer-cancel peer-transfer)

(deftype Peer [site defs step done queues pushes state]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (peer-cancel this))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (peer-transfer this)))

;; Pure | Ap | Effect | Join | Port
(defprotocol Expr
  (deps [_])                                                ;; returns #{Port}
  (flow [_]))                                               ;; returns incseq

(defn invariant [x] (m/cp x))

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
  (deps [_] #{})
  (flow [_] (apply i/fixed (map invariant values)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(defn pure "
(FN (IS VOID))
(FN (IS T) T)
(FN (IS T) T T)
(FN (IS T) T T T)
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
  (deps [_] (into #{} (mapcat deps) inputs))
  (flow [_] (apply i/latest-product invoke (map flow inputs)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(defn ap "
(FN (IS T) (IS (FN T)))
(FN (IS T) (IS (FN T A)) (IS A))
(FN (IS T) (IS (FN T A B)) (IS A) (IS B))
(FN (IS T) (IS (FN T A B C)) (IS A) (IS B) (IS C))
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
  (deps [_] (deps input))
  (flow [_] (i/latest-concat (flow input)))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(defn join "
(FN (IS T) (IS (IS T)))
" [input] (->Join input nil))

(defn effect [incseq]
  (join (pure incseq)))

(def fixed-signals "
(FN (IS VOID))
(FN (IS T) (CF T))
(FN (IS T) (CF T) (CF T))
(FN (IS T) (CF T) (CF T) (CF T))
" (comp (partial m/signal i/combine) i/fixed))

(defn drain "
(FN (IS VOID) (IS T))
" [incseq]
  (m/ap
    (m/amb (i/empty-diff 0)
      (do (m/?> incseq) (m/amb)))))

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
            (recur (hash-combine h (hash (aget free i)))
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
              true (if (= (aget free i) (aget f i))
                     (recur (inc i)) false))) false)))))

(defn bind "
(FN (CTOR T) (CTOR T))
(FN (CTOR T) (CTOR T) (VAR A) (IS A))
(FN (CTOR T) (CTOR T) (VAR A) (IS A) (VAR B) (IS B))
(FN (CTOR T) (CTOR T) (VAR A) (IS A) (VAR B) (IS B) (VAR C) (IS C))
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
    ((flow (result this)) step done)))

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

(defn node-site
  "Returns the site of given node."
  [^Frame frame id]
  (if-some [site (nth (.-nodes (frame-cdef frame)) id)]
    site (frame-site frame)))

(defn call-site
  "Returns the site of given call."
  [^Frame frame id]
  (if-some [site (nth (.-calls (frame-cdef frame)) id)]
    site (frame-site frame)))

(defn result-site
  "Returns the site of result."
  [^Frame frame]
  (if-some [site (.-result (frame-cdef frame))]
    site (frame-site frame)))

(def port-slot-deps 0)
(def port-slot-flow 1)
(def port-slot-refcount 2)
(def port-slot-process 3)
(def port-slot-frozen 4)
(def port-slots 5)

(deftype Port [frame id ^objects state
               ^:unsynchronized-mutable ^:mutable hash-memo]
  #?(:clj Object)
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (if-some [h hash-memo]
      h (set! hash-memo
          (hash-combine (hash frame) (hash id)))))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [_ other]
    (and (instance? Port other)
      (= frame (.-frame ^Port other))
      (= id (.-id ^Port other))))
  Expr
  (deps [_] (aget state port-slot-deps))
  (flow [_] (aget state port-slot-flow))
  IFn
  (#?(:clj invoke :cljs -invoke) [this step done]
    ((flow this) step done)))

(declare port-detach)

(deftype Remote [^Port port step done ^:unsynchronized-mutable ^:mutable diff]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (port-detach port)
    (if (nil? diff)
      (step) (set! diff nil)))
  (#?(:clj invoke :cljs -invoke) [_ value]
    (if (nil? value)
      (do (port-detach port)
          (when (nil? diff) (done)))
      (set! diff
        (if-some [prev diff]
          (i/combine prev value)
          (do (step) value)))))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (if-some [value diff]
      (do (set! diff nil)
          (when (identical? this
                  (aget ^objects (.-state port) port-slot-process))
            (done)) value)
      (do (done) (throw (Cancelled. "Input cancelled."))))))

(defn deregister-from-parent [^Frame frame]
  (when-some [^Frame parent (.-parent frame)]
    (let [^objects children (.-children parent)
          call-id (.-call-id frame)
          siblings (dissoc (aget children call-id) (.-rank frame))]
      (case siblings
        {} (let [^ints ranks (.-ranks parent)
                 callc (frame-call-count parent)
                 refcount (aget ranks callc)]
             (aset ranks callc (unchecked-dec-int refcount))
             (aset children call-id nil)
             (when (zero? refcount) (recur parent)))
        (aset children call-id siblings)))))

(defn register-to-parent [^Frame frame]
  (when-some [^Frame parent (.-parent frame)]
    (let [^objects children (.-children parent)
          call-id (.-call-id frame)
          siblings (aget children call-id)]
      (case siblings
        nil (let [^ints ranks (.-ranks parent)
                  callc (frame-call-count parent)
                  refcount (aget ranks callc)]
              (aset ranks callc (unchecked-inc-int refcount))
              (aset children call-id {(.-rank frame) frame})
              (when (zero? refcount) (recur parent)))
        (aset children call-id (assoc siblings (.-rank frame) frame))))))

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

(defn peer-tap [^Peer peer port]
  (peer-push peer 0 port) peer)

(defn port-attach [^Port port ps]
  (aset ^objects (.-state port) port-slot-process ps)
  (let [^Frame frame (.-frame port)
        ^ints ranks (.-ranks frame)
        callc (frame-call-count frame)
        refcount (aget ranks callc)]
    (aset ranks callc (unchecked-inc-int refcount))
    (when (zero? refcount) (register-to-parent frame))
    (reduce peer-tap (frame-peer frame) (deps port))))

(defn peer-untap [^Peer peer port]
  (peer-push peer 1 port) peer)

(defn port-detach [^Port port]
  (aset ^objects (.-state port) port-slot-process nil)
  (let [^Frame frame (.-frame port)
        ^ints ranks (.-ranks frame)
        callc (frame-call-count frame)
        refcount (unchecked-dec-int (aget ranks callc))]
    (aset ranks callc refcount)
    (when (zero? refcount) (deregister-from-parent frame))
    (reduce peer-untap (frame-peer frame) (deps port))))

(defn port-ready [^Port port]
  (peer-push (frame-peer (.-frame port)) 2 port))

(defn port-tap [^Port port]
  (let [^objects state (.-state port)
        prev (aget state port-slot-refcount)]
    (aset state port-slot-refcount (inc prev))
    (when (zero? prev)
      (aset state port-slot-process
        ((flow port)
         #(port-ready port)
         #(do (aset state port-slot-frozen true)
              (port-ready port)))))))

(defn port-untap [^Port port]
  (let [^objects state (.-state port)
        curr (dec (aget state port-slot-refcount))]
    (aset state port-slot-refcount curr)
    (when (zero? curr)
      ((aget state port-slot-process)))))

(defn make-local [frame id incseq]
  (let [state (object-array port-slots)
        port (->Port frame id state nil)]
    (aset state port-slot-deps #{port})
    (aset state port-slot-flow incseq)
    (aset state port-slot-refcount (identity 0))
    port))

(defn make-remote [frame id deps]
  (let [state (object-array port-slots)
        port (->Port frame id state nil)]
    (aset state port-slot-deps deps)
    (aset state port-slot-flow
      (m/signal i/combine
        (fn [step done]
          (let [ps (->Remote port step done (i/empty-diff 0))]
            (port-attach port ps) (step) ps)))) port))

(defn make-frame [^Frame parent call-id rank site ctor]
  (let [peer (ctor-peer ctor)
        cdef (ctor-cdef ctor)
        callc (count (.-calls cdef))
        result (+ (count (.-nodes cdef)) callc)
        ports (object-array (inc result))
        frame (->Frame parent call-id rank site ctor
                (int-array (inc callc)) (object-array callc) ports nil)
        expr ((.-build cdef) frame)]
    (aset ports result
      (if (instance? Port expr)
        expr (if (= (.-site peer) (result-site frame))
               (make-local frame nil (flow expr))
               (make-remote frame nil (deps expr))))) frame))

(defn peer-cancel [^Peer peer]
  (prn :TODO-cancel))

(defn peer-transfer [^Peer peer]
  (let [^objects peer-state (.-state peer)
        ^objects queues (.-queues peer)
        ^ints pushes (.-pushes peer)]
    (loop [insts []
           tap-pull 0
           untap-pull 0
           ready-pull 0]
      (let [^objects tap-queue (aget queues 0)
            ^objects untap-queue (aget queues 1)
            ^objects ready-queue (aget queues 2)]
        (if-some [port (aget tap-queue tap-pull)]
          (do (aset tap-queue tap-pull nil)
              (port-tap port)
              (recur insts
                (rem (unchecked-inc-int tap-pull)
                  (alength tap-queue)) untap-pull ready-pull))
          (if-some [port (aget untap-queue untap-pull)]
            (do (aset untap-queue untap-pull nil)
                (port-untap port)
                (recur insts tap-pull
                  (rem (unchecked-inc-int untap-pull)
                    (alength untap-queue)) ready-pull))
            (if-some [^Port port (aget ready-queue ready-pull)]
              (do (aset ready-queue ready-pull nil)
                  (recur (conj insts
                           (loop [^Frame frame (.-frame port)
                                  path ()]
                             (if-some [parent (.-parent frame)]
                               (recur parent (conj path [(.-call-id frame) (.-rank frame)]))
                               [path (.-id port) @(aget ^objects (.-state port) port-slot-process)])))
                    tap-pull untap-pull
                    (rem (unchecked-inc-int ready-pull)
                      (alength ready-queue))))
              (do (aset peer-state peer-slot-output-pending true)
                  (aset pushes 0 0)
                  (aset pushes 1 0)
                  (aset pushes 2 0)
                  insts))))))))

(defn child-at [^Frame frame [call-id rank]]
  (let [^objects children (.-children frame)]
    (get (aget children call-id) rank)))

(defn peer-apply-change [^Peer peer [path id diff]]
  (let [^objects state (.-state peer)
        ^Frame frame (reduce child-at (aget state peer-slot-root) path)
        ^objects ports (.-ports frame)]
    ((aget ports id) diff) peer))

(defn peer-input-ready [^Peer peer]
  (let [^objects state (.-state peer)]
    (loop []
      (when (aset state peer-slot-input-busy
              (not (aget state peer-slot-input-busy)))
        (try (reduce peer-apply-change peer
               @(aget state peer-slot-input-process))
             (catch #?(:clj Throwable :cljs :default) e
               (pst e)
               ;; TODO
               ))
        (recur)))))

(defn peer-result-diff [^Peer peer diff]
  (prn :TODO-result-diff diff)
  peer)

(defn peer-result-success [^Peer peer]
  (prn :TODO-result-success))

(defn define-node
  "Defines signals node id for given frame."
  [^Frame frame id expr]
  (let [^objects ports (.-ports frame)
        site (node-site frame id)]
    (when-not (nil? (aget ports id))
      (throw (error "Can't redefine signal node.")))
    (aset ports id
      (if (instance? Port expr)
        expr (if (= site (.-site (frame-peer frame)))
               (make-local frame id (m/signal i/combine (flow expr)))
               (make-remote frame id (deps expr))))) nil))

(defn define-call
  "Defines call site id for given frame."
  [^Frame frame id expr]
  (let [^objects ports (.-ports frame)
        slot (+ id (count (.-nodes (frame-cdef frame))))
        site (call-site frame id)]
    (when-not (nil? (aget ports slot))
      (throw (error "Can't redefine call site.")))
    (aset ports slot
      (if (= site (.-site (frame-peer frame)))
        (make-local frame id
          (m/signal i/combine
            (i/latest-product
              (fn [ctor]
                (when-not (instance? Ctor ctor)
                  (throw (error (str "Not a constructor - " (pr-str ctor)))))
                (when-not (identical? (frame-peer frame) (ctor-peer ctor))
                  (throw (error "Can't call foreign constructor.")))
                (let [^ints ranks (.-ranks frame)
                      rank (aget ranks id)]
                  (aset ranks id (inc rank))
                  (make-frame frame id rank site ctor)))
              (flow expr))))
        (make-remote frame id (deps expr)))) nil))

(defn define-free
  "Defines free variable id for given constructor."
  [^Ctor ctor id incseq]
  (let [^objects free (.-free ctor)]
    (when-not (nil? (aget free id))
      (throw (error "Can't redefine free variable.")))
    (aset free id incseq) nil))

(defn lookup
  "Returns the value associated with given key in the dynamic environment of given frame."
  ([^Frame frame key]
   (lookup frame key (->Unbound key)))
  ([^Frame frame key nf]
   (loop [frame frame]
     (if-some [s ((.-env (frame-ctor frame)) key)]
       s (if-some [p (frame-parent frame)]
           (recur p) nf)))))

(defn make-ctor
  "Returns a fresh constructor for cdef coordinates key and idx."
  [^Frame frame key idx]
  (let [^Peer peer (frame-peer frame)
        ^Cdef cdef ((ca/check some? ((.-defs peer) key) {:key key}) idx)]
    (->Ctor peer key idx (object-array (.-frees cdef)) {} nil)))

(defn node
  "Returns the signal node id for given frame."
  [^Frame frame id]
  (let [^objects ports (.-ports frame)]
    (aget ports id)))

(defn free
  "Returns the free variable id for given frame."
  [^Frame frame id]
  (let [^objects free (.-free (frame-ctor frame))]
    (aget free id)))

(defn call
  "Returns the call site id for given frame."
  [^Frame frame id]
  (let [^objects ports (.-ports frame)
        ^Cdef cdef (frame-cdef frame)]
    (aget ports (+ (count (.-nodes cdef)) id))))

(defn result
  "Returns the result of given frame."
  [^Frame frame]
  (let [^objects ports (.-ports frame)
        ^Cdef cdef (frame-cdef frame)]
    (aget ports (+ (count (.-nodes cdef)) (count (.-calls cdef))))))

(defn peer "
Returns a peer definition from given definitions and main key.
" [site defs main & args]
  (fn [events]
    (fn [step done]
      (let [state (object-array peer-slots)
            peer (->Peer site defs step done
                   (doto (object-array 3)
                     (aset 0 (object-array 1))
                     (aset 1 (object-array 1))
                     (aset 2 (object-array 1)))
                   (int-array 3) state)
            input (m/stream (m/observe events))
            root (->> args
                   (apply bind-args (->Ctor peer main 0 (object-array 0) {} nil))
                   (make-frame nil 0 0 :client))]
        (aset state peer-slot-output-pending true)
        (aset state peer-slot-input-busy true)
        (aset state peer-slot-input-process
          (input #(peer-input-ready peer) done))
        (aset state peer-slot-root root)
        (case site
          :client (aset state peer-slot-result
                    ((m/reduce peer-result-diff peer
                       (m/signal i/combine (flow (result root))))
                     peer-result-success pst))
          :server (reduce peer-tap peer (deps (result root))))
        (peer-input-ready peer) peer))))

;; local only
(defn root-frame [defs main]
  (->> (bind-args (->Ctor (->Peer :client defs nil nil nil nil nil)
                    main 0 (object-array 0) {} nil))
    (make-frame nil 0 0 :client)
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

(def %arity nil)
(def %argv nil)
