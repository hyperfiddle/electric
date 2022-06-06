(ns ^:no-doc hyperfiddle.photon-impl.runtime
  (:refer-clojure :exclude [eval compile])
  (:require [hyperfiddle.photon-impl.yield :refer [yield]]
            [hyperfiddle.photon-impl.local :as l]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str])
  (:import missionary.Cancelled
           (hyperfiddle.photon Failure Pending Remote)
           #?(:clj (clojure.lang IFn IDeref Atom))))

;; A photon program is a tree, which structure is dynamically maintained.
;; Two peers are synchronized (through a protocol) such that the tree structure is identicaly on both peers.
;; Two type of nodes:
;; [Frames] : A piece of DAG with a static structure. Won't be rearanged at runtime. (AKA Static Frame)
;;            - A set of compiled s-expressions + a set of signals weaving these expressions + N inputs + N outputs
;;            - A frame has 2 instances, one on client, one on server.
;;            - Server's outputs are client's inputs and vice-versa.
;;            - Frames are processes.
;;            - Image: a stackframe but for a DAG. A stackframe is allocated to compute the result of a function.
;;              It is volatile (disposable) in a stack-based program. Since photon is reactive, the frame is not disposable.
;;              «ReactiveFrame» «Distributed Reactive Frame»
;; [Objects] : For each `new` in a frame, a managed process is created. Objects are child processes of frames. (AKA Dynamic Frame)
;;             - Parent process of an object is always a frame.
;;             - Parent process of a frame is alawys an object.
;;             - Specificity: Frames have a fixed set of children, objects have a dynamic set of children, they can spawn new frames anytime.
;;                            Child frames of an object are positioned (there is a well defined traversal order)
;;                            Node order (positions) can change at runtime because objects can spawn dynamically (e.g.: p/for).
;;             - Some objects don't have child frames : e.g. (new (m/watch .)), no child frames, no input, no output
;;             - Some frames don't have child objects : e.g. a frame without any `new`, no variability.
;;             - (image: an ordered tree with different kind of nodes at each generation)

;; There is a virtual machine. Its instructions are numbers, they go by pairs.
;; [ Frame Identifier , Argument ]
;; If argument is a negative number, it means we are moving the frame.
;; If argument is a positive number, it identifies a slot in the frame.
;; Slot are stically known places, exposed to the remote frame over network, it can be :
;; 1. A frame input address, (when the remote frame produces an output, an instruction will identify then set the addressed local frame input)
;; 2. The address of p/fn in the remote frame (aka target)
;; 3. The address of `new` in the remote frame (aka source)

;; Creating a frame takes 2 instructions: the receipe, then the place where it should be created.
;; 1. Location (address) of p/fn (which frame receipe do we want to instantiate)
;; 2. Where (address) do we want the instance of p/fn to be instantiated (which object should get a new child frame )


;; # network protocol
;; After boot, both peers are allowed to send messages to each other at any point in time without coordination.
;; A message is a vector of arbitrary values (the data frames) ending with a vector of integers (the control frame).
;; Each peer must process messages sequentially. Decoding a message requires a queue (input values, initially 
;; empty) and two registers (the current frame and the current target, initially undefined).
;; 1. push data frame values to the queue
;; 2. process control frame integers as a sequence of instructions (for each):
;;   * if current frame is undefined : (TODO what happens if the instruction does not resolve to a known frame?)
;;     1. lookup frame identified by instruction :
;;       * zero identifies the root frame
;;       * strictly positive numbers identify frames created by local sources, by birth index
;;       * strictly negative numbers identify frames created by local variables, by negation of birth index
;;     2. define it as current frame
;;   * if current frame is defined :
;;     * if instruction (argument) is strictly negative :
;;       1. move the current frame to position (+ instruction size-of-source), where
;;          - instruction is the argument
;;          - size-of-source: count of frame's siblings + 1 (how many frame children does the parent object have)
;;       2. undefine current frame
;;     * if instruction is positive or zero :
;;       1. lookup the current frame slot identified by instruction (argument)
;;         * if slot is an input : pull a value from the queue and assign this value to the input
;;         * if slot is a source :
;;           * if current target is defined :
;;             1. construct a new frame from this target and insert it at the last child of source (an object)
;;             2. undefine current target
;;           * if current target is undefined : remove frame at the end of source
;;         * if slot is a target : define it as current target
;;       2. undefine current frame

(defn bind-cb [local value cb]
  #(let [prev (l/get-local local)]
     (l/set-local local value)
     (try (cb) (finally (l/set-local local prev)))))

(defn bind-iterator [local value it]
  (reify
    IFn
    (#?(:clj invoke :cljs -invoke) [_]
      (let [prev (l/get-local local)]
        (l/set-local local value) (it)
        (l/set-local local prev) nil))
    IDeref
    (#?(:clj deref :cljs -deref) [_]
      (let [prev (l/get-local local)]
        (l/set-local local value)
        (try @it (finally (l/set-local local prev)))))))

(defn bind-flow [local value flow]
  (fn [n t]
    (let [prev (l/get-local local)]
      (l/set-local local value)
      (let [it (flow n t)]
        (l/set-local local prev)
        (bind-iterator local value it)))))

(defn steady [x] (m/watch (atom x)))

(defn fail [x] (throw x))

(def failure (some-fn #(when (instance? Failure %) %)))

(def pending (Failure. (Pending.)))

(defn error [msg]
  (#?(:clj Error. :cljs js/Error.) msg))

(defn failer [e n t]
  (n) (reify
        IFn (#?(:clj invoke :cljs -invoke) [_])
        IDeref (#?(:clj deref :cljs -deref) [_] (t) (throw e))))

(def latest-apply
  (partial m/latest
    (fn [f & args]
      (or (apply failure f args)
        (try (apply f args)
             (catch #?(:clj Throwable :cljs :default) e
               (Failure. e)))))))

(def latest-first
  (partial m/latest
    (fn [x y] (if (instance? Failure y) y x))))

(defn recover [p >x]
  (yield (fn [x]
           (when (instance? Failure x)
             (p (.-error ^Failure x)))) >x))

(defn clause
  ([f] (fn [e] (f (steady e))))
  ([f c] (fn [e] (when (instance? c e) (f (steady e))))))

(def lift-cancelled
  (partial comp
    (fn [it]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        IDeref
        (#?(:clj deref :cljs -deref) [_]
          (try @it (catch Cancelled e (Failure. e))))))))

(def this (l/local))

;; 0: parent object
;; 1: position (only mutable field in a frame)
;; 2: id
;; 3: proc object array, size infered statically from source code
;; 4: slot object array, size infered statically from source code
;; 5: tier object array, size infered statically from source code
(defn make-frame [object position id procs slots tiers]
  (doto (object-array 6)
    (aset (int 0) object)
    (aset (int 1) position)
    (aset (int 2) id)
    (aset (int 3) procs)
    (aset (int 4) slots)
    (aset (int 5) tiers)))

(defn frame-object [^objects f]
  (aget f (int 0)))

(defn get-frame-position [^objects f]
  (aget f (int 1)))

(defn set-frame-position [^objects f pos]
  (aset f (int 1) pos))

(defn frame-id [^objects f]
  (aget f (int 2)))

(defn frame-procs ^objects [^objects f]
  (aget f (int 3)))

(defn frame-slots ^objects [^objects f]
  (aget f (int 4)))

(defn frame-tiers ^objects [^objects f]
  (aget f (int 5)))

;; 0: context
;; 1: frame id (reference to parent frame (by id))
;; 2: frame tier (a frame has many child object, as much as `new` statements, tier is the static position of the current object in the parent frame’s child array)
;; 3: remote slot if local, nil otherwise (if this object is a variable, we must reference the corresponding remote source)
;; 4: dynamic scope
;; 5: extra procs
;; 6: buffer of child frame list
;; 7: size of child frame list
;; 8: move callbacks, {k [v !]}
(defn make-object [ctx id tier slot ^objects vars]
  (doto (object-array 9)
    (aset (int 0) ctx)
    (aset (int 1) id)
    (aset (int 2) tier)
    (aset (int 3) slot)
    (aset (int 4) (aclone vars))
    (aset (int 5) {})
    (aset (int 6) (object-array 8))
    (aset (int 7) (identity (int 0)))
    (aset (int 8) {})))

(defn object-context [^objects o]
  (aget o (int 0)))

(defn object-frame [^objects o]
  (aget o (int 1)))

(defn object-tier [^objects o]
  (aget o (int 2)))

(defn object-slot [^objects o]
  (aget o (int 3)))

(defn object-vars ^objects [^objects o]
  (aget o (int 4)))

(defn get-object-procs [^objects o]
  (aget o (int 5)))

(defn set-object-procs [^objects o ps]
  (aset o (int 5) ps))

(defn get-object-buffer ^objects [^objects o]
  (aget o (int 6)))

(defn set-object-buffer ^objects [^objects o x]
  (aset o (int 6) x))

(defn get-object-size [^objects o]
  (aget o (int 7)))

(defn set-object-size [^objects o x]
  (aset o (int 7) x))

(defn get-object-hooks [^objects o]
  (aget o (int 8)))

(defn set-object-hooks [^objects o x]
  (aset o (int 8) x))

;; local counter id
;; remote counter id
;; id -> frame
;; remote callback
;; decoder stack
;; decoder frame register
;; decoder target register
(defn make-context [root]
  (doto (object-array 7)
    (aset (int 0) (identity 0))
    (aset (int 1) (identity 0))
    (aset (int 2) (transient {0 root}))))

(defn next-local [^objects ctx] ; frames are identified by an integer. Creating a frame will increment this counter.
  (aset ctx (int 0) (inc (aget ctx (int 0)))))

(defn next-remote [^objects ctx] ; frames are identified by an integer. Creating a frame will increment this counter.
  (aset ctx (int 1) (dec (aget ctx (int 1)))))

; add a frame in the context index, identified by id
(defn assoc-frame-id [^objects ctx id frame]
  (aset ctx (int 2) (assoc! (aget ctx (int 2)) id frame)))

(defn lookup-frame-id [^objects ctx id]
  (get (aget ctx (int 2)) id))

(defn dissoc-frame-id [^objects ctx id]
  (aset ctx (int 2) (dissoc! (aget ctx (int 2)) id)))

(defn set-remote [^objects ctx callback]
  (aset ctx (int 3) callback))

(defn remote
  ([ctx id slot] (remote ctx id slot nil))
  ([^objects ctx id slot <x]
   (when-some [callback (aget ctx (int 3))]
     (callback [id slot <x]))))

(defn set-queue [^objects ctx xs]
  (aset ctx (int 4) xs) ctx)

(defn pop-queue [^objects ctx]
  (let [[x & xs] (aget ctx (int 4))]
    (aset ctx (int 4) xs) x))

(defn set-frame-register [^objects ctx x]
  (aset ctx (int 5) x))

(defn get-frame-register [^objects ctx]
  (aget ctx (int 5)))

(defn set-target-register [^objects ctx x]
  (aset ctx (int 6) x))

(defn get-target-register [^objects ctx]
  (aget ctx (int 6)))

(defn find-scope [f]
  (loop [f f
         s #{}]
    (if-some [obj (frame-object f)]
      (recur (lookup-frame-id
               (object-context obj)
               (object-frame obj))
        (into s (keys (get-object-hooks obj)))) s)))

(declare object-walk-frames)
(defn frame-walk-tiers [frame step k cb start]
  (let [tiers (frame-tiers frame)
        size (alength tiers)
        back (bit-shift-right (- 1 step) 1)
        back-inc-size (* back (inc size))
        stop (- size back-inc-size)]
    (loop [i (if (nil? start) (dec back-inc-size) start)]
      (let [i (+ i step)]
        (when-not (== i stop)
          (if-some [x (let [obj (aget tiers i)]
                        (if-some [v (get (get-object-hooks obj) k)]
                          (cb v) (object-walk-frames obj step k cb nil)))]
            x (recur i)))))))

(defn object-walk-frames [obj step k cb start]
  (let [buf (get-object-buffer obj)
        size (get-object-size obj)
        back (bit-shift-right (- 1 step) 1)
        back-inc-size (* back (inc size))
        stop (- size back-inc-size)]
    (loop [i (if (nil? start) (dec back-inc-size) start)]
      (let [i (+ i step)]
        (when-not (== i stop)
          (if-some [x (frame-walk-tiers (aget buf i) step k cb nil)]
            x (recur i)))))))

(defn cancel-frame [f]
  (let [procs (frame-procs f)]
    (dotimes [i (alength procs)]
      ((aget procs i)))))

(defn shrink-buffer [obj]
  (let [buf (get-object-buffer obj)
        size (dec (get-object-size obj))
        frame (aget buf (int size))]
    (cancel-frame frame)
    (aset buf (int size) nil)
    (set-object-size obj size)
    (dissoc-frame-id (object-context obj) (frame-id frame))))

(defn notify-rotate [f k]
  (let [anchor (loop [f f]
                 (let [obj (frame-object f)]
                   (when-not (contains? (get-object-hooks obj) k)
                     (if-some [a (object-walk-frames obj 1 k identity
                                   (get-frame-position f))]
                       a (recur (lookup-frame-id
                                  (object-context obj)
                                  (object-frame obj)))))))]
    (frame-walk-tiers f 1 k (fn [target] (k target anchor)) nil) f))

(defn frame-rotate [f to]
  (let [obj (frame-object f)
        from (get-frame-position f)
        step (compare to from)]
    (when-not (zero? step)
      (let [buf (get-object-buffer obj)]
        (set-frame-position f to)
        (loop [i from]
          (let [j (+ i step)
                y (aget buf (int j))]
            (set-frame-position y i)
            (aset buf (int i) y)
            (if (== to j)
              (aset buf (int to) f)
              (recur j)))))
      (reduce notify-rotate f (find-scope f)))))

(defn decode-inst [ctx inst]
  (if-some [id (get-frame-register ctx)]
    (do (set-frame-register ctx nil)
        (when-some [f (lookup-frame-id ctx id)]
          (if (neg? inst)
            (frame-rotate f (+ (get-object-size (frame-object f)) inst)) ; frame object returns the parent
            (let [x (aget (frame-slots f) (int inst))]
              (if (instance? Atom x)
                (reset! x (pop-queue ctx))
                (if (ifn? x)
                  (set-target-register ctx x)
                  (if-some [init (get-target-register ctx)]
                    (do (init x (next-remote ctx)) ; next-remote assigns a new id for the new frame we are creating
                        (set-target-register ctx nil))
                    (shrink-buffer x))))))))
    (set-frame-register ctx (- inst))) ctx)

(defn signal [<x]
  (m/signal! (lift-cancelled <x)))

(defn acopy [dest src size]
  #?(:clj (System/arraycopy src 0 dest 0 size))
  #?(:cljs (dotimes [i size] (aset dest i (aget src i)))))

(defn doto-aset [^objects arr k v]
  (doto arr (aset (int k) v)))

(defn frame [proc-count slot-count tier-count ctor]
  (fn [obj frame-id]
    (let [pos (get-object-size obj)
          buf (get-object-buffer obj)
          cap (alength buf)
          buf (if (< pos cap)
                buf (set-object-buffer obj
                      (doto (object-array (bit-shift-left cap 1))
                        (acopy buf cap))))
          vars (object-vars obj)
          extra (get-object-procs obj)
          procs (object-array (+ proc-count (count extra)))
          slots (object-array slot-count)
          tiers (object-array tier-count)
          prevs (reduce-kv
                  (fn [m v <x]
                    (let [prev (aget vars (int v))
                          proc (m/signal! (lift-cancelled <x))]
                      (aset procs (+ proc-count (count m)) proc)
                      (aset vars (int v) proc)
                      (assoc m v prev)))
                  {} extra)]
      (set-object-size obj (inc pos))
      (assoc-frame-id (object-context obj) frame-id
        (aset buf pos (make-frame obj pos frame-id procs slots tiers)))
      (try (ctor vars procs slots tiers frame-id)
           (finally (reduce-kv doto-aset vars prevs))))))

;; Takes an instruction identifying a target and a frame-constructor.
;; Return a flow instantiating the frame.
(defn thunk [target-id target-slot ctor]
  (fn [n t]
    (let [obj (doto (l/get-local this)
                (assert "Unable to call thunk : not an object."))
          ctx (object-context obj)
          id (next-local ctx)]
      (remote ctx target-id target-slot) ; notify remote peer of frame creation
      (remote ctx (object-frame obj) (object-slot obj)) ; notify remote peer of frame mount point (on which object we want to create this frame, its parent)
      (try ((ctor obj id)
            #(let [p (l/get-local this)]
               (l/set-local this obj)                       ;; TODO understand why we need that
               (n) (l/set-local this p))
            #(let [f (lookup-frame-id ctx id)]
               (frame-rotate f (dec (get-object-size obj)))
               (remote ctx (frame-id f) -1)
               (remote ctx (object-frame obj) (object-slot obj))
               (shrink-buffer obj) (t)))
           (catch #?(:clj Throwable :cljs :default) e
             (failer e n t))))))

(defn move
  "Move or delete a frame in the current object. Will search and call `hook`.
   - Arity 1 deletes,
   - Arity 2 moves." 
  ([from]
   (when-some [obj (l/get-local this)]
     (cancel-frame (aget (get-object-buffer obj) (int from)))))
  ([from to]
   (when-some [obj (l/get-local this)]
     (let [f (aget (get-object-buffer obj) (int from))]
       (frame-rotate f to)
       (remote (object-context obj)
         (frame-id f) (- to (get-object-size obj)))))))

(defn switch [obj <<x]
  (m/signal!
    (bind-flow this obj
      (m/cp (try (let [<x (m/?< <<x)]
                   (if (failure <x)
                     <x (m/?< <x)))
                 (catch Cancelled e
                   (Failure. e)))))))

(defn hook [k v <x]
  (assert (some? v) "hook value must be non-nil.")
  (fn [n t]
    (if-some [obj (l/get-local this)]
      (do
        (loop [obj obj]
          (let [f (lookup-frame-id (object-context obj) (object-frame obj))]
            (if-some [a (frame-walk-tiers f 1 k identity (object-tier obj))]
              (k v a)
              (when-some [obj (frame-object f)]
                (if (contains? (get-object-hooks obj) k)
                  (k v nil)
                  (recur obj))))))
        (set-object-hooks obj (assoc (get-object-hooks obj) k v))
        (<x n #(do (set-object-hooks obj (dissoc (get-object-hooks obj) k))
                   (k v) (t))))
      (failer (error "Unable to hook : not an object.") n t))))

(defn capture [v]
  (fn [<x <y]
    (fn [n t]
      (if-some [obj (l/get-local this)]
        (let [procs (get-object-procs obj)]
          (set-object-procs obj (assoc procs v <y))
          (try (<x n t) (finally (set-object-procs obj procs))))
        (failer (ex-info "Unable to bind : not an object." {}) n t)))))

(def unbound (steady (Failure. (error "Unbound var."))))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (js/console.error e)))

(defn merge-events [x y]
  (conj (reduce conj (pop x) (pop y))
    (reduce-kv assoc (peek x) (peek y))))

(defn crash-failure [x]
  (if (instance? Failure x)
    (throw (.-error ^Failure x)) x))

(defn peer [var-count proc-count slot-count tier-count ctor]
  (fn [write ?read]
    (m/reactor
      (let [procs (object-array proc-count)
            slots (object-array slot-count)
            tiers (object-array tier-count)
            context (make-context (make-frame nil 0 0 procs slots tiers))
            >msg (->> (m/ap ; FIXME outputs are sampled too fast, should be in sync with the websocket. Should sample everytime we are done writing on the socket.
                        (let [path (m/?> ##Inf (m/observe
                                                 (fn [!]
                                                   (set-remote context !)
                                                   #(set-remote context nil))))
                              <out (peek path)]
                          (case <out nil path [{(pop path) (m/?> <out)}])))
                   (m/relieve merge-events)
                   (m/stream!))]
        (m/stream!
          (m/ap (let [msg (m/?> >msg)
                      out (peek msg)]
                  (-> []
                    (into (vals out))
                    (doto (->> (prn '>)))
                    (conj (into (pop msg) cat (keys out)))
                    (write) (m/?))
                  (prn :encode-ok))))
        (m/stream! (m/latest crash-failure (ctor context (object-array (repeat var-count unbound)) procs slots tiers)))
        (->> (m/ap (m/? (m/?> (m/seed (repeat ?read)))))
          (m/stream!)
          (m/eduction
            (map (fn [msg]
                   (prn '< (pop msg))
                   (try
                     (reduce decode-inst
                       (set-queue context (pop msg))
                       (peek msg))
                     (catch #?(:clj Throwable :cljs :default) e
                       (pst e)))
                   (prn :decode-ok))))
          (m/stream!))
        (prn :booted)))))

(defn collapse [s n f & args]
  (->> (iterate pop s)
    (transduce (comp (map peek) (take n)) conj args)
    (apply f)
    (conj (nth (iterate pop s) n))))

(defn snapshot [env f & args]
  (update env :stack conj (apply f env args)))

(def empty-frame
  {:tiers  0 ; count of variables and sources
   :procs  0 ; count of disposable objects (processes)
   :slots  0 ; count of inputs, source and targets (exposed by frames)
   :remote 0 ; count of outputs, constants (p/fn), and variables (new)
   })

;; TODO move me
;; `new` creates a local variable and a remote source
;; `p/fn` creates a local constant and a remote target
;; Same duality with input and output, if there is 3 inputs locally, there is 3 outputs remotely.
;; There is no instruction to create inputs and outputs, they are infered from unquote-splicing.

(defn compile [inst {:keys [nop sub pub capture vget bind invoke input output
                            global literal variable source constant target main] :as fns}]
  (-> ((fn walk [env idx [op & args]]
         (case op
           :nop (update env :stack collapse 0 nop)
           :sub (let [[s] args]
                  (-> env
                    (update :stack collapse 0 sub (- idx s))))
           :pub (let [[p c] args]
                  (-> env
                    (walk idx p)
                    (snapshot :procs)
                    (update :procs inc)
                    (walk (inc idx) c)
                    (update :stack collapse 3 pub idx)))
           :def (let [[v] args]
                  (-> env
                    (update :vars max v)
                    (update :stack conj (capture v))))
           :eval (let [[f] args]
                   (update env :stack conj ((:eval fns) f))) ; can't shadow eval in advanced CLJS compilation
           :node (let [[v] args]
                   (-> env
                     (update :vars max v)
                     (update :stack conj (vget v))))
           :bind (let [[v s inst] args]
                   (-> env
                     (update :vars max v)
                     (walk idx inst)
                     (update :stack collapse 1 bind v (- idx s))))
           :apply (-> (reduce (fn [env arg] (walk env idx arg)) env args)
                    (update :stack collapse (count args) invoke))
           :input (-> env
                    (snapshot :slots)
                    (update :slots inc)
                    (snapshot :procs)
                    (update :procs inc)
                    (update :stack collapse 2 input))
           :output (let [[inst cont] args]
                     (-> env
                       (walk idx inst)
                       (snapshot :remote)
                       (update :remote inc)
                       (snapshot :procs)
                       (update :procs inc)
                       (walk idx cont)
                       (update :stack collapse 4 output)))
           :global (let [[x] args]
                     (update env :stack conj (global x)))
           :literal (let [[x] args]
                      (update env :stack conj (literal x)))
           :variable (let [[inst] args]
                       (-> env
                         (walk idx inst)
                         (snapshot :procs)
                         (update :procs inc)
                         (snapshot :tiers)
                         (update :tiers inc)
                         (snapshot :remote)
                         (update :remote inc)
                         (update :stack collapse 4 variable)))
           :source (let [[cont] args]
                     (-> env
                       (snapshot :slots)
                       (update :slots inc)
                       (snapshot :tiers)
                       (update :tiers inc)
                       (walk idx cont)
                       (update :stack collapse 3 source)))
           :constant (let [[inst] args]
                       (-> env
                         (merge empty-frame)
                         (walk idx inst)
                         (snapshot :procs)
                         (snapshot :slots)
                         (snapshot :tiers)
                         (merge (select-keys env (keys empty-frame)))
                         (snapshot :remote)
                         (update :remote inc)
                         (update :stack collapse 5 constant)))
           :target (let [[inst cont] args]
                     (-> env
                       (merge empty-frame)
                       (walk idx inst)
                       (snapshot :procs)
                       (snapshot :slots)
                       (snapshot :tiers)
                       (merge (select-keys env (keys empty-frame)))
                       (snapshot :slots)
                       (update :slots inc)
                       (walk idx cont)
                       (update :stack collapse 6 target)))))
       (assoc empty-frame :vars -1) 0 inst)
    (snapshot :procs)
    (snapshot :slots)
    (snapshot :tiers)
    (snapshot :vars)
    (:stack)
    (collapse 5 main)
    (peek)))

(defn sym [& args]
  (symbol (str/join "-" args)))

(defn emit [prefix inst]
  (compile inst
    {:nop      (fn [])
     :sub      (fn [s] (sym prefix 'pub s))
     :pub      (fn [form proc cont idx]
                 `(let [~(sym prefix 'pub idx)
                        (aset ~(sym prefix 'procs)
                          (int ~proc) (signal ~form))] ~cont))
     :eval     (fn [f] `(steady ~f))
     :vget     (fn [v] `(aget ~(sym prefix 'vars) (int ~v)))
     :bind     (fn [c v s]
                 `(let [~(sym prefix 'prev) (aget ~(sym prefix 'vars) ~v)]
                    (aset ~(sym prefix 'vars) (int ~v) ~(sym prefix 'pub s))
                    (let [~(sym prefix 'res) ~c]
                      (aset ~(sym prefix 'vars) (int ~v) ~(sym prefix 'prev))
                      ~(sym prefix 'res))))
     :invoke   (fn [& forms] `(latest-apply ~@forms))
     :input    (fn [slot proc]
                 `(aset ~(sym prefix 'procs) (int ~proc)
                    (signal (m/watch (aset ~(sym prefix 'slots) (int ~slot) (atom pending))))))
     :output   (fn [form slot proc cont]
                 `(do (remote ~(sym prefix 'ctx) ~(sym prefix 'id) ~slot
                        (aset ~(sym prefix 'procs) (int ~proc)
                          (signal ~form))) ~cont))
     :global   (fn [x] `(steady ~(symbol x)))
     :literal  (fn [x] `(steady (quote ~x)))
     :capture  (fn [v] `(steady (capture ~v)))
     :variable (fn [form proc tier slot]
                 `(aset ~(sym prefix 'procs) (int ~proc)
                    (switch (aset ~(sym prefix 'tiers) (int ~tier)
                              (make-object ~(sym prefix 'ctx) ~(sym prefix 'id) ~tier ~slot
                                ~(sym prefix 'vars))) ~form)))
     :source   (fn [slot tier cont]
                 `(do (aset ~(sym prefix 'slots) (int ~slot)
                        (aset ~(sym prefix 'tiers) (int ~tier)
                          (make-object ~(sym prefix 'ctx) ~(sym prefix 'id) ~tier nil
                            ~(sym prefix 'vars)))) ~cont))
     :constant (fn [form proc-count slot-count tier-count slot]
                 `(steady
                    (thunk ~(sym prefix 'id) ~slot
                      (frame ~proc-count ~slot-count ~tier-count
                        (fn [~(sym prefix 'vars)
                             ~(sym prefix 'procs)
                             ~(sym prefix 'slots)
                             ~(sym prefix 'tiers)
                             ~(sym prefix 'id)] ~form)))))
     :target   (fn [form proc-count slot-count tier-count slot cont]
                 `(do (aset ~(sym prefix 'slots) (int ~slot)
                        (frame ~proc-count ~slot-count ~tier-count
                          (fn [~(sym prefix 'vars)
                               ~(sym prefix 'procs)
                               ~(sym prefix 'slots)
                               ~(sym prefix 'tiers)
                               ~(sym prefix 'id)]
                            ~form))) ~cont))
     :main     (fn [form proc-count slot-count tier-count vars]
                 `(peer ~(inc vars) ~proc-count ~slot-count ~tier-count
                    (fn [~(sym prefix 'ctx)
                         ~(sym prefix 'vars)
                         ~(sym prefix 'procs)
                         ~(sym prefix 'slots)
                         ~(sym prefix 'tiers)]
                      (let [~(sym prefix 'id) 0]
                        ~form))))}))

(tests
  (emit nil [:literal 5]) :=
  `(peer 0 0 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0] (steady '5))))

  (emit nil [:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]) :=
  `(peer 0 0 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (latest-apply (steady ~'clojure.core/+) (steady '2) (steady '3)))))

  (emit nil
    [:pub [:literal 1]
     [:apply [:global :clojure.core/+]
      [:sub 1] [:literal 2]]]) :=
  `(peer 0 1 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (let [~'-pub-0 (aset ~'-procs (int 0) (signal (steady '1)))]
           (latest-apply (steady ~'clojure.core/+) ~'-pub-0 (steady '2))))))

  (emit nil
    [:variable [:global :missionary.core/none]]) :=
  `(peer 0 1 0 1
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (aset ~'-procs (int 0)
           (switch (aset ~'-tiers (int 0) (make-object ~'-ctx ~'-id 0 0 ~'-vars))
             (steady m/none))))))

  (emit nil [:input]) :=
  `(peer 0 1 1 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (aset ~'-procs (int 0)
           (signal (m/watch (aset ~'-slots (int 0) (atom pending))))))))

  (emit nil [:constant [:literal :foo]]) :=
  `(peer 0 0 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (steady
           (thunk ~'-id 0
             (frame 0 0 0
               (fn [~'-vars ~'-procs ~'-slots ~'-tiers ~'-id]
                 (steady ':foo))))))))

  (emit nil
    [:variable
     [:pub [:constant [:literal 3]]
      [:pub [:constant [:input]]
       [:apply [:apply [:global :clojure.core/hash-map]
                [:literal 2] [:sub 2]
                [:literal 4] [:sub 1]
                [:literal 5] [:sub 1]]
        [:literal 1]
        [:constant [:literal 7]]]]]]) :=
  `(peer 0 3 0 1
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (aset ~'-procs (int 2)
           (switch (aset ~'-tiers (int 0)
                     (make-object ~'-ctx ~'-id 0 3 ~'-vars))
             (let [~'-pub-0 (aset ~'-procs (int 0)
                              (signal
                                (steady
                                  (thunk ~'-id 0
                                    (frame 0 0 0
                                      (fn [~'-vars ~'-procs ~'-slots ~'-tiers ~'-id]
                                        (steady '3)))))))]
               (let [~'-pub-1 (aset ~'-procs (int 1)
                                (signal
                                  (steady
                                    (thunk ~'-id 1
                                      (frame 1 1 0
                                        (fn [~'-vars ~'-procs ~'-slots ~'-tiers ~'-id]
                                          (aset ~'-procs (int 0)
                                            (signal (m/watch (aset ~'-slots (int 0) (atom pending)))))))))))]
                 (latest-apply
                   (latest-apply (steady ~'clojure.core/hash-map)
                     (steady '2) ~'-pub-0
                     (steady '4) ~'-pub-1
                     (steady '5) ~'-pub-1)
                   (steady '1)
                   (steady
                     (thunk ~'-id 2
                       (frame 0 0 0
                         (fn [~'-vars ~'-procs ~'-slots ~'-tiers ~'-id]
                           (steady '7)))))))))))))

  (emit nil [:def 0]) :=
  `(peer 1 0 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (steady (capture 0)))))

  (emit nil
    [:pub [:literal nil]
     [:constant [:sub 1]]]) :=
  `(peer 0 1 0 0
     (fn [~'-ctx ~'-vars ~'-procs ~'-slots ~'-tiers]
       (let [~'-id 0]
         (let [~'-pub-0 (aset ~'-procs (int 0)
                          (signal (steady 'nil)))]
           (steady
             (thunk ~'-id 0
               (frame 0 0 0
                 (fn [~'-vars ~'-procs ~'-slots ~'-tiers ~'-id]
                   ~'-pub-0)))))))))

(defn juxt-with ;;juxt = juxt-with vector, juxt-with f & gs = apply f (apply juxt gs)
  ([f]
   (fn
     ([] (f))
     ([a] (f))
     ([a b] (f))
     ([a b c] (f))
     ([a b c & ds] (f))))
  ([f g]
   (fn
     ([] (f (g)))
     ([a] (f (g a)))
     ([a b] (f (g a b)))
     ([a b c] (f (g a b c)))
     ([a b c & ds] (f (apply g a b c ds)))))
  ([f g h]
   (fn
     ([] (f (g) (h)))
     ([a] (f (g a) (h a)))
     ([a b] (f (g a b) (h a b)))
     ([a b c] (f (g a b c) (h a b c)))
     ([a b c & ds] (f (apply g a b c ds) (apply h a b c ds)))))
  ([f g h i]
   (fn
     ([] (f (g) (h) (i)))
     ([a] (f (g a) (h a) (i a)))
     ([a b] (f (g a b) (h a b) (i a b)))
     ([a b c] (f (g a b c) (h a b c) (i a b c)))
     ([a b c & ds] (f (apply g a b c ds) (apply h a b c ds) (apply i a b c ds)))))
  ([f g h i & js]
   (fn
     ([] (apply f (g) (h) (i) (map #(%) js)))
     ([a] (apply f (g a) (h a) (i a) (map #(% a) js)))
     ([a b] (apply f (g a b) (h a b) (i a b) (map #(% a b) js)))
     ([a b c] (apply f (g a b c) (h a b c) (i a b c) (map #(% a b c) js)))
     ([a b c & ds] (apply f (apply g a b c ds) (apply h a b c ds) (apply i a b c ds) (map #(apply % a b c ds) js))))))

(defn globals [program]
  (->> (tree-seq coll? seq program)
       (eduction (comp (filter vector?)
                       (filter (fn [[a _]] (= :global a)))
                       (map second)
                       (distinct)))
       (sort-by (juxt namespace name))))

(defn missing-exports [resolvef program]
  (->> (globals program)
    (eduction (map (juxt (partial resolvef ::not-found) identity))
      (filter #(= ::not-found (first %)))
      (map second)
      (map symbol))))

(defn dynamic-resolve [nf x]
  #?(:clj (try (clojure.core/eval (symbol x))
               (catch clojure.lang.Compiler$CompilerException _ nf))
     :cljs nf))

(defn eval
  ([inst] (eval dynamic-resolve inst))
  ([resolvef inst]
   (compile inst
     {:nop      (fn [] (constantly nil))
      :sub      (fn [s]
                  (fn [pubs ctx vars procs slots tiers id]
                    (nth pubs s)))
      :pub      (fn [form proc cont _]
                  (fn [pubs ctx vars ^objects procs slots tiers id]
                    (cont (conj pubs (aset procs (int proc)
                                       (signal (form pubs ctx vars procs slots tiers id))))
                      ctx vars procs slots tiers id)))
      :vget     (fn [v]
                  (fn [pubs ctx ^objects vars procs slots tiers id]
                    (aget vars (int v))))
      :bind     (fn [c v s]
                  (fn [pubs ctx ^objects vars procs slots tiers id]
                    (let [prev (aget vars (int v))]
                      (aset vars (int v) (nth pubs s))
                      (let [res (c pubs ctx vars procs slots tiers id)]
                        (aset vars (int v) prev)
                        res))))
      :invoke   (fn [& forms] (apply juxt-with latest-apply forms))
      :input    (fn [slot proc]
                  (fn [pubs ctx vars ^objects procs ^objects slots tiers id]
                    (aset procs (int proc)
                      (signal (m/watch (aset slots (int slot) (atom (Failure. (Pending.)))))))))
      :output   (fn [form slot proc cont]
                  (fn [pubs ctx vars ^objects procs slots tiers id]
                    (remote ctx id slot
                      (aset procs (int proc)
                        (signal (form pubs ctx vars procs slots tiers id))))
                    (cont pubs ctx vars procs slots tiers id)))
      :global   (fn [x]
                  (let [r (resolvef ::not-found x)]
                    (case r
                      ::not-found (throw (ex-info (str "Unable to resolve - " (symbol x)) {}))
                      (constantly (steady r)))))
      :literal  (fn [x] (constantly (steady x)))
      :capture  (fn [v] (constantly (steady (capture v))))
      :variable (fn [form proc tier slot]
                  (fn [pubs ctx vars ^objects procs slots ^objects tiers id]
                    (aset procs (int proc)
                      (switch (aset tiers (int tier)
                                (make-object ctx id tier slot vars))
                        (form pubs ctx vars procs slots tiers id)))))
      :source   (fn [slot tier cont]
                  (fn [pubs ctx vars procs ^objects slots ^objects tiers id]
                    (aset slots (int slot)
                      (aset tiers (int tier)
                        (make-object ctx id tier nil vars)))
                    (cont pubs ctx vars procs slots tiers id)))
      :constant (fn [form proc-count slot-count tier-count slot]
                  (fn [pubs ctx vars procs slots tiers id]
                    (steady
                      (thunk id slot
                        (frame proc-count slot-count tier-count
                          (partial form pubs ctx))))))
      :target   (fn [form proc-count slot-count tier-count slot cont]
                  (fn [pubs ctx vars procs ^objects slots tiers id]
                    (aset slots (int slot)
                      (frame proc-count slot-count tier-count
                        (partial form pubs ctx)))
                    (cont pubs ctx vars procs slots tiers id)))
      :main     (fn [form proc-count slot-count tier-count max-var-idx]
                  (peer (inc max-var-idx) proc-count slot-count tier-count
                    (fn [ctx vars procs slots tiers]
                      (form [] ctx vars procs slots tiers 0))))})))
