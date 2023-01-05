(ns ^:no-doc hyperfiddle.photon-impl.runtime
  (:refer-clojure :exclude [eval compile])
  (:require [hyperfiddle.photon-impl.yield :refer [yield]]
            [hyperfiddle.photon-impl.gather :refer [gather]]
            [hyperfiddle.photon-impl.failer :as failer]
            [hyperfiddle.photon-impl.eventually :refer [eventually]]
            [hyperfiddle.photon-impl.local :as l]
            [hyperfiddle.photon-impl.ir :as-alias ir]
            [hyperfiddle.photon.debug :as dbg]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str]
            [contrib.data :as data])
  (:import missionary.Cancelled
           (hyperfiddle.photon Failure Pending Remote)
           #?(:clj (clojure.lang IFn IDeref Atom))))

;; A photon program is a tree, which structure is dynamically maintained.
;; Two peers are synchronized (through a protocol) such that the tree structure is identicaly on both peers.
;; Two type of nodes:
;; [Frames] : A piece of DAG with a static structure. Won't be rearanged at runtime. (AKA Static Frame)
;;            - A set of compiled s-expressions + a set of signals weaving these expressions + N inputs + N outputs
;;            - A frame has 2 instances, one on client, one on server.
;;            - Server's outputs are client's inputs and vice-versa.
;;            - Frames are processes.
;;            - Image: a stackframe but for a DAG. A stackframe is allocated to compute the result of a function.
;;              It is volatile (disposable) in a stack-based program. Since photon is reactive, the frame is not disposable.
;;              «ReactiveFrame» «Distributed Reactive Frame»
;; [Tiers] : For each `new` in a frame, a managed process is created. Tiers are child processes of frames. (AKA Dynamic Frame)
;;             - Parent process of a tier is always a frame.
;;             - Parent process of a frame is alawys a tier.
;;             - Specificity: Frames have a fixed set of children, tiers have a dynamic set of children, they can spawn new frames anytime.
;;                            Child frames of a tier are positioned (there is a well defined traversal order)
;;                            Node order (positions) can change at runtime because tiers can spawn dynamically (e.g.: p/for).
;;             - Some tiers don't have child frames : e.g. (new (m/watch .)), no child frames, no input, no output
;;             - Some frames don't have child tiers : e.g. a frame without any `new`, no variability.
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
;; 2. Where (address) do we want the instance of p/fn to be instantiated (which tier should get a new child frame )


;; # network protocol
;; After boot, both peers are allowed to send messages to each other at any point in time without coordination.
;; A message is a vector of arbitrary values (the data frames) ending with a vector of integers (the control frame).
;; Each peer must process messages sequentially. Decoding a message requires a queue (input values, initially 
;; empty) and two registers (the current frame and the current target, initially undefined).
;; 1. push data frame values to the queue
;; 2. process control frame integers as a sequence of instructions (for each):
;;   * if current frame is undefined :
;;     1. lookup frame identified by instruction :
;;       * zero identifies the root frame
;;       * strictly positive numbers identify frames created by local sources, by birth index
;;       * strictly negative numbers identify frames created by local variables, by negation of birth index
;;     2. define it as current frame
;;   * if current frame is defined :
;;     * if instruction (argument) is strictly negative :
;;       1. move the current frame to position (+ instruction size-of-source), where
;;          - instruction is the argument
;;          - size-of-source: count of frame's siblings + 1 (how many frame children does the parent tier have)
;;          if target position is the same as current, cancel the frame.
;;       2. undefine current frame
;;     * if instruction is positive or zero :
;;       1. lookup the current frame slot identified by instruction (argument)
;;         * if slot is an input :
;;           * if queue is non empty, pop a value and assign this value to the input
;;           * if queue is empty, terminate the input
;;         * if slot is a source :
;;           1. construct a new frame from this target and insert it at the last child of source (a tier)
;;           2. undefine current target
;;         * if slot is a target : define it as current target
;;         * otherwise, remove frame from index
;;       2. undefine current frame

(defn fail [x] (throw x))

(def failure (some-fn #(when (instance? Failure %) %)))

(def pending (Failure. (Pending.)))

(defn error [^String msg] ; Could be ex-info (ExceptionInfo inherits Error or js/Error)
  (#?(:clj Error. :cljs js/Error.) msg))

(defn select-debug-info [debug-info]
  (merge (select-keys debug-info [::ir/op]) (data/select-ns :hyperfiddle.photon.debug debug-info)))

(defn latest-apply [debug-info & args]
  (apply m/latest
    (fn [f & args]
      (if-let [err (apply failure f args)]
        (dbg/error (assoc (select-debug-info debug-info) ::dbg/args args) err)
        (try (apply f args)
             (catch #?(:clj Throwable :cljs :default) e
               (dbg/error (assoc (select-debug-info debug-info) ::dbg/args args) (Failure. e))))))
    args))

(def latest-first
  (partial m/latest
    (fn [x y] (if (instance? Failure y) y x))))

(defn pure [x] (m/cp x))

(def lift-cancelled
  (partial comp
    (fn [it]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        IDeref
        (#?(:clj deref :cljs -deref) [_]
          (try @it (catch Cancelled e (Failure. e))))))))

(defn signal [<x]
  (m/signal! (lift-cancelled <x)))

(def this (l/local))

(def context-slot-root            (int 0))  ;; Immutable. The root frame.
(def context-slot-local-id        (int 1))  ;; The next local id (auto incremented).
(def context-slot-remote-id       (int 2))  ;; The next remote id (auto decremented).
(def context-slot-frame-store     (int 3))  ;; A transient map associating frame ids to frame objects.
(def context-slot-remote          (int 4))  ;; The remote callback
(def context-slot-output          (int 5))  ;; The output callback
(def context-slot-incoming        (int 6))  ;; The incoming callback
(def context-slot-queue           (int 7)) ;; The payloads assigned to inputs (used by decoder).
(def context-slot-frame-register  (int 8)) ;; The frame register (used by decoder).
(def context-slot-target-register (int 9)) ;; The target register (used by decoder).
(def context-slots                (int 10))

(def tier-slot-parent   (int 0))    ;; Immutable. The parent frame.
(def tier-slot-position (int 1))    ;; Immutable. The static position of the tier in the parent frame.
(def tier-slot-buffer   (int 2))    ;; Buffer of array list of child frames.
(def tier-slot-size     (int 3))    ;; Size of array list of child frames.
(def tier-slot-foreigns (int 4))    ;; Foreign flow map
(def tier-slot-hooks    (int 5))    ;; Hooks
(def tier-slot-vars     (int 6))    ;; A snapshot of the dynamic environment.
(def tier-slot-remote   (int 7))    ;; If local, the slot of the remote part.
(def tier-slots         (int 8))

(def frame-slot-context   (int 0))  ;; Immutable. The global context.
(def frame-slot-parent    (int 1))  ;; Immutable. The parent tier, nil iff root frame.
(def frame-slot-id        (int 2))  ;; Immutable. Zero if root frame, a strictly positive number if the frame was created by a variable, a strictly negative number if the frame was created by a source.
(def frame-slot-position  (int 3))  ;; The index of the frame among its siblings.
(def frame-slot-foreign   (int 4))  ;; Immutable
(def frame-slot-static    (int 5))  ;; Immutable
(def frame-slot-dynamic   (int 6))  ;; Immutable
(def frame-slot-variables (int 7))  ;; Immutable
(def frame-slot-sources   (int 8))  ;; Immutable
(def frame-slot-targets   (int 9))  ;; Immutable
(def frame-slot-inputs    (int 10)) ;; Immutable
(def frame-slot-tiers     (int 11)) ;; Immutable
(def frame-slot-constants (int 12)) ;; Immutable
(def frame-slot-outputs   (int 13)) ;; Immutable
(def frame-slots          (int 14))

(defn aswap
  ([^objects arr slot f]
   (aset arr slot (f (aget arr slot))))
  ([^objects arr slot f a]
   (aset arr slot (f (aget arr slot) a)))
  ([^objects arr slot f a b]
   (aset arr slot (f (aget arr slot) a b)))
  ([^objects arr slot f a b c]
   (aset arr slot (f (aget arr slot) a b c)))
  ([^objects arr slot f a b c & ds]
   (aset arr slot (apply f (aget arr slot) a b c ds))))

(defn doto-aset [^objects arr k v]
  (doto arr (aset (int k) v)))

(defn make-context ^objects []
  (doto (object-array context-slots)
    (aset context-slot-local-id (identity 0))
    (aset context-slot-remote-id (identity 0))
    (aset context-slot-frame-store (transient {}))))

(defn make-tier [^objects parent position]
  (aset ^objects (aget parent frame-slot-tiers) (int position)
    (doto (object-array tier-slots)
      (aset tier-slot-parent parent)
      (aset tier-slot-position position)
      (aset tier-slot-buffer (object-array 8))
      (aset tier-slot-size (identity (int 0)))
      (aset tier-slot-foreigns {})
      (aset tier-slot-hooks {}))))

(defn make-frame [^objects context parent id position
                  foreign static dynamic variable-count source-count
                  constant-count target-count output-count input-count
                  ^objects buffer ^objects vars boot]
  (let [tier-count (+ variable-count source-count)
        frame (doto (object-array frame-slots)
                (aset frame-slot-context context)
                (aset frame-slot-parent parent)
                (aset frame-slot-id id)
                (aset frame-slot-position position)
                (aset frame-slot-foreign (object-array (count foreign)))
                (aset frame-slot-static (object-array (count static)))
                (aset frame-slot-dynamic (object-array (count dynamic)))
                (aset frame-slot-variables (object-array variable-count))
                (aset frame-slot-sources (object-array source-count))
                (aset frame-slot-targets (object-array target-count))
                (aset frame-slot-inputs (object-array input-count))
                (aset frame-slot-tiers (object-array tier-count))
                (aset frame-slot-constants (object-array constant-count))
                (aset frame-slot-outputs (object-array output-count)))]
    (dotimes [i tier-count] (make-tier frame i))
    (aset buffer (int position) frame)
    (aswap context context-slot-frame-store assoc! id frame)
    (let [prevs (reduce-kv
                  (fn [m v <x]
                    (let [prev (aget vars (int v))
                          proc (signal <x)]
                      (aset ^objects (aget frame frame-slot-foreign) (count m) proc)
                      (aset vars (int v) proc)
                      (assoc m v prev)))
                  {} foreign)]
      (reduce-kv (fn [^objects arr i <x]
                   (aset arr (int i) (signal <x)) arr)
        (aget frame frame-slot-static) static)
      (reduce-kv (fn [^objects arr i v]
                   (aset arr (int i) (signal (aget vars (int v)))) arr)
        (aget frame frame-slot-dynamic) dynamic)
      (let [result (boot frame vars)]
        (reduce-kv doto-aset vars prevs)
        (when-some [callback (aget context context-slot-output)]
          (callback #{frame}))
        result))))

(def input-slot-notifier (int 0))
(def input-slot-terminator (int 1))
(def input-slot-current (int 2))
(def input-slots (int 3))

(deftype InputIterator [^objects i]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (when-not (nil? (aget i input-slot-terminator))
      (when-some [n (aget i input-slot-notifier)]
        (aset i input-slot-notifier nil)
        (let [y (aget i input-slot-current)]
          (aset i input-slot-current (Failure. (Cancelled.)))
          (when (identical? i y) (n))))))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (let [x (aget i input-slot-current)]
      (aset i input-slot-current i)
      (when (nil? (aget i input-slot-notifier))
        (when-some [t (aget i input-slot-terminator)]
          (aset i input-slot-terminator nil) (t))) x)))

(defn input [^objects frame slot]
  (fn [n t]
    (n) (->InputIterator
          (aset ^objects (aget frame frame-slot-inputs) (int slot)
            (doto (object-array input-slots)
              (aset input-slot-notifier n)
              (aset input-slot-terminator t)
              (aset input-slot-current pending))))))

(defn input-change [^objects i x]
  (when-some [n (aget i input-slot-notifier)]
    (let [y (aget i input-slot-current)]
      (aset i input-slot-current x)
      (when (identical? i y) (n)))))

(defn input-close [^objects i]
  (when-not (nil? (aget i input-slot-notifier))
    (aset i input-slot-notifier nil)
    (when (identical? i (aget i input-slot-current))
      (when-some [t (aget i input-slot-terminator)]
        (aset i input-slot-terminator nil) (t)))))

(defn remote [^objects frame slot]
  (when-some [callback (aget ^objects (aget frame frame-slot-context) context-slot-remote)]
    (callback [(aget frame frame-slot-id) slot])))

(defn check-unbound-var [debug-info <x]
  (m/latest (fn [x]
              (if (= ::unbound x)
                (Failure. (error (str "Unbound var `" (::dbg/name debug-info) "`")))
                x)) <x))

(defn check-failure [debug-info <x]
  (m/latest (fn [x]
              (if (instance? Failure x)
                (dbg/error (select-debug-info debug-info) x)
                x)) <x))

(defn output [^objects frame slot <x debug-info]
  (aset ^objects (aget frame frame-slot-outputs) slot
    (check-failure debug-info (signal <x))))

(defn static [^objects frame slot]
  (aget ^objects (aget frame frame-slot-static) (int slot)))

(defn dynamic [^objects frame slot debug-info]
  (check-unbound-var debug-info (aget ^objects (aget frame frame-slot-dynamic) (int slot))))

(defn tree
  "A snapshot of the tree below given frame, as nested vectors. Frame vectors start with their id."
  [^objects f]
  (let [^objects tiers (aget f frame-slot-tiers)]
    (loop [v []
           i (int 0)]
      (if (== i (alength tiers))
        {:id (aget f frame-slot-id)
         :inputs (alength ^objects (aget f frame-slot-inputs))
         :targets (alength ^objects (aget f frame-slot-targets))
         :sources (alength ^objects (aget f frame-slot-sources))
         :tiers v}
        (recur
          (conj v
            (let [^objects tier (aget tiers i)
                  ^objects buf (aget tier tier-slot-buffer)]
              (loop [v []
                     i (int 0)]
                (if (== i (aget tier tier-slot-size))
                  v (recur (conj v (tree (aget buf i)))
                      (inc i))))))
          (inc i))))))

(defn find-scope [f]
  (loop [f f
         s #{}]
    (if-some [^objects tier (aget ^objects f frame-slot-parent)]
      (recur (aget tier tier-slot-parent)
        (into s (keys (aget tier tier-slot-hooks)))) s)))

(declare tier-walk-frames)
(defn frame-walk-tiers [^objects frame step k cb start]
  (let [^objects tiers (aget frame frame-slot-tiers)
        size (alength tiers)
        back (bit-shift-right (- 1 step) 1)
        back-inc-size (* back (inc size))
        stop (- size back-inc-size)]
    (loop [i (if (nil? start) (dec back-inc-size) start)]
      (let [i (+ i step)]
        (when-not (== i stop)
          (if-some [x (let [^objects tier (aget tiers i)]
                        (if-some [v (get (aget tier tier-slot-hooks) k)]
                          (cb v) (tier-walk-frames tier step k cb nil)))]
            x (recur i)))))))

(defn tier-walk-frames [^objects tier step k cb start]
  (let [^objects buf (aget tier tier-slot-buffer)
        size (aget tier tier-slot-size)
        back (bit-shift-right (- 1 step) 1)
        back-inc-size (* back (inc size))
        stop (- size back-inc-size)]
    (loop [i (if (nil? start) (dec back-inc-size) start)]
      (let [i (+ i step)]
        (when-not (== i stop)
          (if-some [x (frame-walk-tiers (aget buf i) step k cb nil)]
            x (recur i)))))))

(defn notify-rotate [f k]
  (let [anchor (loop [^objects f f]
                 (let [tier ^objects (aget f frame-slot-parent)]
                   (when-not (contains? (aget tier tier-slot-hooks) k)
                     (if-some [a (tier-walk-frames tier 1 k identity
                                   (aget f frame-slot-position))]
                       a (recur (aget tier tier-slot-parent))))))]
    (frame-walk-tiers f 1 k (fn [target] (k target anchor)) nil) f))

(defn array-call [^objects arr]
  (dotimes [i (alength arr)]
    ((aget arr i))))

(defn frame-dispose [^objects f]
  (aset f frame-slot-position nil)
  (array-call (aget f frame-slot-static))
  (array-call (aget f frame-slot-dynamic))
  (array-call (aget f frame-slot-foreign))
  (array-call (aget f frame-slot-variables))
  (array-call (aget f frame-slot-constants)))

(defn frame-rotate [^objects f to]
  (let [from (aget f frame-slot-position)
        step (compare to from)
        ^objects tier (aget f frame-slot-parent)
        ^objects buf (aget tier tier-slot-buffer)]
    (if (== to from)
      (let [size (dec (aget tier tier-slot-size))]
        (loop [i to]
          (when-not (== i size)
            (let [j (inc i)
                  y (aget buf (int j))]
              (aset ^objects y frame-slot-position i)
              (aset buf (int i) y)
              (recur j))))
        (aset tier tier-slot-size size)
        (aset buf (int size) nil)
        (frame-dispose f))
      (do (loop [i from]
            (let [j (+ i step)
                  ^objects y (aget buf (int j))]
              (aset y frame-slot-position i)
              (aset buf (int i) y)
              (when-not (== to j) (recur j))))
          (aset f frame-slot-position to)
          (aset buf (int to) f)
          (reduce notify-rotate f
            (find-scope f))))))

(defn move
  "Move a frame. If origin position is equal to target position, frame is removed. Will search and call `hook`."
  ([^objects tier from to]
   (let [f (aget ^objects (aget tier tier-slot-buffer) (int from))]
     (remote f (- to (aget tier tier-slot-size)))
     (frame-rotate f to))))

(defn frame-cancel [^objects f]
  (when-some [pos (aget f frame-slot-position)]
    (remote f (- pos (aget ^objects (aget f frame-slot-parent) tier-slot-size)))
    (frame-rotate f pos)))

(defn terminate-observers [^objects ctx]
  (when-some [cb (aget ctx context-slot-remote)] (cb nil))
  (when-some [cb (aget ctx context-slot-output)] (cb nil)))

(defn decode-inst [^objects ctx inst]
  (if-some [id (aget ctx context-slot-frame-register)]
    (let [^objects f (get (aget ctx context-slot-frame-store) id)]
      (aset ctx context-slot-frame-register nil)
      (if (neg? inst)
        (frame-rotate f (+ (aget ^objects (aget f frame-slot-parent) tier-slot-size) inst))
        (let [^objects inputs (aget f frame-slot-inputs)
              offset (alength inputs)]
          (if (< inst offset)
            (let [input (aget inputs (int inst))]
              (if (nil? (aget ctx context-slot-queue))
                (input-close input)
                (input-change input
                  (let [[x & xs] (aget ctx context-slot-queue)]
                    (aset ctx context-slot-queue xs) x))))
            (let [inst (- inst offset)
                  ^objects targets (aget f frame-slot-targets)
                  offset (alength targets)]
              (if (< inst offset)
                (aset ctx context-slot-target-register (aget targets (int inst)))
                (let [inst (- inst offset)
                      ^objects sources (aget f frame-slot-sources)
                      offset (alength sources)]
                  (if (< inst offset)
                    (let [source (aget sources (int inst))
                          target (aget ctx context-slot-target-register)]
                      (aset ctx context-slot-target-register nil)
                      (target source (aswap ctx context-slot-remote-id dec)))
                    (aswap ctx context-slot-frame-store dissoc! id)))))))))
    (aset ctx context-slot-frame-register (- inst))) ctx)

(defn acopy [dest src size]
  #?(:clj (System/arraycopy src 0 dest 0 size))
  #?(:cljs (dotimes [i size] (aset dest i (aget src i)))))

(defn constructor [static dynamic variable-count source-count constant-count target-count output-count input-count boot]
  (fn [^objects tier id]
    (let [^objects par (aget tier tier-slot-parent)
          ^objects buf (aget tier tier-slot-buffer)
          pos (aget tier tier-slot-size)
          cap (alength buf)
          buf (if (< pos cap)
                buf (aset tier tier-slot-buffer
                      (doto (object-array (bit-shift-left cap 1))
                        (acopy buf cap))))]
      (aset tier tier-slot-size (inc pos))
      (make-frame (aget par frame-slot-context)
        tier id pos (aget tier tier-slot-foreigns) static dynamic
        variable-count source-count constant-count target-count output-count input-count
        buf (aget tier tier-slot-vars) boot))))

(deftype FrameIterator [f it]
  IFn
  (#?(:clj invoke :cljs -invoke) [_] (frame-cancel f))
  IDeref
  (#?(:clj deref :cljs -deref) [_] @it))

;; Takes an instruction identifying a target and a frame-constructor.
;; Return a flow instantiating the frame.
(defn constant [^objects frame slot ctor]
  (let [^objects consts (aget frame frame-slot-constants)
        ^objects context (aget frame frame-slot-context)]
    (aset consts slot
      (signal
        (m/watch
          (atom
            (fn [n t]
              (if-some [^objects tier (l/get-local this)]
                (let [parent (aget tier tier-slot-parent)
                      id (aswap context context-slot-local-id inc)]
                  (remote frame (+ (alength ^objects (aget frame frame-slot-outputs)) slot))   ; notify remote peer of frame creation
                  (remote parent
                    (+ (alength ^objects (aget parent frame-slot-outputs))
                      (alength ^objects (aget parent frame-slot-constants))
                      (aget tier tier-slot-remote)))                                            ; notify remote peer of frame mount point (on which tier we want to create this frame, its parent)
                  (let [<x (ctor tier id)
                        ^objects f (get (aget context context-slot-frame-store) id)]
                    (->FrameIterator f
                      (<x n #(do (frame-cancel f)
                                 (remote f
                                   (+ (alength ^objects (aget f frame-slot-outputs))
                                     (alength ^objects (aget f frame-slot-constants))
                                     (alength ^objects (aget f frame-slot-variables))))
                                 (aswap context context-slot-frame-store dissoc!
                                   (aget f frame-slot-id)) (t))))))
                (failer/run (error "Unable to build frame - not an object.") n t)))))))))

(defn inject [v]
  (fn [<x <y]
    (fn [n t]
      ;; TODO make result depend on <y to catch failures, in case binding is ignored
      (if-some [^objects tier (l/get-local this)]
        (let [foreigns (aget tier tier-slot-foreigns)]
          (aset tier tier-slot-foreigns (assoc foreigns v <y))
          (try (<x n t) (finally (aset tier tier-slot-foreigns foreigns))))
        (failer/run (error "Unable to inject - not an object.") n t)))))

(defn bind [f & args]
  (fn [n t]
    (if-some [tier (l/get-local this)]
      (try ((apply f tier args) n t) ; hook tier and pass to userland !
           (catch #?(:clj Throwable :cljs :default) e
             (failer/run e n t)))
      (failer/run (error "Unable to bind - not an object.") n t))))

(defn with [tier <x]
  (fn [n t]
    (let [prev (l/get-local this)]
      (l/set-local this tier)
      (try (<x n t) (finally (l/set-local this prev))))))

(defn clause
  ([f] (fn [e] (f (pure e))))
  ([f c] (fn [e] (when (instance? c (dbg/unwrap e)) (f (pure e))))))

(defn recover [tier catch <x]
  (yield (fn [x]
           (when (instance? Failure x)
             (when-some [<c (catch (.-error ^Failure x))]
               (with tier <c)))) <x))

(defn variable [^objects frame ^objects vars position slot <<x]
  (let [^objects tier (aget ^objects (aget frame frame-slot-tiers) (int position))]
    (aset tier tier-slot-remote slot)
    (aset tier tier-slot-vars (aclone vars))
    (aset ^objects (aget frame frame-slot-variables) (int slot)
      (m/signal!
        (m/cp (try (let [<x (m/?< <<x)]
                     (if (failure <x)
                       <x (m/?< (with tier <x))))
                   (catch #?(:clj Throwable :cljs :default) e
                     (Failure. e))))))))

(defn source [^objects frame ^objects vars position slot]
  (aset ^objects (aget frame frame-slot-sources) (int slot)
    (doto ^objects (aget ^objects (aget frame frame-slot-tiers) (int position))
      (aset tier-slot-vars (aclone vars)))))

(defn target [^objects frame slot ctor]
  (aset ^objects (aget frame frame-slot-targets) (int slot) ctor))

(defn hook [k v <x]
  (assert (some? v) "hook value must be non-nil.")
  (fn [n t]
    (if-some [tier (l/get-local this)]
      (do
        (loop [^objects tier tier]
          (let [^objects f (aget tier tier-slot-parent)]
            (if-some [a (frame-walk-tiers f 1 k identity (aget tier tier-slot-position))]
              (k v a)
              (when-some [^objects tier (aget f frame-slot-parent)]
                (if-some [a (tier-walk-frames tier 1 k identity (aget f frame-slot-position))]
                  (k v a)
                  (if (contains? (aget tier tier-slot-hooks) k)
                    (k v nil) (recur tier)))))))
        (aswap tier tier-slot-hooks assoc k v)
        (<x n #(do (aswap tier tier-slot-hooks dissoc k)
                   (k v) (t))))
      (failer/run (error "Unable to hook - not an object.") n t))))

(def unbound (pure ::unbound))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (js/console.error e)))

(defn remote-event [x] [x {} #{}])
(defn change-event [path x] [[] {path x} #{}])
(defn terminate-event [path] [[] {} #{path}])
(def merge-events (partial mapv into))

(defn frame-outputs [^objects frame]
  (let [^objects outputs (aget frame frame-slot-outputs)
        frame-id (aget frame frame-slot-id)]
    (map (fn [slot] [[frame-id slot] (aget outputs slot)])
      (range (alength outputs)))))

(defn subject-at [^objects arr slot]
  (fn [!] (aset arr slot !) #(aset arr slot nil)))

(defn gather-events [>remote >output]
  (gather merge-events
    (m/ap
      (m/amb
        (m/sample remote-event >remote)
        (let [[path <out] (m/?> (m/eduction cat (map frame-outputs) cat >output))]
          (eventually (terminate-event path)
            (m/latest (partial change-event path) <out)))))))

;; TODO merge to the IO layer
(defn event->message [[inst data done]]
  (-> []
    (into (vals data))
    (conj (-> inst
            (into cat (keys data))
            (into cat done)))))

(defn parse-event [^objects ctx msg]
  (aset ctx context-slot-queue (seq (pop msg)))
  (reduce decode-inst ctx (peek msg)))

(defn frame-shutdown [^objects frame]
  (frame-dispose frame)
  (let [^objects inputs (aget frame frame-slot-inputs)]
    (dotimes [i (alength inputs)]
      (input-close (aget inputs i))))
  (let [^objects tiers (aget frame frame-slot-tiers)]
    (dotimes [t (alength tiers)]
      (let [^objects tier (aget tiers t)
            ^objects buf (aget tier tier-slot-buffer)]
        (dotimes [f (aget tier tier-slot-size)]
          (frame-shutdown (aget buf f)))))))

(defn redirect-errors [cb <x]
  (m/latest (fn [x] (when (instance? Failure x) (cb (.-error x)))) <x))

(defn process-incoming-events [^objects context >incoming]
  (m/sample (partial reduce parse-event context) >incoming))

(defn write-outgoing-events [write >events]
  (m/ap (m/? (write (event->message (m/?> >events))))))

(defn peer [var-count dynamic variable-count source-count constant-count target-count output-count input-count ctor]
  (fn rec
    ([write ?read] (rec write ?read pst))
    ([write ?read on-error]
     (m/reactor
       (let [^objects context (make-context)
             >remote (->> (subject-at context context-slot-remote)
                       (m/observe)
                       (m/eduction (take-while some?))
                       (m/relieve into)
                       (m/stream!))
             >output (->> (subject-at context context-slot-output)
                       (m/observe)
                       (m/eduction (take-while some?))
                       (m/relieve into)
                       (m/stream!))]
         (when-some [<main (make-frame context nil 0 0 {} [] dynamic
                             variable-count source-count constant-count target-count output-count input-count
                             context (object-array (repeat var-count unbound)) ctor)]
           (m/stream! (redirect-errors on-error <main)))
         (m/stream! (process-incoming-events context (m/stream! (m/relieve into (m/sample vector (m/observe ?read))))))
         (m/stream! (write-outgoing-events write (m/stream! (gather-events >remote >output)))))))))

(defn collapse [s n f & args]
  (->> (iterate pop s)
    (transduce (comp (map peek) (take n)) conj args)
    (apply f)
    (conj (nth (iterate pop s) n))))

(defn snapshot [env f & args]
  (update env :stack conj (apply f env args)))

(defn reverse-index [m]
  (reduce-kv (fn [v x i] (assoc v i x))
    (vec (repeat (count m) nil)) m))

(def empty-frame
  {:variable 0
   :source   0
   :constant 0
   :target   0
   :output   0
   :input    0
   :static   {}
   :dynamic  {}})

;; TODO move me
;; `new` creates a local variable and a remote source
;; `p/fn` creates a local constant and a remote target
;; Same duality with input and output, if there is 3 inputs locally, there is 3 outputs remotely.
;; There is no instruction to create inputs and outputs, they are infered from unquote-splicing.

(defn compile [inst {:keys [nop sub pub inject lift vget bind invoke input output static dynamic
                            global literal variable source constant target main] :as fns}]
  (-> ((fn walk [env off idx dyn inst]
         (case (::ir/op inst)
           ::ir/nop (update env :stack conj nop)
           ::ir/sub (let [p (- idx (::ir/index inst))]
                      (if (< p off)
                        (let [f (:static env)
                              i (f p (count f))]
                          (-> env
                            (assoc :static (assoc f p i))
                            (update :stack conj (static i))))
                        (update env :stack conj (sub p))))
           ::ir/pub (-> env
                      (walk off idx dyn (::ir/init inst))
                      (walk off (inc idx) dyn (::ir/inst inst))
                      (update :stack collapse 2 pub idx))
           ::ir/def (let [v (::ir/slot inst)]
                      (-> env
                        (update :vars max v)
                        (update :stack conj (inject v))))
           ::ir/lift (-> env
                       (walk off idx dyn (::ir/init inst))
                       (update :stack collapse 1 lift))
           ::ir/eval (update env :stack conj ((:eval fns) (::ir/form inst))) ; can't shadow eval in advanced CLJS compilation
           ::ir/node (let [v (::ir/slot inst)
                           env (update env :vars max v)]
                       (if (dyn v)
                         (update env :stack conj (vget v))
                         (let [d (:dynamic env)
                               i (d v (count d))]
                           (-> env
                             (assoc :dynamic (assoc d v i))
                             (update :stack conj (dynamic i inst))))))
           ::ir/bind (let [v (::ir/slot inst)]
                       (-> env
                         (update :vars max v)
                         (walk off idx (conj dyn v) (::ir/inst inst))
                         (update :stack collapse 1 bind v (- idx (::ir/index inst)))))
           ::ir/apply (let [f (::ir/fn inst)
                            args (::ir/args inst)]
                        (-> (reduce (fn [env arg] (walk env off idx dyn arg)) env (cons f args))
                          (update :stack collapse (inc (count args))
                            (partial invoke
                              (loop [f f]
                                (case (::ir/op f)
                                  ::ir/global (assoc f ::dbg/type :apply, ::dbg/name (symbol (::ir/name f)))
                                  ::ir/node (assoc f ::dbg/type :apply)
                                  ::ir/literal {::dbg/type :apply ::dbg/name (::ir/value f)}
                                  ::ir/eval (assoc f ::dbg/type :eval)
                                  ::ir/sub (assoc f ::dbg/type :apply)
                                  ::ir/input (assoc f ::dbg/type :apply)
                                  ::ir/apply (recur (::ir/fn f))
                                  {::dbg/type :unknown-apply, :op f}))))))
           ::ir/input (-> env
                        (snapshot :input)
                        (update :input inc)
                        (update :stack collapse 1 input))
           ::ir/output (-> env
                         (walk off idx dyn (::ir/init inst))
                         (snapshot :output)
                         (update :output inc)
                         (walk off idx dyn (::ir/inst inst))
                         (update :stack collapse 3 #(output %1 %2 %3 inst)))
           ::ir/global (update env :stack conj (global (::ir/name inst)))
           ::ir/literal (update env :stack conj (literal (::ir/value inst)))
           ::ir/variable (-> env
                           (walk off idx dyn (::ir/init inst))
                           (snapshot :source)
                           (snapshot :variable)
                           (update :variable inc)
                           (update :stack collapse 3 variable))
           ::ir/source (-> env
                         (snapshot :variable)
                         (snapshot :source)
                         (update :source inc)
                         (walk off idx dyn (::ir/inst inst))
                         (update :stack collapse 3 source))
           ::ir/constant (-> env
                           (merge empty-frame)
                           (walk idx idx #{} (::ir/init inst))
                           (snapshot (comp reverse-index :static))
                           (snapshot (comp reverse-index :dynamic))
                           (snapshot :variable)
                           (snapshot :source)
                           (snapshot :constant)
                           (snapshot :target)
                           (snapshot :output)
                           (snapshot :input)
                           (merge (select-keys env (keys empty-frame)))
                           (snapshot :constant)
                           (update :constant inc)
                           (update :stack collapse 10 (partial constant inst)))
           ::ir/target (-> env
                         (merge empty-frame)
                         (walk idx idx #{} (::ir/init inst))
                         (snapshot (comp reverse-index :static))
                         (snapshot (comp reverse-index :dynamic))
                         (snapshot :variable)
                         (snapshot :source)
                         (snapshot :constant)
                         (snapshot :target)
                         (snapshot :output)
                         (snapshot :input)
                         (merge (select-keys env (keys empty-frame)))
                         (snapshot :target)
                         (update :target inc)
                         (walk off idx dyn (::ir/inst inst))
                         (update :stack collapse 11 target))))
       (assoc empty-frame :vars -1) 0 0 #{} inst)
    (snapshot (comp inc :vars))
    (snapshot (comp reverse-index :dynamic))
    (snapshot :variable)
    (snapshot :source)
    (snapshot :constant)
    (snapshot :target)
    (snapshot :output)
    (snapshot :input)
    (:stack)
    (collapse 9 main)
    (peek)))

(defn sym [& args]
  (symbol (str/join "-" args)))

(defn emit [prefix inst]
  (compile inst
    {:nop      nil
     :sub      (fn [idx] (sym prefix 'pub idx))
     :pub      (fn [form cont idx]
                 `(let [~(sym prefix 'pub idx) (signal ~form)] ~cont))
     :static   (fn [i] `(static ~(sym prefix 'frame) ~i))
     :dynamic  (fn [i debug-info] `(dynamic ~(sym prefix 'frame) ~i '~(select-debug-info debug-info)))
     :eval     (fn [f] `(pure ~f))
     :lift     (fn [f] `(pure ~f))
     :vget     (fn [v] `(aget ~(sym prefix 'vars) (int ~v)))
     :bind     (fn [form slot idx]
                 `(let [~(sym prefix 'prev) (aget ~(sym prefix 'vars) ~slot)]
                    (aset ~(sym prefix 'vars) (int ~slot) ~(sym prefix 'pub idx))
                    (let [~(sym prefix 'res) ~form]
                      (aset ~(sym prefix 'vars) (int ~slot) ~(sym prefix 'prev))
                      ~(sym prefix 'res))))
     :invoke   (fn [debug-info & forms] `(latest-apply '~(select-debug-info debug-info) ~@forms))
     :input    (fn [slot] `(input ~(sym prefix 'frame) ~slot))
     :output   (fn [form slot cont debug-info]
                 `(do (output ~(sym prefix 'frame) ~slot ~form '~(select-debug-info debug-info)) ~cont))
     :global   (fn [x] `(pure ~(symbol x)))
     :literal  (fn [x] `(pure (quote ~x)))
     :inject   (fn [v] `(pure (inject ~v)))
     :variable (fn [form remote slot]
                 `(variable ~(sym prefix 'frame) ~(sym prefix 'vars) ~(+ remote slot) ~slot ~form))
     :source   (fn [remote slot cont]
                 `(do (source ~(sym prefix 'frame) ~(sym prefix 'vars) ~(+ remote slot) ~slot) ~cont))
     :constant (fn [debug-info form static dynamic variable-count source-count constant-count target-count output-count input-count slot]
                 `(constant ~(sym prefix 'frame) ~slot
                    (constructor ~(mapv (fn [p] (sym prefix 'pub p)) static) ~dynamic
                      ~variable-count ~source-count
                      ~constant-count ~target-count
                      ~output-count ~input-count
                      (fn [~(sym prefix 'frame)
                           ~(sym prefix 'vars)]
                        (check-failure '~(select-debug-info debug-info) ~form)))))
     :target   (fn [form static dynamic variable-count source-count constant-count target-count output-count input-count slot cont]
                 `(do (target ~(sym prefix 'frame) ~slot
                        (constructor ~(mapv (fn [p] (sym prefix 'pub p)) static) ~dynamic
                          ~variable-count ~source-count
                          ~constant-count ~target-count
                          ~output-count ~input-count
                          (fn [~(sym prefix 'frame)
                               ~(sym prefix 'vars)]
                            ~form))) ~cont))
     :main     (fn [form var-count dynamic variable-count source-count constant-count target-count output-count input-count]
                 `(peer ~var-count ~dynamic
                    ~variable-count ~source-count
                    ~constant-count ~target-count
                    ~output-count ~input-count
                    (fn [~(sym prefix 'frame)
                         ~(sym prefix 'vars)]
                      ~form)))}))

(tests
  (emit nil {::ir/op ::ir/literal ::ir/value 5}) :=
  `(peer 0 [] 0 0 0 0 0 0
     (fn [~'-frame ~'-vars]
       (pure '5)))

  (emit nil {::ir/op   ::ir/apply
             ::ir/fn   {::ir/op ::ir/global ::ir/name :clojure.core/+}
             ::ir/args [{::ir/op ::ir/literal ::ir/value 2}
                        {::ir/op ::ir/literal ::ir/value 3}]}) :=
  `(peer 0 [] 0 0 0 0 0 0
     (fn [~'-frame ~'-vars]
       (latest-apply '{::ir/op    ::ir/global
                       ::dbg/type :apply
                       ::dbg/name ~'clojure.core/+}
         (pure ~'clojure.core/+)
         (pure '2)
         (pure '3))))

  (emit nil
    {::ir/op   ::ir/pub
     ::ir/init {::ir/op ::ir/literal ::ir/value 1}
     ::ir/inst {::ir/op   ::ir/apply
                ::ir/fn   {::ir/op ::ir/global ::ir/name :clojure.core/+}
                ::ir/args [{::ir/op ::ir/sub ::ir/index 1}
                           {::ir/op ::ir/literal ::ir/value 2}]}}) :=
  `(peer 0 [] 0 0 0 0 0 0
     (fn [~'-frame ~'-vars]
       (let [~'-pub-0 (signal (pure '1))]
         (latest-apply '{::ir/op    ::ir/global
                         ::dbg/type :apply
                         ::dbg/name ~'clojure.core/+}
           (pure ~'clojure.core/+) ~'-pub-0 (pure '2)))))

  (emit nil
    {::ir/op ::ir/variable
     ::ir/init {::ir/op ::ir/global
                ::ir/name :missionary.core/none}}) :=
  `(peer 0 [] 1 0 0 0 0 0
     (fn [~'-frame ~'-vars]
       (variable ~'-frame ~'-vars 0 0 (pure m/none))))

  (emit nil {::ir/op ::ir/input}) :=
  `(peer 0 [] 0 0 0 0 0 1
     (fn [~'-frame ~'-vars]
       (input ~'-frame 0)))

  (emit nil {::ir/op   ::ir/constant
             ::ir/init {::ir/op ::ir/literal
                        ::ir/value :foo}}) :=
  `(peer 0 [] 0 0 1 0 0 0
     (fn [~'-frame ~'-vars]
       (constant ~'-frame 0
         (constructor [] [] 0 0 0 0 0 0
           (fn [~'-frame ~'-vars]
             (check-failure '{::ir/op ::ir/constant}
               (pure ':foo)))))))

  (emit nil
    {::ir/op   ::ir/variable
     ::ir/init {::ir/op   ::ir/pub
                ::ir/init {::ir/op   ::ir/constant
                           ::ir/init {::ir/op    ::ir/literal
                                      ::ir/value 3}}
                ::ir/inst {::ir/op   ::ir/pub
                           ::ir/init {::ir/op   ::ir/constant
                                      ::ir/init {::ir/op ::ir/input}}
                           ::ir/inst {::ir/op   ::ir/apply
                                      ::ir/fn   {::ir/op   ::ir/apply
                                                 ::ir/fn   {::ir/op   ::ir/global
                                                            ::ir/name :clojure.core/hash-map}
                                                 ::ir/args [{::ir/op ::ir/literal ::ir/value 2}
                                                            {::ir/op ::ir/sub ::ir/index 2}
                                                            {::ir/op ::ir/literal ::ir/value 4}
                                                            {::ir/op ::ir/sub ::ir/index 1}
                                                            {::ir/op ::ir/literal ::ir/value 5}
                                                            {::ir/op ::ir/sub ::ir/index 1}]}
                                      ::ir/args [{::ir/op ::ir/literal ::ir/value 1}
                                                 {::ir/op ::ir/constant ::ir/init {::ir/op ::ir/literal ::ir/value 7}}]}}}})
  `(peer 0 [] 1 0 3 0 0 0
     (fn [~'-frame ~'-vars]
       (variable ~'-frame ~'-vars 0 0
         (let [~'-pub-0 (signal
                          (constant ~'-frame 0
                            (constructor [] [] 0 0 0 0 0 0
                              (fn [~'-frame ~'-vars]
                                (check-failure 'nil (pure '3))))))]
           (let [~'-pub-1 (signal
                            (constant ~'-frame 1
                              (constructor [] [] 0 0 0 0 0 1
                                (fn [~'-frame ~'-vars]
                                  (check-failure 'nil (input ~'-frame 0))))))]
             (latest-apply '{::dbg/type :unknown-apply, ; FIXME remove this debug noise
                             :op
                             [:apply
                              [:global :clojure.core/hash-map nil]
                              [:literal 2]
                              [:sub 2]
                              [:literal 4]
                              [:sub 1]
                              [:literal 5]
                              [:sub 1]]}
               (latest-apply '{::ir/op ::ir/global
                               ::dbg/type :apply,
                               ::dbg/name clojure.core/hash-map}
                 (pure hash-map)
                 (pure '2) ~'-pub-0
                 (pure '4) ~'-pub-1
                 (pure '5) ~'-pub-1)
               (pure '1)
               (constant ~'-frame 2
                 (constructor [] [] 0 0 0 0 0 0
                   (fn [~'-frame ~'-vars]
                     (check-failure 'nil (pure '7)))))))))))

  (emit nil {::ir/op ::ir/def ::ir/slot 0}) :=
  `(peer 1 [] 0 0 0 0 0 0
     (fn [~'-frame ~'-vars]
       (pure (inject 0))))

  (emit nil
    {::ir/op   ::ir/pub
     ::ir/init {::ir/op ::ir/literal ::ir/value nil}
     ::ir/inst {::ir/op ::ir/constant ::ir/init {::ir/op ::ir/sub ::ir/index 1}}}) :=
  `(peer 0 [] 0 0 1 0 0 0
     (fn [~'-frame ~'-vars]
       (let [~'-pub-0 (signal (pure 'nil))]
         (constant ~'-frame 0
           (constructor [~'-pub-0] [] 0 0 0 0 0 0
             (fn [~'-frame ~'-vars]
               (check-failure '{::ir/op ::ir/constant}
                 (static ~'-frame 0)))))))))

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
     {:nop      (constantly nil)
      ;; FIXME Eval is a security hazard, client should not send any program to
      ;;       eval on server. Solution is for server to compile and store
      ;;       programs, client address them by unique name (hash).
      :eval     (fn [form] (constantly (pure (clojure.core/eval form))))
      :sub      (fn [idx]
                  (fn [pubs frame vars]
                    (nth pubs idx)))
      :pub      (fn [form cont _]
                  (fn [pubs frame vars]
                    (cont (conj pubs (signal (form pubs frame vars))) frame vars)))
      :lift     (fn [form]
                  (fn [pubs frame vars]
                    (pure (form pubs frame vars))))
      :vget     (fn [slot]
                  (fn [pubs frame ^objects vars]
                    (aget vars (int slot))))
      :static   (fn [slot]
                  (fn [pubs frame vars]
                    (static frame slot)))
      :dynamic  (fn [slot debug-info]
                  (fn [pubs frame vars]
                    (dynamic frame slot debug-info)))
      :bind     (fn [form slot s]
                  (fn [pubs frame ^objects vars]
                    (let [prev (aget vars (int slot))]
                      (aset vars (int slot) (nth pubs s))
                      (let [res (form pubs frame vars)]
                        (aset vars (int slot) prev) res))))
      :invoke   (fn [debug-info & forms] (apply juxt-with (partial latest-apply debug-info) forms))
      :input    (fn [slot]
                  (fn [pubs frame vars]
                    (input frame slot)))
      :output   (fn [form slot cont debug-info]
                  (fn [pubs frame vars]
                    (output frame slot (form pubs frame vars) debug-info)
                    (cont pubs frame vars)))
      :global   (fn [x]
                  (let [r (resolvef ::not-found x)]
                    (case r
                      ::not-found (throw (ex-info (str "Unable to resolve symbol: " (symbol x)) {}))
                      (constantly (pure r)))))
      :literal  (fn [x] (constantly (pure x)))
      :inject   (fn [slot] (constantly (pure (inject slot))))
      :variable (fn [form remote slot]
                  (fn [pubs frame vars]
                    (variable frame vars (+ remote slot) slot
                      (form pubs frame vars))))
      :source   (fn [remote slot cont]
                  (fn [pubs frame vars]
                    (source frame vars (+ remote slot) slot)
                    (cont pubs frame vars)))
      :constant (fn [debug-info form static dynamic variable-count source-count constant-count target-count output-count input-count slot]
                  (fn [pubs frame vars]
                    (constant frame slot
                      (constructor (mapv pubs static) dynamic variable-count source-count constant-count target-count output-count input-count
                        (fn [& args]
                          (check-failure debug-info (apply form pubs args)))))))
      :target   (fn [form static dynamic variable-count source-count constant-count target-count output-count input-count slot cont]
                  (fn [pubs frame vars]
                    (target frame slot
                      (constructor (mapv pubs static) dynamic variable-count source-count constant-count target-count output-count input-count
                        (partial form pubs)))
                    (cont pubs frame vars)))
      :main     (fn [form var-count dynamic variable-count source-count constant-count target-count output-count input-count]
                  (peer var-count dynamic
                    variable-count source-count
                    constant-count target-count
                    output-count input-count
                    (partial form [])))})))
