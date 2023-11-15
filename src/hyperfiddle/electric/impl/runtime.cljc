(ns ^:no-doc hyperfiddle.electric.impl.runtime
  (:refer-clojure :exclude [compile])
  (:require [hyperfiddle.electric.impl.yield2 :refer [yield]]
            [hyperfiddle.electric.impl.failer :as failer]
            [hyperfiddle.electric.impl.local :as l]
            [hyperfiddle.electric.impl.ir :as ir]
            [hyperfiddle.electric.debug :as dbg]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str]
            [contrib.data :as data]
            [contrib.assert :as ca]
            [contrib.walk :as walk]
            [hyperfiddle.electric.impl.ir-utils :as ir-utils])
  (:import missionary.Cancelled
           (hyperfiddle.electric Failure Pending Remote)
           #?(:clj (clojure.lang IFn IDeref Atom))))

;; An Electric program is a tree, which structure is dynamically maintained.
;; Two peers are synchronized (through a protocol) such that the tree structure is identicaly on both peers.
;; Two type of nodes:
;; [Frames] : A piece of DAG with a static structure. Won't be rearanged at runtime. (AKA Static Frame)
;;            - A set of compiled s-expressions + a set of signals weaving these expressions + N inputs + N outputs
;;            - A frame has 2 instances, one on client, one on server.
;;            - Server's outputs are client's inputs and vice-versa.
;;            - Frames are processes.
;;            - Image: a stackframe but for a DAG. A stackframe is allocated to compute the result of a function.
;;              It is volatile (disposable) in a stack-based program. Since Electric is reactive, the frame is not disposable.
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

;; Network protocol
;; Each peer streams events to its remote peer via a bidirectional channel. An event is a clojure map with 4 entries :
;; * :acks is a non-negative integer counting the number of non-empty changesets received by the peer sending the event
;;   since the previous event was sent.
;; * :tree is a vector of tree instructions. Order of instructions matters. A tree instruction describes an atomic mutation of the tree, it is a map
;;   with a mandatory :op entry defining the instruction type and defining the rest of the keyset. Instructions are :
;;   * :create appends a new frame at the end of a tier, owned by the peer sending the event. The frame constructor is
;;     defined by the entry :target, the endpoint is defined by the entry :source. Both are ordered pairs of two
;;     numbers, the frame id and the position of the target or the source in the frame.
;;   * :rotate performs of cyclic permutation of frames in a tier, owned by the peer sending the event. The frame
;;     identified by the :frame entry is moved to position defined by the :position entry. If the cycle is trivial (A -> A), the
;;     frame is removed.
;;   * :remove dissociates a frame from the index. legacy hack, should be removed.
;; * :change is a map associating ports with values. A port is absolute for the system, inputs and output relative terms for a port (it only make sense from a single peer perspective).
;;   A port is represented as an ordered pair of two numbers, a frame id
;;   and the position of this port in the frame. The value is the new state of port. (:assign could be a synonym)
;; * :freeze is a set of ports. Each port present in this set must be considered terminated (i.e. its state won't ever
;;   change again).
;; A frame id is negative if the frame is owned by the peer sending the event, positive if the frame is owned by the
;; peer receiving the event, zero is the root frame.

(defn fail [exception _in-scope-stacktrace]
  ;; When throwing from a `catch` block, we want to throw an exception while preserving the stack trace of the exception that triggered the catch block.
  ;; first arg is the exception we want to throw
  ;; second arg is the exception in scope (if we are in a catch block) or nil
  ;; second arg is ignored here, but being part of the arguments, it will be
  ;; visible to `latest-apply` and so be part of the async stack trace.
  ;; See `handle-apply-error`.
  (throw exception))

(def failure (some-fn #(when (instance? Failure %) %)))

(def pending (Failure. (Pending.)))

(defn error [^String msg] ; Could be ex-info (ExceptionInfo inherits Error or js/Error)
  (#?(:clj Error. :cljs js/Error.) msg))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (js/console.error e)))

(defn select-debug-info [debug-info]
  (merge (select-keys debug-info [::ir/op]) (data/select-ns :hyperfiddle.electric.debug debug-info)))

(defn check-failure [debug-info <x]
  (m/latest (fn [x]
              (if (instance? Failure x)
                (dbg/error (select-debug-info debug-info) x)
                x)) <x))

(defn handle-apply-error [debug-info args error]
  (if (= `fail (::dbg/name debug-info))
    (let [[thrown context] args]
      (dbg/error (assoc (select-debug-info debug-info) ::dbg/args [thrown]) (Failure. error) context))
    (dbg/error (assoc (select-debug-info debug-info) ::dbg/args args) (Failure. error))))

(defn latest-apply [debug-info & args]
  (ca/check (partial every? some?) args debug-info)
  (apply m/latest
    (fn [f & args]
      (if-let [err (apply failure f args)]
        (dbg/error (assoc (select-debug-info debug-info) ::dbg/args args) err)
        (try (apply f args)
             (catch #?(:clj Throwable :cljs :default) e
               (handle-apply-error debug-info args e)))))
    args))

(def latest-first
  (partial m/latest
    (fn [x y] (if (instance? Failure y) y x))))

(defn pure [x] (m/cp x))

(def empty-event
  {:acks 0
   :tree []
   :change {}
   :freeze #{}})

(deftype It [state cancel transfer]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer state)))

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

(def context-slot-root            (int 0))   ;; Immutable. The root frame.
(def context-slot-local-id        (int 1))   ;; The next local id (auto incremented).
(def context-slot-remote-id       (int 2))   ;; The next remote id (auto decremented).
(def context-slot-pending-rpos    (int 3))   ;; The reading position in the pending circular buffer.
(def context-slot-pending-wpos    (int 4))   ;; The writing position in the pending circular buffer.
(def context-slot-pending-buffer  (int 5))   ;; The pending circular buffer of outputs changed for each message sent.
(def context-slot-frame-store     (int 6))   ;; A transient map associating frame ids to frame objects.
(def context-slot-event           (int 7))   ;; The next event to transfer.
(def context-slot-check           (int 8))   ;; The set of inputs that must be checked on next event transfer.
(def context-slot-notifier        (int 9))   ;; The notifier callback
(def context-slot-terminator      (int 10))  ;; The terminator callback
(def context-slot-incoming        (int 11))  ;; The incoming callback
(def context-slots                (int 12))

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
(def frame-slot-last-variable (int 13))
(def frame-slot-last-constant (int 14))
(def frame-slot-last-source (int 15))
(def frame-slot-last-target (int 16))
(def frame-slot-last-input (int 17))
(def frame-slots          (int 18))

(def input-slot-frame      (int 0))                         ;; parent frame
(def input-slot-notifier   (int 1))                         ;; consumer notifier
(def input-slot-terminator (int 2))                         ;; consumer terminator
(def input-slot-current    (int 3))                         ;; current state
(def input-slot-dirty      (int 4))                         ;; head of linked list of dirty outputs
(def input-slot-check      (int 5))                         ;; next item in linked list of check inputs
(def input-slot-pending    (int 6))                         ;; number of outputs waiting for ack
(def input-slot-cancel     (int 7))                         ;; nil when no pending transfer, otherwise cancel status
(def input-slots           (int 8))

(def output-slot-input    (int 0))                          ;; parent input
(def output-slot-id       (int 1))                          ;; output id, relative to parent frame
(def output-slot-iterator (int 2))                          ;; producer iterator
(def output-slot-current  (int 3))                          ;; current state
(def output-slot-dirty    (int 4))                          ;; tail of linked list of dirty outputs
(def output-slot-done     (int 5))                          ;; frozen
(def output-slot-prev     (int 6))                          ;; previous item in doubly linked list of pending outputs
(def output-slot-next     (int 7))                          ;; next item in doubly linked list of pending outputs
(def output-slot-time     (int 8))                          ;; position of the doubly linked list of pending outputs in the circular buffer, nil if not pending
(def output-slots         (int 9))

(defn frame-id []
  (fn [n t]
    (let [^objects tier (l/get-local this)
          ^objects frame (aget tier tier-slot-parent)
          id (aget frame frame-slot-id)]
      (n) (reify
            IFn (#?(:clj invoke :cljs -invoke) [_])
            IDeref (#?(:clj deref :cljs -deref) [_] (t) id)))))

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

(defn make-context ^objects []
  (doto (object-array context-slots)
    (aset context-slot-local-id (identity 0))
    (aset context-slot-remote-id (identity 0))
    (aset context-slot-pending-rpos (identity 0))
    (aset context-slot-pending-wpos (identity 0))
    (aset context-slot-pending-buffer (object-array 2))
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

(defn inject-foreigns [frame vars foreign]
  (reduce-kv
    (fn [m sym <x]
      (let [prev (get @vars sym)
            proc (signal <x)]
        (aset ^objects (aget frame frame-slot-foreign) (count m) proc)
        (swap! vars assoc sym proc)
        (assoc m sym prev)))
    {} foreign))

(defn undo-foreigns [vars prevs] (reduce-kv (fn [vrs sym <x] (doto vrs (swap! assoc sym <x))) vars prevs))

(defn init-statics [frame static]
  (reduce-kv (fn [^objects arr i <x]
               (aset arr (int i) (signal <x)) arr)
    (aget frame frame-slot-static) static))

(defn- init-dynamic [vars node-sym frame]
  (when-some [flow (get vars node-sym)]
    (aswap frame frame-slot-dynamic assoc node-sym (signal flow))))

(defn- init-dynamics [frame vars dynamics]
  (run! #(init-dynamic @vars % frame) dynamics))

(defn ensure-node [frame vars env node-info]
  (let [node-sym (:var-name node-info)]
    (when-not (contains? @vars node-sym)
      (init-dynamics frame vars (:dynamic node-info))
      (let [flow ((:fn node-info) frame vars env)]
        (swap! vars assoc node-sym flow)))))

(defn- init-nodes [frame vars env get-used-nodes]
  (doseq [info (get-used-nodes)]
    (when (map? info)                   ; skip unbound nodes
      (init-nodes frame vars env (:get-used-nodes info))
      (ensure-node frame vars env info))))

(defn make-frame [^objects context parent id position
                  foreign static dynamic variable-count source-count
                  constant-count target-count output-count input-count
                  ^objects buffer ^objects vars boot get-used-nodes nm env]
  (let [tier-count (+ variable-count source-count)
        frame (doto (object-array frame-slots)
                (aset frame-slot-context context)
                (aset frame-slot-parent parent)
                (aset frame-slot-id id)
                (aset frame-slot-position position)
                (aset frame-slot-foreign (object-array (count foreign)))
                (aset frame-slot-static (object-array (count static)))
                (aset frame-slot-dynamic {})
                (aset frame-slot-variables (object-array variable-count))
                (aset frame-slot-sources (object-array source-count))
                (aset frame-slot-targets (object-array target-count))
                (aset frame-slot-inputs (object-array input-count))
                (aset frame-slot-tiers (object-array tier-count))
                (aset frame-slot-constants (object-array constant-count))
                (aset frame-slot-last-variable -1)
                (aset frame-slot-last-constant -1)
                (aset frame-slot-last-source -1)
                (aset frame-slot-last-target -1)
                (aset frame-slot-last-input -1))]
    (dotimes [i tier-count] (make-tier frame i))
    (aset buffer (int position) frame)
    (aswap context context-slot-frame-store assoc! id frame)
    (let [prevs (inject-foreigns frame vars foreign)]
      (init-statics frame static)
      (init-nodes frame vars env get-used-nodes)
      (init-dynamics frame vars dynamic)
      ;; (prn :make-frame nm dynamic @(aget frame frame-slot-dynamic))
      (let [result (boot frame vars env)]
        (undo-foreigns vars prevs)
        result))))

(defn input-ready [^objects input]
  (when (nil? (aget input input-slot-cancel))
    (aset input input-slot-cancel false)
    ((aget input input-slot-notifier))))

(defn output-dirty [^objects output]
  (let [^objects input (aget output output-slot-input)
        ^objects dirty (aget input input-slot-dirty)]
    (aset output output-slot-dirty dirty)
    (aset input input-slot-dirty output)
    (when (nil? dirty)
      (when (identical? input (aget input input-slot-check))
        (let [^objects frame (aget input input-slot-frame)
              ^objects context (aget frame frame-slot-context)
              ^objects check (aget context context-slot-check)]
          (aset context context-slot-check input)
          (aset input input-slot-check check)
          (when (nil? (aget context context-slot-event))
            (aset context context-slot-event empty-event)
            ((aget context context-slot-notifier))))))
    (input-ready input)))

(defn output-spawn [^objects input ^objects output]
  (when-not (nil? output)
    (aset output output-slot-input input)
    (aset output output-slot-iterator
      ((aget output output-slot-iterator)
       (fn [] (output-dirty output))
       (fn []
         (aset output output-slot-done true)
         (output-dirty output)))))
  input)

(defn make-output [id <x]
  (let [output (object-array output-slots)]
    (aset output output-slot-id id)
    (aset output output-slot-done false)
    (aset output output-slot-prev output)
    (aset output output-slot-next output)
    (aset output output-slot-dirty output)
    (aset output output-slot-current output)
    (aset output output-slot-iterator <x)
    output))

(defn input-cancel [^objects input]
  (let [c (aget input input-slot-cancel)]
    (aset input input-slot-cancel true)
    (when (nil? c) ((aget input input-slot-notifier)))))

(defn input-change [^objects input x]
  (aset input input-slot-current x)
  (input-ready input))

(defn input-freeze [^objects input]
  (aset input input-slot-pending -1)
  (when (nil? (aget input input-slot-cancel))
    (aset input input-slot-cancel false)
    ((aget input input-slot-terminator))))

(defn update-event [^objects context k f & args]
  (if-some [event (aget context context-slot-event)]
    (aset context context-slot-event (apply update event k f args))
    (do (aset context context-slot-event (apply update empty-event k f args))
        ((aget context context-slot-notifier)))))

(defn input-check [^objects input]
  (let [^objects frame (aget input input-slot-frame)
        ^objects context (aget frame frame-slot-context)]
    (loop []
      (if-some [^objects output (aget input input-slot-dirty)]
        (let [path [(- (aget frame frame-slot-id)) (aget output output-slot-id)]]
          (aset input input-slot-dirty (aget output output-slot-dirty))
          (aset output output-slot-dirty output)
          (if (aget output output-slot-done)
            (update-event context :freeze conj path)
            (let [x @(aget output output-slot-iterator)]
              (when-not (= (aget output output-slot-current) (aset output output-slot-current x))
                (let [^objects buffer (aget context context-slot-pending-buffer)
                      wpos (aget context context-slot-pending-wpos)]
                  (if-some [t (aget output output-slot-time)]
                    (let [^objects p (aget output output-slot-prev)
                          ^objects n (aget output output-slot-next)]
                      (aset buffer t
                        (when-not (identical? p output)
                          (aset p output-slot-next n)
                          (aset n output-slot-prev p))))
                    (aswap input input-slot-pending inc))
                  (aset output output-slot-time wpos)
                  (if-some [^objects p (aget buffer wpos)]
                    (let [^objects n (aget p output-slot-next)]
                      (aset p output-slot-next output)
                      (aset n output-slot-prev output)
                      (aset output output-slot-prev p)
                      (aset output output-slot-next n))
                    (do (aset buffer wpos output)
                        (aset output output-slot-prev output)
                        (aset output output-slot-next output)))
                  (update-event context :change assoc path x)))))
          (recur))))))

(defn input-transfer [^objects input]
  (input-check input)
  (if (aget input input-slot-cancel)
    (do ((aget input input-slot-terminator))
        (throw (Cancelled.)))
    (case (aget input input-slot-pending)
      -1 (do ((aget input input-slot-terminator))
             (aget input input-slot-current))
      0 (do (aset input input-slot-cancel nil)
            (aget input input-slot-current))
      (do (aset input input-slot-cancel nil)
          pending))))

(defn make-input [^objects frame deps]
  (let [input (object-array input-slots)]
    (aset input input-slot-frame frame)
    (aset input input-slot-pending 0)
    (aset input input-slot-current pending)
    (aset input input-slot-cancel false)
    (aset input input-slot-check input)
    (reduce output-spawn input deps)))

(defn input-spawn [^objects frame deps]
  (let [slot (aswap frame frame-slot-last-input inc)]
    (m/signal! ;; inputs are cancelled when reactor is cancelled
      (fn [n t]
        (let [input (make-input frame deps)]
          (aset input input-slot-notifier n)
          (aset input input-slot-terminator t)
          (aset ^objects (aget frame frame-slot-inputs) (int slot) input)
          (n) (->It input input-cancel input-transfer))))))

(defn check-unbound-var [_debug-info <x]
  (m/latest (fn [x]
              (if (and (vector? x) (= :hyperfiddle.electric.impl.lang/unbound (first x)))
                (Failure. (error (str "Unbound electric var `" (second x) "`")))
                x)) <x))

(defn static [^objects frame slot]
  (aget ^objects (aget frame frame-slot-static) (int slot)))

(defn dynamic [^objects frame symb debug-info]
  (check-unbound-var debug-info (ca/check some? (get (aget frame frame-slot-dynamic) symb) debug-info)))

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

(defn map-vals-call [mp] (run! #(%) (vals mp)))

(defn frame-dispose [^objects f]
  (aset f frame-slot-position nil)
  (array-call (aget f frame-slot-static))
  (map-vals-call (aget f frame-slot-dynamic))
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
     (update-event (aget f frame-slot-context) :tree conj
       {:op       :rotate
        :frame    (- (aget f frame-slot-id))
        :position to})
     (frame-rotate f to))))

(defn frame-cancel [^objects f]
  (when-some [pos (aget f frame-slot-position)]
    (update-event (aget f frame-slot-context) :tree conj
      {:op       :rotate
       :frame    (- (aget f frame-slot-id))
       :position pos})
    (frame-rotate f pos)))

(defn acopy [src src-off dest dest-off size]
  #?(:clj (System/arraycopy src src-off dest dest-off size))
  #?(:cljs (dotimes [i size] (aset dest (+ dest-off i) (aget src (+ src-off i)))))
  dest)

(defn- ?grow-tier-buffer [tier pos]
  (let [^objects buf (aget tier tier-slot-buffer), cap (alength buf)]
    (if (< pos cap)
      buf
      (aset tier tier-slot-buffer
        (acopy buf 0 (object-array (bit-shift-left cap 1)) 0 cap)))))

(defn constructor [dynamic variable-count source-count constant-count target-count output-count input-count boot]
  (fn [env static]
    (fn [^objects tier id]
      (let [^objects par (aget tier tier-slot-parent)
            pos (aget tier tier-slot-size)
            ^objects buf (?grow-tier-buffer tier pos)]
        (aset tier tier-slot-size (inc pos))
        (make-frame (aget par frame-slot-context)
          tier id pos (aget tier tier-slot-foreigns) static dynamic
          variable-count source-count constant-count target-count output-count input-count
          buf (aget tier tier-slot-vars) boot (fn get-used-nodes []) :constructor env)))))

(deftype FrameIterator [f it]
  IFn
  (#?(:clj invoke :cljs -invoke) [_] (frame-cancel f))
  IDeref
  (#?(:clj deref :cljs -deref) [_] @it))

;; Takes an instruction identifying a target and a frame-constructor.
;; Return a flow instantiating the frame.
(defn constant [^objects frame ctor]
  (let [slot (aswap frame frame-slot-last-constant inc)
        ^objects consts (aget frame frame-slot-constants)
        ^objects context (aget frame frame-slot-context)]
    (aset consts slot
      (signal
        (m/watch
          (atom
            (fn [n t]
              (if-some [^objects tier (l/get-local this)]
                (let [parent (aget tier tier-slot-parent)
                      id (aswap context context-slot-local-id inc)]
                  (update-event context :tree conj
                    {:op     :create
                     :target [(- (aget frame frame-slot-id)) slot]
                     :source [(- (aget parent frame-slot-id)) (aget tier tier-slot-remote)]})
                  (let [<x (ctor tier id)
                        ^objects f (get (aget context context-slot-frame-store) id)]
                    (->FrameIterator f
                      (<x n #(do (frame-cancel f)
                                 (update-event context :tree conj {:op :remove :frame (- id)})
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

(defn variable [^objects frame vars <<x]
  (let [slot (aswap frame frame-slot-last-variable inc)
        position (+ slot (inc (aget frame frame-slot-last-source)))
        ^objects tier (aget ^objects (aget frame frame-slot-tiers) (int position))]
    (aset tier tier-slot-remote slot)
    (aset tier tier-slot-vars (atom @vars))
    (aset ^objects (aget frame frame-slot-variables) (int slot)
      (m/signal!
        (m/cp (try (let [<x (m/?< <<x)]
                     (cond (failure <x) <x
                           (nil? <x)    (Failure. (ex-info "called `new` on nil" {}))
                           :else        (m/?< (with tier <x))))
                   (catch #?(:clj Throwable :cljs :default) e
                     (Failure. e))))))))

(defn source [^objects frame vars]
  (let [slot (aswap frame frame-slot-last-source inc)
        position (+ slot (inc (aget frame frame-slot-last-variable)))]
    (aset ^objects (aget frame frame-slot-sources) (int slot)
      (doto ^objects (aget ^objects (aget frame frame-slot-tiers) (int position))
        (aset tier-slot-vars (atom @vars))))) nil)

(defn target [^objects frame ctor]
  (aset ^objects (aget frame frame-slot-targets) (int (aswap frame frame-slot-last-target inc)) ctor) nil)

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

(defn subject-at [^objects arr slot]
  (fn [!] (aset arr slot !) #(aset arr slot nil)))

(defn context-ack [^objects context]
  (let [rpos (aget context context-slot-pending-rpos)
        ^objects buffer (aget context context-slot-pending-buffer)
        ^objects output (aget buffer rpos)]
    (when (= rpos (aget context context-slot-pending-wpos))
      (throw (error "Unexpected ack.")))
    (aset context context-slot-pending-rpos
      (bit-and (unchecked-inc rpos)
        (unchecked-dec (alength buffer))))
    (aset buffer rpos nil)
    (loop [output output]
      (when-not (nil? output)
        (aset (aget output output-slot-prev) output-slot-next nil)
        (aset output output-slot-prev nil)
        (aset output output-slot-time nil)
        (let [^objects input (aget output output-slot-input)]
          (when (zero? (aswap input input-slot-pending dec))
            (input-ready input)))
        (recur (aget output output-slot-next))))))

(defn context-cancel [^objects context]
  (update-event context :cancel identity))

(defn context-transfer [^objects context]
  (loop []
    (if-some [^objects input (aget context context-slot-check)]
      (do (aset context context-slot-check (aget input input-slot-check))
          (aset input input-slot-check input)
          (input-check input)
          (recur))
      (let [event (aget context context-slot-event)]
        (when (contains? event :cancel)
          ((aget context context-slot-terminator))
          (throw (Cancelled.)))
        (when-not (= {} (:change event))
          (let [^objects buffer (aget context context-slot-pending-buffer)
                size (alength buffer)
                rpos (aget context context-slot-pending-rpos)
                wpos (aget context context-slot-pending-wpos)]
            (when (= rpos (aset context context-slot-pending-wpos
                            (bit-and (unchecked-inc wpos)
                              (unchecked-dec size))))
              (let [larger (object-array (bit-shift-left size 1))
                    split (- size rpos)]
                (acopy buffer rpos larger 0 split)
                (acopy buffer 0 larger split rpos)
                (dotimes [t size]
                  (when-some [output (aget larger t)]
                    (loop [^objects o output]
                      (aset o output-slot-time t)
                      (let [n (aget o output-slot-next)]
                        (when-not (identical? n output)
                          (recur n))))))
                (aset context context-slot-pending-buffer larger)
                (aset context context-slot-pending-wpos size)
                (aset context context-slot-pending-rpos 0)))))
        (aset context context-slot-event nil) event))))

(defn eval-tree-inst [^objects context inst]
  (case (:op inst)
    :create (let [{[target-frame target-slot] :target
                   [source-frame source-slot] :source} inst]
              ((-> context
                 (aget context-slot-frame-store)
                 ^objects (get target-frame)
                 ^objects (aget frame-slot-targets)
                 (aget target-slot))
               (-> context
                 (aget context-slot-frame-store)
                 ^objects (get source-frame)
                 ^objects (aget frame-slot-sources)
                 (aget source-slot))
               (aswap context context-slot-remote-id dec)))
    :rotate (-> context
              (aget context-slot-frame-store)
              (get (:frame inst))
              (frame-rotate (:position inst)))
    :remove (aswap context context-slot-frame-store dissoc! (:frame inst)))
  context)

(defn eval-change-inst [^objects context [id slot] value]
  (-> context
    (aget context-slot-frame-store)
    ^objects (get id)
    ^objects (aget frame-slot-inputs)
    ^objects (aget slot)
    (input-change value))
  context)

(defn eval-freeze-inst [^objects context [id slot]]
  (-> context
    (aget context-slot-frame-store)
    ^objects (get id)
    ^objects (aget frame-slot-inputs)
    ^objects (aget slot)
    (input-freeze))
  context)

(defn parse-event [^objects context {:keys [acks tree change freeze]}]
  (try (dotimes [_ acks] (context-ack context))
       (reduce eval-tree-inst context tree)
       (when-not (= {} change)
         (update-event context :acks inc)
         (reduce-kv eval-change-inst context change))
       (reduce eval-freeze-inst context freeze)
       (catch #?(:clj Throwable :cljs :default) e (#?(:clj prn :cljs js/console.error) e) (throw e))))

(defn process-incoming-events [^objects context >incoming]
  (m/sample (partial reduce parse-event context) >incoming))

(defn write-outgoing-events [write >events]
  (m/ap (let [e (m/?> >events)]
          (when-not (= e empty-event)
            (m/? (write e))))))

(defn peer [dynamic variable-count source-count constant-count target-count output-count input-count ctor get-used-nodes nm env]
  (fn rec
    ([write ?read] (rec write ?read pst))
    ([write ?read on-error]
     (m/reactor
       (let [^objects context (make-context)]
         (m/stream!
           (write-outgoing-events write
             (m/stream!
               (fn [n t]
                 (aset context context-slot-notifier n)
                 (aset context context-slot-terminator t)
                 (when-some [<main (try (make-frame context nil 0 0 {} [] dynamic
                                          variable-count source-count constant-count target-count output-count input-count
                                          context (atom {}) ctor get-used-nodes nm env)
                                        (catch #?(:clj Throwable :cljs :default) e (prn e) (throw e)))]
                   (try (m/stream! (m/latest (fn [x] (when (instance? Failure x) (on-error (.-error x)))) <main))
                        (catch #?(:clj Throwable :cljs :default) e (prn e) (throw e))))
                 (->It context context-cancel context-transfer)))))
         (m/stream! (process-incoming-events context (m/stream! (m/relieve into (m/sample vector (m/observe ?read)))))))))))

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
   :free     #{}
   :static   {}
   :dynamic  {}})

;; TODO move me
;; `new` creates a local variable and a remote source
;; `p/fn` creates a local constant and a remote target
;; Same duality with input and output, if there is 3 inputs locally, there is 3 outputs remotely.
;; There is no instruction to create inputs and outputs, they are infered from unquote-splicing.

(defn sym [& args]
  (symbol (str/join "-" args)))

(defn find-nodes [ir]
  (transduce (distinct)
    (completing (fn [ac nx] (cond-> ac (= ::ir/node (::ir/op nx)) (conj (::ir/slot nx)))))
    [] (ir-utils/->reducible ir)))

(defn remove-dep-nodes [ir]
  (walk/postwalk (fn [v] (cond-> v (::ir/deps v) (update ::ir/deps #(filterv (comp not #{::ir/node} ::ir/op) %))))
    ir))

(tests
  (remove-dep-nodes (ir/input [(ir/node 'x) (ir/output ir/nop)])) := (ir/input [(ir/output ir/nop)]))

(defn compile [prefix inst]
  (let [nodes (find-nodes inst)
        inst (remove-dep-nodes inst)
        frame (sym prefix 'frame)
        vars (sym prefix 'vars)
        ctor-at (fn [i] (sym prefix 'ctor i))
        expr-at (fn [i] (sym prefix 'expr i))
        restore-free (fn [env free]
                       (reduce-kv (fn [env i f] (assoc env f (list `aget (sym prefix 'env) i))) env free))
        capture-free (fn [env free]
                       `(doto (object-array ~(count free))
                          ~@(eduction (map-indexed (fn [i f] (list `aset i (env f)))) free)))
        emit-exprs (fn [exprs]
                     (list `fn [frame vars (sym prefix 'env)]
                       (list `let
                         (into [] (comp (map-indexed (fn [i expr] [(expr-at i) expr])) cat) (pop exprs))
                         (peek exprs))))
        update-current (fn [ctors f & args] (conj (pop ctors) (apply f (peek ctors) args)))
        from-last-expr (fn [exprs f & args] (conj exprs (apply f (expr-at (dec (count exprs))) args)))
        add-many (fn [ctors env args]
                   (reduce
                     (fn [[ctors args] arg]
                       (let [ctors (arg ctors env)]
                         [ctors (conj args (expr-at (dec (count (peek ctors)))))]))
                     [ctors []] args))]
    (-> ((fn walk [env off idx dyn inst]
           (case (::ir/op inst)
             ::ir/nop (update env :stack conj (fn [ctors _env] (update-current ctors conj nil)))
             ::ir/sub (let [p (- idx (::ir/index inst))]
                        (if (< p off)
                          (let [f (:static env)
                                i (f p (count f))]
                            (-> env
                              (update :free conj p)
                              (assoc :static (assoc f p i))
                              (update :stack conj (fn [ctors _env]
                                                    (update-current ctors conj `(static ~frame ~i))))))
                          (update env :stack conj (fn [ctors env] (update-current ctors conj (env p))))))
             ::ir/pub (-> env
                        (walk off idx dyn (::ir/init inst))
                        (walk off (inc idx) dyn (::ir/inst inst))
                        (update :stack collapse 2 (fn [form cont idx]
                                                    (fn [ctors env]
                                                      (let [ctors (form ctors env)]
                                                        (-> ctors
                                                          (update-current from-last-expr (fn [x] `(signal ~x)))
                                                          (cont (assoc env idx (expr-at (count (peek ctors)))))))))
                          idx))
             ::ir/do  (let [deps (::ir/deps inst)]
                        (-> (reduce (fn [env arg] (walk env off idx dyn arg)) env deps)
                          (update :stack collapse (count deps) vector)
                          (walk off idx dyn (::ir/inst inst))
                          (update :stack collapse 2 (fn [deps form]
                                                      (fn [ctors env]
                                                        (let [[ctors deps] (add-many ctors env deps)]
                                                          (-> ctors
                                                            (update-current conj `(make-input ~frame ~deps))
                                                            (form env))))))))
             ::ir/def (let [symb (::ir/slot inst)]
                        (-> env
                          (update :stack conj (fn [ctors _env]
                                                (update-current ctors conj `(pure (inject '~symb)))))))
             ::ir/lift (-> env
                         (walk off idx dyn (::ir/init inst))
                         (update :stack collapse 1 (fn [f]
                                                     (fn [ctors env]
                                                       (-> ctors
                                                         (f env)
                                                         (update-current from-last-expr (fn [x] `(pure ~x))))))))
             ::ir/eval (update env :stack conj (fn [ctors _env]
                                                 (update-current ctors conj `(pure ~(::ir/form inst)))))
             ::ir/node (let [symb (::ir/slot inst)]
                         (if (dyn symb)
                           (update env :stack conj (fn [ctors _env]
                                                     (update-current ctors conj `(get (deref ~vars) '~symb))))
                           (let [d (:dynamic env)
                                 i (d symb (count d))]
                             (-> env
                               (assoc :dynamic (assoc d symb i))
                               (update :stack conj
                                 (fn [ctors _env]
                                   (update-current ctors conj
                                     `(dynamic ~frame '~symb '~(assoc (select-debug-info inst)
                                                                 ::dbg/sym symb, ::dbg/slot i)))))))))
             ::ir/bind (let [v (::ir/slot inst)]
                         (-> env
                           (walk off idx (conj dyn v) (::ir/inst inst))
                           (update :stack collapse 1
                             (fn [form symb idx]
                               (fn [ctors env]
                                 (-> ctors
                                   (update-current conj `(get (deref ~vars) '~symb) `(swap! ~vars assoc '~symb ~(env idx)))
                                   (form env)
                                   (update-current conj `(swap! ~vars assoc '~symb ~(expr-at (count (peek ctors)))))
                                   (update-current (fn [exprs] (conj exprs (expr-at (- (count exprs) 2))))))))
                             v (- idx (::ir/index inst)))))
             ::ir/apply (let [f (::ir/fn inst)
                              args (::ir/args inst)]
                          (-> (reduce (fn [env inst] (walk env off idx dyn inst)) env (cons f args))
                            (update :stack collapse (inc (count args))
                              (fn [& forms]
                                (fn [ctors env]
                                  (let [[ctors forms] (add-many ctors env forms)]
                                    (update-current ctors conj
                                      `(latest-apply
                                         '~(select-debug-info (loop [f f]
                                                                (case (::ir/op f)
                                                                  ::ir/global (assoc f ::dbg/type :apply, ::dbg/name (symbol (::ir/name f)))
                                                                  ::ir/node (assoc f ::dbg/type :apply)
                                                                  ::ir/literal {::dbg/type :apply ::dbg/name (::ir/value f)}
                                                                  ::ir/eval (assoc f ::dbg/type :eval)
                                                                  ::ir/sub (assoc f ::dbg/type :apply)
                                                                  ::ir/input (assoc f ::dbg/type :apply)
                                                                  ::ir/apply (recur (::ir/fn f))
                                                                  {::dbg/type :unknown-apply, :op f})))
                                         ~@forms))))))))
             ::ir/input (let [deps (::ir/deps inst)]
                          (-> (reduce (fn [env arg] (walk env off idx dyn arg)) env deps)
                            (update :stack collapse (count deps) vector)
                            (update :input inc)
                            (update :stack collapse 1
                              (fn [deps]
                                (fn [ctors env]
                                  (let [[ctors deps] (add-many ctors env deps)]
                                    (update-current ctors conj `(input-spawn ~frame ~deps))))))))
             ::ir/output (-> env
                           (walk off idx dyn (::ir/init inst))
                           (snapshot :output)
                           (update :output inc)
                           (update :stack collapse 2
                             (fn [form slot]
                               (fn [ctors env]
                                 (-> ctors
                                   (form env)
                                   (update-current from-last-expr
                                     (fn [x] `(make-output ~slot (check-failure '~(select-debug-info inst) ~x)))))))))
             ;; ::ir/global (update env :stack conj `(pure ~(symbol (::ir/name inst))))
             ::ir/literal (update env :stack conj (fn [ctors _env]
                                                    (update-current ctors conj `(pure (quote ~(::ir/value inst))))))
             ::ir/variable (-> env
                             (walk off idx dyn (::ir/init inst))
                             (update :variable inc)
                             (update :stack collapse 1
                               (fn [form]
                                 (fn [ctors env]
                                   (-> ctors
                                     (form env)
                                     (update-current from-last-expr (fn [x] (list `variable frame vars x))))))))
             ::ir/source (-> env
                           (update :source inc)
                           (update :stack conj (fn [ctors _env]
                                                 (update-current ctors conj (list `source frame vars)))))
             ::ir/constant (-> env
                             (merge empty-frame)
                             (walk idx idx #{} (::ir/init inst))
                             (snapshot (comp vec :free))
                             (snapshot (comp reverse-index :static))
                             (snapshot (comp reverse-index :dynamic))
                             (snapshot :variable)
                             (snapshot :source)
                             (snapshot :constant)
                             (snapshot :target)
                             (snapshot :output)
                             (snapshot :input)
                             (update :free (partial into (:free env) (filter #(< % off))))
                             (merge (select-keys env (keys (dissoc empty-frame :free))))
                             (update :constant inc)
                             (update :stack collapse 10
                               (fn [form free static dynamic variable-count source-count constant-count target-count output-count input-count]
                                 (fn [ctors env]
                                   (let [exprs (peek ctors)
                                         ctors (-> (pop ctors)
                                                 (conj [])
                                                 (form (restore-free env free)))]
                                     (-> ctors
                                       (update-current from-last-expr
                                         (fn [x] `(check-failure '~(select-debug-info inst) ~x)))
                                       (update-current
                                         (fn [exprs]
                                           (list `constructor (list 'quote dynamic)
                                             variable-count source-count
                                             constant-count target-count
                                             output-count input-count
                                             (emit-exprs exprs))))
                                       (conj exprs)
                                       (update-current conj
                                         (list `constant frame
                                           (list (ctor-at (dec (count ctors)))
                                             (capture-free env free) (mapv env static))))))))))
             ::ir/target (let [deps (::ir/deps inst)]
                           (-> (reduce (fn [env inst] (walk env idx idx #{} inst))
                                 (merge env empty-frame) deps)
                             (update :stack collapse (count deps) vector)
                             (snapshot (comp vec :free))
                             (snapshot (comp reverse-index :static))
                             (snapshot (comp reverse-index :dynamic))
                             (snapshot :variable)
                             (snapshot :source)
                             (snapshot :constant)
                             (snapshot :target)
                             (snapshot :output)
                             (snapshot :input)
                             (update :free (partial into (:free env) (filter #(< % off))))
                             (merge (select-keys env (keys (dissoc empty-frame :free))))
                             (update :target inc)
                             (update :stack collapse 10
                               (fn [deps free static dynamic variable-count source-count constant-count target-count output-count input-count]
                                 (fn [ctors env]
                                   (let [exprs (peek ctors)
                                         [ctors deps] (-> (pop ctors)
                                                        (conj [])
                                                        (add-many (restore-free env free) deps))]
                                     (-> ctors
                                       (update-current conj `(make-input ~frame ~deps))
                                       (update-current
                                         (fn [exprs]
                                           (list `constructor (list 'quote dynamic)
                                             variable-count source-count
                                             constant-count target-count
                                             output-count input-count
                                             (emit-exprs exprs))))
                                       (conj exprs)
                                       (update-current conj
                                         (list `target frame
                                           (list (ctor-at (dec (count ctors)))
                                             (capture-free env free) (mapv env static)))))))))))
             (throw (ex-info (str "unknown instruction: " inst) {:inst inst}))))
         empty-frame 0 0 #{} inst)
      (snapshot (comp reverse-index :dynamic))
      (snapshot :variable)
      (snapshot :source)
      (snapshot :constant)
      (snapshot :target)
      (snapshot :output)
      (snapshot :input)
      (:stack)
      (collapse 8
        (fn [form dynamic nvariable nsource nconstant ntarget noutput ninput]
          {:fn (let [ctors (form [[]] {})]
                 (list `let (into [] (comp (map-indexed (fn [i ctor] [(ctor-at i) ctor])) cat) (pop ctors))
                   (emit-exprs (peek ctors)))) #_`(fn [~frame ~vars] ~form)
           :dynamic `'~dynamic, :nvariable nvariable :nsource nsource,
           :get-used-nodes `(fn [] ~nodes)
           :nconstant nconstant, :ntarget ntarget, :noutput noutput, :ninput ninput}))
      (peek))))

(defn- get-used-nodes-recursively [info]
  (loop [walked #{}, unwalked (seq [info])]
    (if-some [[to-walk & unwalked] unwalked]
      (if (map? to-walk)                ; skip unbound nodes
        (if (walked to-walk)
          (recur walked unwalked)
          (recur (conj walked to-walk) (into unwalked ((:get-used-nodes to-walk)))))
        (recur walked unwalked))
      walked)))

(defn main [info]
  (let [info (cond-> info (var? info) deref)
        all-nodes (get-used-nodes-recursively info)
        {:keys [nvariable nsource nconstant ntarget noutput ninput]}
        (apply merge-with +
          (eduction (map #(select-keys % [:nvariable :nsource :nconstant :ntarget :noutput :ninput]))
            all-nodes))]
    (peer (:dynamic info) nvariable nsource nconstant ntarget noutput ninput (:fn info) (:get-used-nodes info) (:var-name info) nil)))

;; used indirectly in compiler `analyze-case`
(defn case-default-throw [v] (throw (new #?(:clj IllegalArgumentException :cljs js/Error) (str "No matching clause: " v))))
(defn pick-case-branch [picker-map v default-branch & branches]
  (if-some [i (picker-map v)] (nth branches i) default-branch))
