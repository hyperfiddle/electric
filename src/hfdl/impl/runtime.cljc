(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval quote])
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.yield :refer [yield]]
            [missionary.core :as m]
            [hyperfiddle.dev.utils :as utils])
  (:import missionary.Cancelled
           #?(:clj (clojure.lang IFn IDeref))))

;; network protocol
;; message: [[[target-frame target-slot source-frame source-slot] ...] #{frame ...} {[frame slot] value ...}]
;; frame: zero if root, positive integer if owned by sender, negative integer if owned by receiver

;; context array
;; 0: request callback
;; 1: publish callback
;; 2: autoinc (latest local frame-id)
;; 3: autodec (latest remote frame-id)
;; 4: frame-id -> atoms
;; 5: frame-id -> targets
;; 6: frame-id -> sources
;; 7: frame-id -> destructor

(defrecord Failure [error])

(def failure (some-fn #(when (instance? Failure %) %)))

(def latest-apply
  (partial m/latest
    (fn [f & args]
      (or (apply failure f args)
        (try (apply f args)
             (catch #?(:clj Throwable :cljs :default) e
               (->Failure e)))))))

(def latest-error
  (partial m/latest
    (fn [x] (if (instance? Failure x) x (->Failure x)))))

(def latest-first
  (partial m/latest
    (fn [x y] (if (instance? Failure y) y x))))

(def lift-cancelled
  (partial comp
    (fn [it]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        IDeref
        (#?(:clj deref :cljs -deref) [_]
          (try @it (catch Cancelled e (->Failure e))))))))

(defn steady [x] (m/watch (atom x)))

(defrecord Pending [])
(def pending (->Failure (->Pending)))

(def current (u/local))

(defn discard [^objects context frame]
  ((get (aget context (int 7)) frame))
  (aset context (int 4) (dissoc (aget context (int 4)) frame))
  (aset context (int 5) (dissoc (aget context (int 5)) frame))
  (aset context (int 6) (dissoc (aget context (int 6)) frame))
  (aset context (int 7) (dissoc (aget context (int 7)) frame)))

(deftype Builder [ctx frame ats tgts srcs sigs vars outs])
(def builder (u/local))

(defn allocate [context inputs targets sources signals variables outputs ctor frame nodes]
  (let [prev (u/get-local builder)
        ats (object-array inputs)
        tgts (object-array targets)
        srcs (object-array sources)
        sigs (object-array signals)
        vars (object-array variables)
        outs (object-array outputs)]
    (aset ^objects context (int 4) (assoc (aget ^objects context (int 4)) frame ats))
    (aset ^objects context (int 5) (assoc (aget ^objects context (int 5)) frame tgts))
    (aset ^objects context (int 6) (assoc (aget ^objects context (int 6)) frame srcs))
    (aset ^objects context (int 7)
      (assoc (aget ^objects context (int 7))
        frame #(do (dotimes [i signals] ((aget sigs i)))
                   (dotimes [i variables] ((aget vars i)))
                   (dotimes [i outputs] ((aget outs i))))))
    (u/set-local builder (->Builder context frame ats tgts srcs sigs vars outs))
    (try (ctor nodes)
         (finally
           (u/set-local builder prev)
           ((aget ^objects context (int 1))
            (m/ap
              (loop [slot 0]
                (if (< slot outputs)
                  (m/amb= [[] #{} {[frame slot] (m/?> (aget outs slot))}]
                    (recur (inc slot))) (m/amb)))))))))

(defn input [slot]
  (when-some [^Builder b (u/get-local builder)]
    (m/signal! (lift-cancelled (m/watch (aset (.-ats b) slot (atom pending))))))) ;; TODO cleanup

(defn target [slot inputs targets sources signals variables outputs ctor]
  (when-some [^Builder b (u/get-local builder)]
    (aset ^objects (.-tgts b) slot
      #(let [context (.-ctx b)]
         (allocate context inputs targets sources signals variables outputs ctor
           (aset ^objects context (int 3) (dec (aget ^objects context (int 3)))) %)))))

(defn source [slot nodes]
  (when-some [^Builder b (u/get-local builder)]
    (aset ^objects (.-srcs b) slot nodes)))

(defn signal [slot flow]
  (when-some [^Builder b (u/get-local builder)]
    (aset ^objects (.-sigs b) slot (m/signal! (lift-cancelled flow)))))

(defn variable [slot nodes <<x]
  (when-some [^Builder b (u/get-local builder)]
    (aset ^objects (.-vars b) slot
      (m/signal!
        (u/bind-flow current [(.-frame b) slot nodes]
          (m/cp (try (let [<x (m/?< <<x)]
                       (if (failure <x)
                         <x (m/?< <x)))
                     (catch Cancelled e
                       (->Failure e)))))))))

(defn output [slot flow]
  (when-some [^Builder b (u/get-local builder)]
    (aset ^objects (.-outs b) slot (m/stream! (lift-cancelled flow)))))

(defn constant [slot inputs targets sources signals variables outputs ctor]
  (when-some [^Builder b (u/get-local builder)]
    (fn [n t]
      (if-some [v (u/get-local current)]
        (let [context (.-ctx b)
              frame (.-frame b)
              cb (aget ^objects context (int 0))
              id (aset ^objects context (int 2) (inc (aget ^objects context (int 2))))]
          (cb [[(into [frame slot] (pop v))] #{} {}])
          (try ((allocate context inputs targets sources signals variables outputs ctor id (peek v))
                #(let [p (u/get-local current)]
                   (u/set-local current v) (n) (u/set-local current p))
                #(do (discard context id) (cb [[] #{id} {}]) (t)))
               (catch #?(:clj Throwable :cljs :default) e (u/failer e n t))))
        (u/failer (ex-info "Unable to build frame : not in peer context." {}) n t)))))

(defn recover [slot nodes flow fallback]
  (when-some [^Builder b (u/get-local builder)]
    (let [v [(.-frame b) slot nodes]]
      (yield #(when (instance? Failure %)
                (u/bind-flow current v
                  (fallback (steady (:error %))))) flow))))

(defn capture [& slots]
  (steady
    (fn [flow & args]
      (let [m (zipmap slots args)]
        (fn [n t]
          (if-some [v (u/get-local current)]
            (do (u/set-local current
                  (conj (pop v)
                    (reduce-kv (fn [nodes slot flow]
                                 (assoc nodes slot (m/signal! (lift-cancelled flow)))) ;; TODO cleanup
                      (peek v) m)))
              (try (flow n t) (finally (u/set-local current v))))
            (u/failer (ex-info "Unable to bind : not in peer context." {}) n t)))))))

(defn create [context [target-frame target-slot source-frame source-slot]]
  (if-some [ctors (get (aget ^objects context (int 5)) (- target-frame))]
    (if-some [nodes (get (aget ^objects context (int 6)) (- source-frame))]
      ((aget ^objects ctors target-slot) (aget ^objects nodes source-slot))
      (utils/warn "create on dead source frame :" (- target-frame) target-slot (- source-frame) source-slot))
    (utils/warn "create on dead target frame :" (- target-frame) target-slot (- source-frame) source-slot))
  context)

(defn cancel [context frame]
  (discard context (- frame))
  context)

(defn change [context [frame slot] value]
  (if-some [inputs (get (aget ^objects context (int 4)) (- frame))]
    (reset! (aget ^objects inputs slot) value)
    (utils/warn "change on dead frame :" (- frame) slot value))
  context)

(defn peer [nodes inputs targets sources signals variables outputs boot]
  (fn [write ?read]
    (m/reactor
      (let [ctx (doto ^objects (object-array 8)
                  (aset (int 0) (m/mbx))
                  (aset (int 1) (m/mbx))
                  (aset (int 2) (identity 0))
                  (aset (int 3) (identity 0))
                  (aset (int 4) {})
                  (aset (int 5) {})
                  (aset (int 6) {})
                  (aset (int 7) {}))]
        (m/stream! (allocate ctx inputs targets sources signals variables outputs boot 0 (vec (repeat nodes nil))))
        (->> (u/poll ?read)
          (m/stream!)
          (m/eduction
            (map (fn [[adds rets mods]]
                   (reduce create ctx adds)
                   (reduce cancel ctx rets)
                   (reduce-kv change ctx mods))))
          (m/stream!))
        (->> (m/ap (m/amb=
                     (loop []
                       (m/amb (m/? (aget ctx (int 0))) (recur)))
                     (loop []
                       (let [>x (m/? (aget ctx (int 1)))]
                         (m/amb= (m/?> >x) (recur))))))
          (m/relieve (partial map into))
          (m/stream!)
          (u/foreach write)
          (m/stream!))))))

(defn juxt-with
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

(defn missing-exports [env program]
  (map symbol (remove env (globals program))))

(def eval
  (let [slots (u/local)
        init {:nodes 0
              :inputs 0
              :targets 0
              :sources 0
              :signals 0
              :outputs 0
              :constants 0
              :variables 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))
            (node [s]
              (u/set-local slots (update (u/get-local slots) :nodes max s)) s)]
      (fn [resolve inst]
        (letfn [(eval-inst [idx [op & args]]
                  (case op
                    :nop (fn [_ _])
                    :sub (let [i (- idx (first args))]
                           (fn [pubs _] (nth pubs i)))
                    :pub (let [f (eval-inst idx (first args))
                               s (slot :signals)
                               g (eval-inst (inc idx) (second args))]
                           (fn [pubs nodes]
                             (g (conj pubs (signal s (f pubs nodes))) nodes)))
                    :def (let [ns (mapv node args)]
                           (fn [_ _] (apply capture ns)))
                    :node (let [n (node (first args))]
                            (fn [_ nodes] (nth nodes n)))
                    :bind (let [n (node (first args))
                                [f g] (mapv (partial eval-inst idx) (next args))]
                            (fn [pubs nodes]
                              (g pubs (assoc nodes n (f pubs nodes)))))
                    :apply (apply juxt-with latest-apply (mapv (partial eval-inst idx) args))
                    :error (apply juxt-with latest-error (mapv (partial eval-inst idx) args))
                    :first (apply juxt-with latest-first (mapv (partial eval-inst idx) args))
                    :input (let [s (slot :inputs)]
                             (fn [_ _] (input s)))
                    :output (let [f (eval-inst idx (first args))
                                  s (slot :outputs)
                                  g (eval-inst idx (second args))]
                              (fn [pubs nodes]
                                (output s (f pubs nodes))
                                (g pubs nodes)))
                    :target (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                  (u/with-local slots init (eval-inst idx (first args)))
                                  s (slot :targets)
                                  g (eval-inst idx (second args))]
                              (fn [pubs nodes]
                                (target s inputs targets sources signals variables outputs
                                  (fn [nodes] (f pubs nodes)))
                                (g pubs nodes)))
                    :source (let [s (slot :sources)
                                  f (eval-inst idx (first args))]
                              (fn [pubs nodes]
                                (source s nodes)
                                (f pubs nodes)))
                    :global (if (contains? resolve (first args))
                              (constantly (steady (get resolve (first args))))
                              (throw (ex-info (str "Unable to resolve - "
                                                   (symbol (first args)))
                                              {:missing-exports (missing-exports resolve inst)})))
                    :literal (constantly (steady (first args)))
                    :recover (let [f (eval-inst idx (first args))
                                   [g {:keys [inputs targets sources signals variables outputs]}]
                                   (u/with-local slots init (eval-inst (inc idx) (second args)))
                                   c (slot :constants)
                                   v (slot :variables)]
                               (fn [context frame pubs nodes]
                                 (recover v nodes (f context frame pubs nodes)
                                   #(constant c inputs targets sources signals variables outputs
                                      (fn [nodes] (g (conj pubs %) nodes))))))
                    :constant (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                    (u/with-local slots init (eval-inst idx (first args)))
                                    s (slot :constants)]
                                (fn [pubs _]
                                  (steady
                                    (constant s inputs targets sources signals variables outputs
                                      (fn [nodes] (f pubs nodes))))))
                    :variable (let [f (eval-inst idx (first args))
                                    v (slot :variables)]
                                (fn [pubs nodes]
                                  (variable v nodes (f pubs nodes))))
                    (throw (ex-info (str "Unsupported operation - " op) {:op op :args args}))))]
          (let [[f {:keys [nodes inputs targets sources signals variables outputs]}]
                (u/with-local slots init (eval-inst 0 inst))]
            (peer nodes inputs targets sources signals variables outputs
              (fn [nodes] (f [] nodes)))))))))
