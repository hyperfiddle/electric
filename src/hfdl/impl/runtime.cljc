(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval quote])
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.impl.yield :refer [yield]]
            [missionary.core :as m])
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
;; 4: frame-id -> inputs
;; 5: frame-id -> targets
;; 6: frame-id -> sources
;; 7: frame-id -> captures

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

(defn steady [x] (m/observe (fn [!] (! x) u/nop)))

(defrecord Pending [])
(def pending (->Failure (->Pending)))

(defrecord Stopped [])
(def stopped (->Failure (->Stopped)))

(defn subscribe [f]
  (fn [n t]
    (let [it (f n t)]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        IDeref
        (#?(:clj deref :cljs -deref) [_]
          (try @it (catch Cancelled _ stopped)))))))

(defn input [context frame slot]
  (m/watch (aset ^objects (get (aget ^objects context (int 4)) frame) slot (atom pending))))

(defn allocate [context inputs targets sources signals ctor frame nodes]
  (aset ^objects context (int 4) (assoc (aget ^objects context (int 4)) frame (object-array inputs)))
  (aset ^objects context (int 5) (assoc (aget ^objects context (int 5)) frame (object-array targets)))
  (aset ^objects context (int 6) (assoc (aget ^objects context (int 6)) frame (object-array sources)))
  (aset ^objects context (int 7) (assoc (aget ^objects context (int 7)) frame (object-array signals)))
  (ctor frame nodes))

(defn target [inputs targets sources signals ctor context frame slot]
  (aset ^objects (get (aget ^objects context (int 5)) frame) slot
    #(allocate context inputs targets sources signals ctor
       (aset ^objects context (int 3) (dec (aget ^objects context (int 3)))) %)))

(defn source [nodes context frame slot]
  (aset ^objects (get (aget ^objects context (int 6)) frame) slot nodes))

(defn publish [flow context frame slot]
  (subscribe (aset ^objects (get (aget ^objects context (int 7)) frame) slot (m/signal! flow))))

(defn output [flow context frame slot]
  ((aget ^objects context (int 1)) [frame slot flow]))

(defn discard [^objects context frame]
  (let [^objects signals (get (aget context (int 7)) frame)]
    (dotimes [i (alength signals)] ((aget signals i))))
  (aset context (int 4) (dissoc (aget context (int 4)) frame))
  (aset context (int 5) (dissoc (aget context (int 5)) frame))
  (aset context (int 6) (dissoc (aget context (int 6)) frame))
  (aset context (int 7) (dissoc (aget context (int 7)) frame)))

(def current (u/local))

(defn constant [inputs targets sources signals ctor context frame slot]
  (fn [n t]
    (if-some [v (u/get-local current)]
      (let [cb (aget ^objects context (int 0))
            id (aset ^objects context (int 2) (inc (aget ^objects context (int 2))))]
        (cb [[(into [frame slot] (pop v))] #{} {}])
        (try ((allocate context inputs targets sources signals ctor id (peek v))
              #(let [p (u/get-local current)]
                 (u/set-local current v) (n) (u/set-local current p))
              #(do (discard context id) (cb [[] #{id} {}]) (t)))
             (catch #?(:clj Throwable :cljs :default) e (u/failer e n t))))
      (u/failer (ex-info "Unable to build frame : not in peer context." {}) n t))))

(defn capture [& slots]
  (steady
    (fn [flow & args]
      (let [m (zipmap slots args)]
        (fn [n t]
          (if-some [v (u/get-local current)]
            (do (u/set-local current (conj (pop v) (reduce-kv assoc (peek v) m)))
                (try (flow n t) (finally (u/set-local current v))))
            (u/failer (ex-info "Unable to bind : not in peer context." {}) n t)))))))

(defn recover [fallback flow nodes frame slot]
  (let [v [frame slot nodes]]
    (yield #(when (instance? Failure %)
              (u/bind-flow current v
                (fallback (steady (:error %))))) flow)))

(defn variable [flow nodes frame slot]
  (let [v [frame slot nodes]]
    (->> flow
      (m/eduction (dedupe)
        (map #(u/bind-flow current v
                (if (failure %)
                  (steady %) %))))
      (switch))))

(defn create [context [target-frame target-slot source-frame source-slot]]
  (if-some [ctors (get (aget ^objects context (int 5)) (- target-frame))]
    (if-some [nodes (get (aget ^objects context (int 6)) (- source-frame))]
      ((aget ^objects ctors target-slot) (aget ^objects nodes source-slot))
      (println "create on dead source frame :" (- target-frame) target-slot (- source-frame) source-slot))
    (println "create on dead target frame :" (- target-frame) target-slot (- source-frame) source-slot))
  context)

(defn cancel [context frame]
  (discard context (- frame))
  context)

(defn change [context [frame slot] value]
  (if-some [inputs (get (aget ^objects context (int 4)) (- frame))]
    (reset! (aget ^objects inputs slot) value)
    (println "change on dead frame :" (- frame) slot value))
  context)

(defn peer [inputs targets sources signals boot]
  (fn [write ?read]
    (let [r (m/reactor
              (let [ctx (doto ^objects (object-array 8)
                          (aset (int 0) (m/mbx))
                          (aset (int 1) (m/mbx))
                          (aset (int 2) (identity 0))
                          (aset (int 3) (identity 0))
                          (aset (int 4) {0 (object-array inputs)})
                          (aset (int 5) {0 (object-array targets)})
                          (aset (int 6) {0 (object-array sources)})
                          (aset (int 7) {0 (object-array signals)}))]
                (m/stream! (boot ctx))
                (->> (u/poll ?read)
                  (m/stream!)
                  (m/eduction
                    (map (fn [[adds rets mods]]
                           (reduce create ctx adds)
                           (reduce cancel ctx rets)
                           (reduce-kv change ctx mods))))
                  (m/stream!))
                (->> (m/ap (m/amb=
                             (m/? (m/?> (m/seed (repeat (aget ctx (int 0))))))
                             (loop []
                               (let [x (m/? (aget ctx (int 1)))]
                                 (m/amb= [[] #{} {(pop x) (try (m/?> (peek x))
                                                               (catch #?(:clj  Throwable
                                                                         :cljs :default) _
                                                                 (m/?> m/none)))}] (recur))))))
                  (m/relieve (partial map into))
                  (m/stream!)
                  (u/foreach (-> write (u/log-args '>)))
                  (m/stream!))))]
      (m/sp (try (m/? r) (catch Cancelled _))))))

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
                    :nop (fn [_ _ _ _])
                    :sub (let [i (- idx (first args))]
                           (fn [_ _ pubs _] (nth pubs i)))
                    :pub (let [f (eval-inst idx (first args))
                               s (slot :signals)
                               g (eval-inst (inc idx) (second args))]
                           (fn [context frame pubs nodes]
                             (g context frame
                               (conj pubs (publish (f context frame pubs nodes)
                                            context frame s)) nodes)))
                    :def (let [ns (mapv node args)]
                           (fn [_ _ _ _] (apply capture ns)))
                    :node (let [n (node (first args))]
                            (fn [_ _ _ nodes] (nth nodes n)))
                    :bind (let [n (node (first args))
                                [f g] (mapv (partial eval-inst idx) (next args))]
                            (fn [context frame pubs nodes]
                              (g context frame pubs
                                (assoc nodes n (f context frame pubs nodes)))))
                    :apply (apply juxt-with latest-apply (mapv (partial eval-inst idx) args))
                    :error (apply juxt-with latest-error (mapv (partial eval-inst idx) args))
                    :first (apply juxt-with latest-first (mapv (partial eval-inst idx) args))
                    :input (let [s (slot :inputs)]
                             (fn [context frame _ _]
                               (input context frame s)))
                    :output (let [f (eval-inst idx (first args))
                                  s (slot :outputs)
                                  g (eval-inst idx (second args))]
                              (fn [context frame pubs nodes]
                                (output (f context frame pubs nodes) context frame s)
                                (g context frame pubs nodes)))
                    :target (let [[f {:keys [inputs targets sources signals]}]
                                  (u/with-local slots init (eval-inst idx (first args)))
                                  s (slot :targets)
                                  g (eval-inst idx (second args))]
                              (fn [context frame pubs nodes]
                                (target inputs targets sources signals
                                  (fn [frame nodes]
                                    (f context frame pubs nodes))
                                  context frame s)
                                (g context frame pubs nodes)))
                    :source (let [s (slot :sources)
                                  f (eval-inst idx (first args))]
                              (fn [context frame pubs nodes]
                                (source nodes context frame s)
                                (f context frame pubs nodes)))
                    :global (if (contains? resolve (first args))
                              (constantly (steady (get resolve (first args))))
                              (throw (ex-info (str "Unable to resolve - "
                                                   (symbol (first args)))
                                              {:missing-exports (missing-exports resolve inst)})))
                    :literal (constantly (steady (first args)))
                    :recover (let [f (eval-inst idx (first args))
                                   [g {:keys [inputs targets sources signals]}]
                                   (u/with-local slots init (eval-inst (inc idx) (second args)))
                                   c (slot :constants)
                                   v (slot :variables)]
                               (fn [context frame pubs nodes]
                                 (recover
                                   #(constant inputs targets sources signals
                                      (fn [frame nodes]
                                        (g context frame (conj pubs %) nodes))
                                      context frame c)
                                   (f context frame pubs nodes)
                                   nodes frame v)))
                    :constant (let [[f {:keys [inputs targets sources signals]}]
                                    (u/with-local slots init (eval-inst idx (first args)))
                                    s (slot :constants)]
                                (fn [context frame pubs _]
                                  (steady
                                    (constant inputs targets sources signals
                                      (fn [frame nodes] (f context frame pubs nodes))
                                      context frame s))))
                    :variable (let [f (eval-inst idx (first args))
                                    s (slot :signals)
                                    v (slot :variables)]
                                (fn [context frame pubs nodes]
                                  (-> (f context frame pubs nodes)
                                    (variable nodes frame v)
                                    (publish context frame s))))
                    (throw (ex-info (str "Unsupported operation - " op) {:op op :args args}))))]
          (let [[f {:keys [nodes inputs targets sources signals]}]
                (u/with-local slots init (eval-inst 0 inst))]
            (peer inputs targets sources signals
              (fn [context] (f context 0 [] (vec (repeat nodes nil)))))))))))
