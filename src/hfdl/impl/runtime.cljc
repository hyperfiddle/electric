(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval quote])
  (:require [hfdl.impl.util :as u]
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
;; 7: frame-id -> signals
;; 8: frame-id -> variables
;; 9: frame-id -> outputs

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

(defn lift-failure [<x]
  (m/cp (try (m/?< <x) (catch #?(:clj Throwable :cljs :default) e (->Failure e)))))

(defn steady [x] (lift-failure (m/watch (atom x))))

(defrecord Pending [])
(def pending (->Failure (->Pending)))

(defn input [context frame slot]
  (lift-failure (m/watch (aset ^objects (get (aget ^objects context (int 4)) frame) slot (atom pending)))))

(defn allocate [context inputs targets sources signals variables outputs ctor frame nodes]
  (let [ins (object-array inputs)
        tgts (object-array targets)
        srcs (object-array sources)
        sigs (object-array signals)
        vars (object-array variables)
        outs (object-array outputs)]
    (aset ^objects context (int 4) (assoc (aget ^objects context (int 4)) frame ins))
    (aset ^objects context (int 5) (assoc (aget ^objects context (int 5)) frame tgts))
    (aset ^objects context (int 6) (assoc (aget ^objects context (int 6)) frame srcs))
    (aset ^objects context (int 7) (assoc (aget ^objects context (int 7)) frame sigs))
    (aset ^objects context (int 8) (assoc (aget ^objects context (int 8)) frame vars))
    (aset ^objects context (int 9) (assoc (aget ^objects context (int 9)) frame outs))
    (let [flow (ctor frame nodes)]
      ((aget ^objects context (int 1))
       (m/ap
         (loop [slot 0]
           (if (< slot outputs)
             (m/amb= [[] #{} {[frame slot] (m/?> (aget outs slot))}]
               (recur (inc slot))) (m/amb))))) flow)))

(defn target [inputs targets sources signals variables outputs ctor context frame slot]
  (aset ^objects (get (aget ^objects context (int 5)) frame) slot
    #(allocate context inputs targets sources signals variables outputs ctor
       (aset ^objects context (int 3) (dec (aget ^objects context (int 3)))) %)))

(defn source [nodes context frame slot]
  (aset ^objects (get (aget ^objects context (int 6)) frame) slot nodes))

(defn publish [flow context frame slot]
  (lift-failure (aset ^objects (get (aget ^objects context (int 7)) frame) slot (m/signal! flow))))

(defn output [flow context frame slot]
  (aset ^objects (get (aget context (int 9)) frame) slot (m/stream! flow)))

(defn discard [^objects context frame]
  (let [^objects sigs (get (aget context (int 7)) frame)
        ^objects vars (get (aget context (int 8)) frame)
        ^objects outs (get (aget context (int 9)) frame)]
    (dotimes [i (alength sigs)] ((aget sigs i)))
    (dotimes [i (alength vars)] ((aget vars i)))
    (dotimes [i (alength outs)] ((aget outs i))))
  (aset context (int 4) (dissoc (aget context (int 4)) frame))
  (aset context (int 5) (dissoc (aget context (int 5)) frame))
  (aset context (int 6) (dissoc (aget context (int 6)) frame))
  (aset context (int 7) (dissoc (aget context (int 7)) frame))
  (aset context (int 8) (dissoc (aget context (int 8)) frame))
  (aset context (int 8) (dissoc (aget context (int 9)) frame)))

(def current (u/local))

(defn constant [inputs targets sources signals variables outputs ctor context frame slot]
  (fn [n t]
    (if-some [v (u/get-local current)]
      (let [cb (aget ^objects context (int 0))
            id (aset ^objects context (int 2) (inc (aget ^objects context (int 2))))]
        (cb [[(into [frame slot] (pop v))] #{} {}])
        (try ((allocate context inputs targets sources signals variables outputs ctor id (peek v))
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

(defn variable [flow nodes context frame slot]
  (aset ^objects (get (aget ^objects context (int 8)) frame) slot
    (m/signal!
      (u/bind-flow current [frame slot nodes]
        (m/cp (let [f (m/?< flow)]
                (if (failure f)
                  f (try (m/?< f)
                         (catch #?(:clj Throwable :cljs :default) e
                           (->Failure e))))))))))

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

(defn peer [inputs targets sources signals variables outputs boot]
  (fn [write ?read]
    (m/reactor
      (let [ctx (doto ^objects (object-array 10)
                  (aset (int 0) (m/mbx))
                  (aset (int 1) (m/mbx))
                  (aset (int 2) (identity 0))
                  (aset (int 3) (identity 0))
                  (aset (int 4) {0 (object-array inputs)})
                  (aset (int 5) {0 (object-array targets)})
                  (aset (int 6) {0 (object-array sources)})
                  (aset (int 7) {0 (object-array signals)})
                  (aset (int 8) {0 (object-array variables)})
                  (aset (int 9) {0 (object-array outputs)}))]
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
                    :target (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                  (u/with-local slots init (eval-inst idx (first args)))
                                  s (slot :targets)
                                  g (eval-inst idx (second args))]
                              (fn [context frame pubs nodes]
                                (target inputs targets sources signals variables outputs
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
                                   [g {:keys [inputs targets sources signals variables outputs]}]
                                   (u/with-local slots init (eval-inst (inc idx) (second args)))
                                   c (slot :constants)
                                   v (slot :variables)]
                               (fn [context frame pubs nodes]
                                 (recover
                                   #(constant inputs targets sources signals variables outputs
                                      (fn [frame nodes]
                                        (g context frame (conj pubs %) nodes))
                                      context frame c)
                                   (f context frame pubs nodes)
                                   nodes frame v)))
                    :constant (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                    (u/with-local slots init (eval-inst idx (first args)))
                                    s (slot :constants)]
                                (fn [context frame pubs _]
                                  (steady
                                    (constant inputs targets sources signals variables outputs
                                      (fn [frame nodes] (f context frame pubs nodes))
                                      context frame s))))
                    :variable (let [f (eval-inst idx (first args))
                                    v (slot :variables)]
                                (fn [context frame pubs nodes]
                                  (-> (f context frame pubs nodes)
                                    (variable nodes context frame v))))
                    (throw (ex-info (str "Unsupported operation - " op) {:op op :args args}))))]
          (let [[f {:keys [nodes inputs targets sources signals variables outputs]}]
                (u/with-local slots init (eval-inst 0 inst))]
            (peer inputs targets sources signals variables outputs
              (fn [context] (f context 0 [] (vec (repeat nodes nil)))))))))))
