(ns ^:no-doc hyperfiddle.photon-impl.runtime
  (:refer-clojure :exclude [eval])
  (:require [hyperfiddle.photon-impl.yield :refer [yield]]
            [hyperfiddle.photon-impl.local :as l]
            [missionary.core :as m]
            [hyperfiddle.dev.logger :as log]
            [hyperfiddle.rcf :refer [tests]])
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

(deftype Failure [error])

(def failure (some-fn #(when (instance? Failure %) %)))

(def latest-apply
  (partial m/latest
    (fn [f & args]
      (or (apply failure f args)
        (try (apply f args)
             (catch #?(:clj Throwable :cljs :default) e
               (->Failure e)))))))

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
          (try @it (catch Cancelled e (->Failure e))))))))

;; TODO move out of impl ns
(deftype Pending [])
(deftype Remote [])

(def cancelled (->Failure (Cancelled.)))
(def pending (->Failure (Pending.)))
(def remote (->Failure (Remote.)))

(def current (l/local))

(defn discard [^objects context frame]
  ((get (aget context (int 7)) frame))
  (aset context (int 4) (dissoc (aget context (int 4)) frame))
  (aset context (int 5) (dissoc (aget context (int 5)) frame))
  (aset context (int 6) (dissoc (aget context (int 6)) frame))
  (aset context (int 7) (dissoc (aget context (int 7)) frame)))

(defn read-change [value]
  (case value
    :hyperfiddle.photon/remote    remote
    :hyperfiddle.photon/pending   pending
    :hyperfiddle.photon/cancelled cancelled
    value))

(defn write-change [log value]
  (if (failure value)
    (let [e (.-error ^Failure value)]
      (if (instance? Cancelled e)
        :hyperfiddle.photon/cancelled
        (if (instance? Pending e)
          :hyperfiddle.photon/pending
          (do (log e)
              :hyperfiddle.photon/remote))))
    value))

(deftype Builder [log ctx frame ats tgts srcs sigs vars outs])
(def builder (l/local))

(defn allocate [log context inputs targets sources signals variables outputs ctor frame nodes]
  (let [out (aget ^objects context (int 1))
        prev (l/get-local builder)
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
    (l/set-local builder (->Builder log context frame ats tgts srcs sigs vars outs))
    (try (ctor nodes)
         (finally
           (l/set-local builder prev)
           (out (m/ap
                  (loop [slot 0]
                    (if (< slot outputs)
                      (m/amb= [[] #{} {[frame slot] (write-change log (m/?> (aget outs slot)))}]
                        (recur (inc slot))) (m/amb)))))))))

(defn input [slot]
  (when-some [^Builder b (l/get-local builder)]
    (m/signal! (lift-cancelled (m/watch (aset (.-ats b) slot (atom pending))))))) ;; TODO cleanup

(defn target [slot inputs targets sources signals variables outputs ctor]
  (when-some [^Builder b (l/get-local builder)]
    (aset ^objects (.-tgts b) slot
      (let [log (.-log b)
            context (.-ctx b)]
        #(allocate log context inputs targets sources signals variables outputs ctor
           (aset ^objects context (int 3) (dec (aget ^objects context (int 3)))) %)))))

(defn source [slot nodes]
  (when-some [^Builder b (l/get-local builder)]
    (aset ^objects (.-srcs b) slot nodes)))

(defn signal [slot flow]
  (when-some [^Builder b (l/get-local builder)]
    (aset ^objects (.-sigs b) slot (m/signal! (lift-cancelled flow)))))

(defn variable [slot nodes <<x]
  (when-some [^Builder b (l/get-local builder)]
    (aset ^objects (.-vars b) slot
      (m/signal!
        (bind-flow current [(.-frame b) slot nodes]
          (m/cp (try (let [<x (m/?< <<x)]
                       (if (failure <x)
                         <x (m/?< <x)))
                     (catch Cancelled e
                       (->Failure e)))))))))

(defn output [slot flow]
  (when-some [^Builder b (l/get-local builder)]
    (aset ^objects (.-outs b) slot (m/stream! (lift-cancelled flow)))))

(defn failer [e n t]
  (n) (reify
        IFn (#?(:clj invoke :cljs -invoke) [_])
        IDeref (#?(:clj deref :cljs -deref) [_] (t) (throw e))))

(defn constant [slot inputs targets sources signals variables outputs ctor]
  (when-some [^Builder b (l/get-local builder)]
    (fn [n t]
      (if-some [v (l/get-local current)]
        (let [log (.-log b)
              context (.-ctx b)
              frame (.-frame b)
              cb (aget ^objects context (int 0))
              id (aset ^objects context (int 2) (inc (aget ^objects context (int 2))))]
          (cb [[(into [frame slot] (pop v))] #{} {}])
          (try ((allocate log context inputs targets sources signals variables outputs ctor id (peek v))
                #(let [p (l/get-local current)]
                   (l/set-local current v) (n) (l/set-local current p))
                #(do (discard context id) (cb [[] #{id} {}]) (t)))
               (catch #?(:clj Throwable :cljs :default) e (failer e n t))))
        (failer (ex-info "Unable to build frame : not in peer context." {}) n t)))))

(defn capture [& slots]
  (steady
    (fn [flow & args]
      (let [m (zipmap slots args)]
        (fn [n t]
          (if-some [v (l/get-local current)]
            (do (l/set-local current
                  (conj (pop v)
                    (reduce-kv (fn [nodes slot flow]
                                 (assoc nodes slot (m/signal! (lift-cancelled flow)))) ;; TODO cleanup
                      (peek v) m)))
              (try (flow n t) (finally (l/set-local current v))))
            (failer (ex-info "Unable to bind : not in peer context." {}) n t)))))))

(defn create [context [target-frame target-slot source-frame source-slot]]
  (if-some [ctors (get (aget ^objects context (int 5)) (- target-frame))]
    (if-some [nodes (get (aget ^objects context (int 6)) (- source-frame))]
      ((aget ^objects ctors target-slot) (aget ^objects nodes source-slot))
      (log/warn "create on dead source frame :" (- target-frame) target-slot (- source-frame) source-slot))
    (log/warn "create on dead target frame :" (- target-frame) target-slot (- source-frame) source-slot))
  context)

(defn cancel [context frame]
  (discard context (- frame))
  context)

(defn change [context [frame slot] value]
  (if-some [inputs (get (aget ^objects context (int 4)) (- frame))]
    (reset! (aget ^objects inputs slot) (read-change value))
    (log/warn "change on dead frame :" (- frame) slot value))
  context)

(defn foreach [f input]
  (m/ap (m/? (f (m/?> input)))))

(defn poll [task]
  (m/ap (m/? (m/?> (m/seed (repeat task))))))

(defn pst [e]
  #?(:clj (.printStackTrace ^Throwable e)
     :cljs (js/console.error e)))

(defn peer [nodes inputs targets sources signals variables outputs boot]
  (fn p
    ([write ?read] (p write ?read pst))
    ([write ?read log]
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
         (m/stream! (allocate log ctx inputs targets sources signals variables outputs boot 0 (vec (repeat nodes nil))))
         (->> (poll ?read)
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
           (foreach write)
           (m/stream!)))))))

(def emit
  (let [slots (l/local)
        init {:nodes 0
              :inputs 0
              :targets 0
              :sources 0
              :signals 0
              :outputs 0
              :constants 0
              :variables 0}]
    (letfn [(slot [k]
              (let [m (l/get-local slots), n (get m k)]
                (l/set-local slots (assoc m k (inc n))) n))
            (node [s]
              (l/set-local slots (update (l/get-local slots) :nodes max (inc s))) s)
            (emit-inst [sym pub [op & args]]
              (case op
                :nop nil
                :sub (let [i (- pub (first args))]
                       (sym 'pub i))
                :pub (let [f (emit-inst sym pub (first args))
                           s (slot :signals)]
                       `(let [~(sym 'pub pub) (signal ~s ~f)]
                          ~(emit-inst sym (inc pub) (second args))))
                :def (cons `capture (mapv node args))
                :node (list `nth (sym 'nodes) (node (first args)))
                :bind (let [[slot inst cont] args]
                        (list `let [(sym 'nodes) (list `assoc (sym 'nodes) (node slot) (emit-inst sym pub inst))]
                          (emit-inst sym pub cont)))
                :apply `(latest-apply ~@(mapv (partial emit-inst sym pub) args))
                :input (let [i (slot :inputs)] `(input ~i))
                :output (let [f (emit-inst sym pub (first args))
                              o (slot :outputs)
                              g (emit-inst sym pub (second args))]
                          `(do (output ~o ~f) ~g))
                :target (let [[frame {:keys [inputs targets sources signals variables outputs]}]
                              (l/with-local slots init (emit-inst sym pub (first args)))
                              t (slot :targets)
                              g (emit-inst sym pub (second args))]
                          `(do (target ~t ~inputs ~targets ~sources ~signals ~variables ~outputs
                                 (fn [~(sym 'nodes)] ~frame)) ~g))
                :source (let [s (slot :sources)
                              f (emit-inst sym pub (first args))]
                          `(do (source ~s ~(sym 'nodes)) ~f))
                :global `(steady ~(symbol (first args)))
                :literal `(steady (quote ~(first args)))
                :interop `(latest-apply
                            ~(let [arg-syms (into [] (map sym) (range (count (next args))))]
                               (list `fn arg-syms (apply (first args) arg-syms)))
                            ~(mapv (partial emit-inst sym pub) (next args)))
                :constant (let [[frame {:keys [inputs targets sources signals variables outputs]}]
                                (l/with-local slots init (emit-inst sym pub (first args)))
                                c (slot :constants)]
                            `(steady (constant ~c ~inputs ~targets ~sources ~signals ~variables ~outputs
                                       (fn [~(sym 'nodes)] ~frame))))
                :variable (let [form (emit-inst sym pub (first args))
                                v (slot :variables)]
                            `(variable ~v ~(sym 'nodes) ~form))))]
      (fn [sym inst]
        (let [[form {:keys [nodes inputs targets sources signals variables outputs]}]
              (l/with-local slots init (emit-inst sym 0 inst))]
          `(peer ~nodes ~inputs ~targets ~sources ~signals ~variables ~outputs
             (fn [~(sym 'nodes)] ~form)))))))

(tests
  (emit (comp symbol str) [:literal 5]) :=
  `(peer 0 0 0 0 0 0 0 (fn [~'nodes] (steady '5)))

  (emit (comp symbol str)
    [:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]) :=
  `(peer 0 0 0 0 0 0 0
     (fn [~'nodes]
       (latest-apply (steady +) (steady '2) (steady '3))))

  (emit (comp symbol str)
    [:pub [:literal 1]
     [:apply [:global :clojure.core/+]
      [:sub 1] [:literal 2]]]) :=
  `(peer 0 0 0 0 1 0 0
     (fn [~'nodes]
       (let [~'pub0 (signal 0 (steady '1))]
         (latest-apply (steady +) ~'pub0 (steady '2)))))

  (emit (comp symbol str)
    [:variable [:global :missionary.core/none]]) :=
  `(peer 0 0 0 0 0 1 0
     (fn [~'nodes] (variable 0 ~'nodes (steady m/none))))

  (emit (comp symbol str) [:input]) :=
  `(peer 0 1 0 0 0 0 0
     (fn [~'nodes] (input 0)))

  (emit (comp symbol str) [:constant [:literal :foo]]) :=
  `(peer 0 0 0 0 0 0 0
     (fn [~'nodes]
       (steady
         (constant 0 0 0 0 0 0 0
           (fn [~'nodes] (steady ':foo))))))

  (emit (comp symbol str)
    [:variable
     [:pub [:constant [:literal 3]]
      [:pub [:constant [:input]]
       [:apply [:apply [:global :clojure.core/hash-map]
                [:literal 2] [:sub 2]
                [:literal 4] [:sub 1]
                [:literal 5] [:sub 1]]
        [:literal 1]
        [:constant [:literal 7]]]]]]) :=
  `(peer 0 0 0 0 2 1 0
     (fn [~'nodes]
       (variable 0 ~'nodes
         (let [~'pub0 (signal 0
                        (steady
                          (constant 0 0 0 0 0 0 0
                            (fn [~'nodes] (steady '3)))))]
           (let [~'pub1 (signal 1
                          (steady
                            (constant 1 1 0 0 0 0 0
                              (fn [~'nodes] (input 0)))))]
             (latest-apply
               (latest-apply (steady hash-map)
                 (steady '2) ~'pub0
                 (steady '4) ~'pub1
                 (steady '5) ~'pub1)
               (steady '1)
               (steady
                 (constant 2 0 0 0 0 0 0
                   (fn [~'nodes] (steady '7))))))))))

  (emit (comp symbol str) [:def 0]) :=
  `(peer 1 0 0 0 0 0 0 (fn [~'nodes] (capture 0)))

  (emit (comp symbol str)
    [:pub [:literal nil]
     [:constant [:sub 1]]]) :=
  `(peer 0 0 0 0 1 0 0
     (fn [~'nodes]
       (let [~'pub0 (signal 0 (steady 'nil))]
         (steady (constant 0 0 0 0 0 0 0 (fn [~'nodes] ~'pub0)))))))

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

(defn missing-exports [resolvef program]
  (->> (globals program)
    (eduction (map (juxt (partial resolvef ::not-found) identity))
      (filter #(= ::not-found (first %)))
      (map second)
      (map symbol))))

(def eval
  (let [slots (l/local)
        init {:nodes 0
              :inputs 0
              :targets 0
              :sources 0
              :signals 0
              :outputs 0
              :constants 0
              :variables 0}]
    (letfn [(slot [k]
              (let [m (l/get-local slots), n (get m k)]
                (l/set-local slots (assoc m k (inc n))) n))
            (node [s]
              (l/set-local slots (update (l/get-local slots) :nodes max s)) s)]
      (fn [resolvef inst]
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
                    :input (let [s (slot :inputs)]
                             (fn [_ _] (input s)))
                    :output (let [f (eval-inst idx (first args))
                                  s (slot :outputs)
                                  g (eval-inst idx (second args))]
                              (fn [pubs nodes]
                                (output s (f pubs nodes))
                                (g pubs nodes)))
                    :target (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                  (l/with-local slots init (eval-inst idx (first args)))
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
                    :global (if (resolvef (first args)) ; check if exported, don’t resolve now.
                              (constantly (steady (resolvef ::not-found (first args)) ; resolve when sampled
                                            ))
                              (throw (ex-info (str "Unable to resolve - "
                                                (symbol (first args)))
                                       {:missing-exports (missing-exports resolvef inst)})))
                    :literal (constantly (steady (first args)))
                    :constant (let [[f {:keys [inputs targets sources signals variables outputs]}]
                                    (l/with-local slots init (eval-inst idx (first args)))
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
                (l/with-local slots init (eval-inst 0 inst))]
            (peer nodes inputs targets sources signals variables outputs
              (fn [nodes] (f [] nodes)))))))))
