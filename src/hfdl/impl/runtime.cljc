(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval quote])
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.impl.rfor :refer [rfor]]
            [missionary.core :as m]))

;; network protocol
;; message: [path... {path value}]
;; path: [frame-id slot-id]
;; frame-id: integer (negative if remote)

;; context array
;; 0: request callback
;; 1: publish callback
;; 2: autoinc (latest local frame-id)
;; 3: autodec (latest remote frame-id)
;; 4: frame-id -> inputs
;; 5: frame-id -> targets
;; 6: frame-id -> signals
;; 7: bindings

(defn steady [x]
  (m/observe (fn [!] (! x) u/nop)))

(defn input [context frame slot]
  (let [inputs (get (aget context (int 4)) frame)]
    (->> (fn [!] (aset inputs slot !) u/nop)
      (m/observe)
      (m/relieve {})
      (m/signal!))))

(defn spawn [context frame inputs targets signals]
  (aset context (int 4) (assoc (aget context (int 4)) frame (object-array inputs)))
  (aset context (int 5) (assoc (aget context (int 5)) frame (object-array targets)))
  (aset context (int 6) (assoc (aget context (int 6)) frame (object-array signals))))

(defn target [context frame slot inputs targets signals ctor]
  (aset (get (aget context (int 5)) frame) slot
    (fn []
      (let [frame (aset context (int 3) (dec (aget context (int 3))))]
        (spawn context frame inputs targets signals)
        (ctor frame) context))))

(defn publish [context frame slot flow]
  (aset (get (aget context (int 6)) frame) slot (m/signal! flow)))

(defn output [context frame slot flow]
  ((aget context (int 1)) [(- frame) slot flow]))

(defn discard [context frame]
  (let [signals (get (aget context (int 6)) frame)]
    (dotimes [i (alength signals)] ((aget signals i))))
  (doto context
    (aset (int 4) (dissoc (aget context (int 4)) frame))
    (aset (int 5) (dissoc (aget context (int 5)) frame))
    (aset (int 6) (dissoc (aget context (int 6)) frame))))

(defn constant [context frame slot inputs targets signals ctor]
  (let [req (aget context (int 0))]
    (steady
      (fn [n t]
        (req [(- frame) slot])
        (let [frame (aset context (int 2) (inc (aget context (int 2))))]
          (spawn context frame inputs targets signals)
          ((ctor frame) n #(do (discard context frame)
                               (req [(- frame) -1]) (t))))))))

(defn get-node [context slot]
  (get (aget context (int 7)) slot))

(defn set-node [context slot value]
  (aset context (int 7) (assoc (aget context (int 7)) slot value)))

(defn capture [context & slots]
  (let [curr (aget context (int 7))]
    (fn [flow & args]
      (let [curr (reduce (partial apply assoc)
                   curr (map vector slots args))]
        (fn [n t]
          (let [prev (aget context (int 7))]
            (aset context (int 7) curr)
            (try (flow n t)
                 (finally (aset context (int 7) prev)))))))))

(defn variable [context frame slot flow]
  (->> flow
    (m/stream!) ;; TODO cancel this one
    (m/eduction (dedupe) (map (capture context)))
    (switch)
    (publish context frame slot)))

(defn message
  ([] [{}])
  ([x] x)
  ([x y]
   (-> (pop x)
     (into (pop y))
     (conj (merge (peek x) (peek y)))))
  ([x y & zs]
   (reduce message (message x y) zs)))

(defn change [context [frame slot] value]
  (if-some [inputs (get (aget context (int 4)) frame)]
    ((aget inputs slot) value)
    (println "input on dead frame :" frame slot value))
  context)

(defn handle [context [frame slot]]
  (case slot
    -1 (discard context frame)
    ((-> context
       (aget (int 5))
       (get frame)
       (aget slot)))))

(defn peer [nodes inputs targets signals boot]
  (fn [write >read]
    (m/reactor
      (let [ctx (doto (object-array 8)
                  (aset (int 0) (m/mbx))
                  (aset (int 1) (m/mbx))
                  (aset (int 2) 0)
                  (aset (int 3) 0)
                  (aset (int 4) {0 (object-array inputs)})
                  (aset (int 5) {0 (object-array targets)})
                  (aset (int 6) {0 (object-array signals)})
                  (aset (int 7) (vec (repeat nodes nil))))]
        (m/stream! (boot ctx))
        (->> >read
          (m/stream!)
          (m/eduction
            (map (fn [x]
                   (reduce handle ctx (pop x))
                   (reduce-kv change ctx (peek x)))))
          (m/stream!))
        (->> (m/ap (m/amb=
                     (loop []
                       (let [x (m/? (aget ctx (int 0)))]
                         (m/amb= [x {}] (recur))))
                     (loop []
                       (let [x (m/? (aget ctx (int 1)))]
                         (m/amb= [{(pop x) (try (m/?> (peek x))
                                                (catch #?(:clj  Throwable
                                                          :cljs :default) _
                                                  (m/?> m/none)))}] (recur))))))
          (m/relieve message)
          (m/stream!)
          (u/foreach (-> write (u/log-args '>)))
          (m/stream!))))))

(def eval
  (let [slots (u/local)
        init {:input 0 :target 0 :signal 0 :output 0 :constant 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))]
      (fn [resolve insts]
        (letfn [(eval-inst [[op & args]]
                  (case op
                    :sub (let [[idx] args]
                           (fn [_ _ pubs] (nth pubs (- (count pubs) idx))))
                    :pub (let [[f g] (mapv eval-inst args)
                               s (slot :signal)]
                           (fn [context frame pubs]
                             (g context frame (conj pubs (publish context frame s (f context frame pubs))))))
                    :def (fn [context _ _] (steady (apply capture context args)))
                    :node (let [n (first args)]
                            (fn [context _ _] (get-node context n)))
                    :apply (comp (partial apply m/latest u/call)
                             (apply juxt (map eval-inst args)))
                    :input (let [f (if-some [args (seq args)]
                                     (apply juxt (map eval-inst args))
                                     (constantly []))
                                 i (slot :input)]
                             (fn [context frame pubs]
                               (f context frame pubs)
                               (input context frame i)))
                    :output (let [f (eval-inst (first args))
                                  o (slot :output)]
                              (fn [context frame pubs]
                                (output context frame o (f context frame pubs))))
                    :target (let [[f {:keys [input target signal]}]
                                  (u/with-local slots init
                                    (if-some [args (seq args)]
                                      (apply juxt (mapv eval-inst args))
                                      (constantly [])))
                                  t (slot :target)]
                              (fn [context frame pubs]
                                (target context frame t input target signal #(f context frame pubs))))
                    :global (if (contains? resolve (first args))
                              (let [x (get resolve (first args))]
                                (fn [_ _ _] (steady x)))
                              (throw (ex-info (str "Unable to resolve - "
                                                (symbol (first args))) {})))
                    :literal (let [x (first args)]
                               (fn [_ _ _] (steady x)))
                    :constant (let [[f {:keys [input target signal]}]
                                    (u/with-local slots init
                                      (eval-inst (first args)))
                                    c (slot :constant)]
                                (fn [context frame pubs]
                                  (constant context frame c input target signal #(f context frame pubs))))
                    :variable (let [f (eval-inst (first args))]
                                (fn [context frame pubs]
                                  (variable context frame (slot :signal) (f context frame pubs))))
                    (throw (ex-info (str "Unsupported operation - " op) {:op op :args args}))))]
          (let [[[fs] {:keys [input target signal]}]
                (u/with-local slots init
                  (reduce
                    (fn [[fs insts] inst]
                      (let [insts (conj insts (eval-inst inst))]
                        (case (first inst)
                          (:output :target) [fs insts]
                          [(conj fs (comp peek (apply juxt insts))) []])))
                    [[] []] insts))]
            (peer (dec (count fs)) input target signal
              (fn [context]
                (reduce-kv (fn [_ i f] (set-node context i (f context 0 []))) nil (pop fs))
                ((peek fs) context 0 [])))))))))
