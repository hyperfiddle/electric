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

(defn input [context slot]
  (let [inputs (get (aget context (int 4)) (aget context (int 2)))]
    (->> (fn [!] (aset inputs slot !) u/nop)
      (m/observe)
      (m/relieve {})
      (m/signal!))))

(defn frame [context id inputs targets signals]
  (aset context (int 4) (assoc (aget context (int 4)) id (object-array inputs)))
  (aset context (int 5) (assoc (aget context (int 5)) id (object-array targets)))
  (aset context (int 6) (assoc (aget context (int 6)) id (object-array signals))))

(defn target [context slot inputs targets signals ctor]
  (aset (get (aget context (int 5)) (aget context (int 2))) slot
    (fn []
      (let [id (aset context (int 3) (dec (aget context (int 3))))]
        (frame context id inputs targets signals)
        (ctor) context))))

(defn publish [context slot flow]
  (aset (get (aget context (int 6)) (aget context (int 2))) slot (m/signal! flow)))

(defn output [context slot flow]
  ((aget context (int 1)) [(- (aget context (int 2))) slot flow]))

(defn constant [context slot inputs targets signals ctor]
  (let [req (aget context (int 0))
        id (aget context (int 2))]
    (steady
      (fn [n t]
        (req [(- id) slot])
        (let [id (aset context (int 2) (inc (aget context (int 2))))]
          (frame context id inputs targets signals)
          ((ctor) n #(do (req [(- id) -1])
                         (comment destructor) (t))))))))

(defn get-node [context slot]
  (aget (aget context (int 7)) slot))

(defn set-node [context slot value]
  (aset (aget context (int 7)) slot value))

(defn capture [context]
  (let [curr (aclone (aget context (int 7)))]
    (fn [flow]
      (fn [n t]
        (let [prev (aget context (int 7))]
          (aset context (int 7) curr)
          (try (flow n t)
               (finally
                 (aset context (int 7) prev))))))))

(defn binder [context & slots]
  (fn [flow & args]
    (fn [n t]
      (let [prev (mapv (partial get-node context) slots)]
        (mapv (partial set-node context) slots args)
        (try (flow n t)
             (finally (mapv (partial set-node context) slots prev)))))))

;; TODO cancel signals
(defn variable [context flow]
  (->> flow
    (m/stream!)
    (m/eduction (dedupe) (map (capture context)))
    (switch)
    (m/signal!)))

(defn discard [context frame]
  (let [signals (get (aget context (int 6)) frame)]
    (loop [i 0]
      (when (< i (alength signals))
        ((aget signals i))
        (recur (inc i)))))
  (doto context
    (aset (int 4) (dissoc (aget context (int 4)) frame))
    (aset (int 5) (dissoc (aget context (int 5)) frame))
    (aset (int 6) (dissoc (aget context (int 6)) frame))))

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
                  (aset (int 7) (object-array nodes)))]
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
                           (fn [_ pubs] (nth pubs (- (count pubs) idx))))
                    :pub (let [[f g] (mapv eval-inst args)
                               s (slot :signal)]
                           (fn [context pubs]
                             (g context (conj pubs (publish context s (f context pubs))))))
                    :def (fn [context _] (steady (apply binder context args)))
                    :node (let [n (first args)]
                            (fn [context _] (get-node context n)))
                    :apply (comp (partial apply m/latest u/call)
                             (apply juxt (map eval-inst args)))
                    :input (let [f (apply juxt (map eval-inst args))
                                 i (slot :input)]
                             (fn [context pubs]
                               (f context pubs)
                               (input context i)))
                    :output (let [f (eval-inst (first args))
                                  o (slot :output)]
                              (fn [context pubs]
                                (output context o (f context pubs))))
                    :target (let [[f {:keys [input target signal]}]
                                  (u/with-local slots init
                                    (apply juxt (mapv eval-inst args)))
                                  t (slot :target)]
                              (fn [context pubs]
                                (target context t input target signal #(f context pubs))))
                    :global (if (contains? resolve (first args))
                              (let [x (get resolve (first args))]
                                (fn [_ _] (steady x)))
                              (throw (ex-info (str "Unable to resolve - "
                                                (symbol (first args))) {})))
                    :literal (let [x (first args)]
                               (fn [_ _] (steady x)))
                    :constant (let [[f {:keys [input target signal]}]
                                    (u/with-local slots init
                                      (eval-inst (first args)))
                                    c (slot :constant)]
                                (fn [context pubs]
                                  (constant context c input target signal #(f context pubs))))
                    :variable (let [f (eval-inst (first args))]
                                (fn [context pubs]
                                  (variable context (f context pubs))))
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
                (reduce-kv (fn [_ i f] (set-node context i (f context []))) nil (pop fs))
                ((peek fs) context [])))))))))