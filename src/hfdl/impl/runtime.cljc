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

(defn steady [x]
  (m/observe (fn [!] (! x) u/nop)))

(defn input [cb]
  (->> (fn [!] (cb !) u/nop)
    (m/observe)
    (m/relieve {})
    (m/signal!)))

(defn output [context frame slot flow]
  ((aget context (int 1)) [(- frame) slot flow]))

(defn constant [context target slot ctor]
  (steady
    (fn [n t]
      (let [req (aget context (int 0))
            source (inc (aget context (int 2)))
            flow (ctor (aset context (int 2) source))]
        (req [(- target) slot])
        (flow n #(do (req [(- source) -1]) (comment destructor) (t)))))))

(defn create [context frame input target signal]
  (doto context
    (aset (int 4) (assoc (aget context (int 4)) frame input))
    (aset (int 5) (assoc (aget context (int 5)) frame target))
    (aset (int 6) (assoc (aget context (int 6)) frame signal))))

(defn discard [context frame]
  (let [signals (get (aget context (int 6)) frame)]
    (doto context
      (aset (int 4) (dissoc (aget context (int 4)) frame))
      (aset (int 5) (dissoc (aget context (int 5)) frame))
      (aset (int 6) (dissoc (aget context (int 6)) frame)))
    (loop [i 0]
      (when (< i (alength signals))
        ((aget signals i))
        (recur (inc i))))))

(defn target [context frame slot]
  ((-> context
     (aget (int 5))
     (get frame)
     (aget slot))
   (aset context (int 3)
     (dec (aget context (int 3))))))

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
    (target context frame slot))
  context)

(defn peer [boot write >read]
  (m/reactor
    (let [ctx (doto (object-array 7)
                (aset (int 0) (m/mbx))
                (aset (int 1) (m/mbx))
                (aset (int 2) 0)
                (aset (int 3) 0)
                (aset (int 4) {})
                (aset (int 5) {})
                (aset (int 6) {}))]
      (m/stream! (or (boot ctx) m/none))
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
                                              (catch #?(:clj Throwable
                                                        :cljs :default) _
                                                (m/?> m/none)))}] (recur))))))
        (m/relieve message)
        (m/stream!)
        (u/foreach (-> write (u/log-args '>)))
        (m/stream!)))))

(def eval
  (let [slots (u/local)
        init {:input [] :target [] :signal [] :output 0 :constant 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))
            (store [k x]
              (let [m (u/get-local slots)]
                (u/set-local slots (update m k conj x)) nil))]
      (fn [resolve nodes]
        (fn [context]
          ((fn eval-frame [locals insts frame]
             (let [prev (u/get-local slots)]
               (u/set-local slots init)
               (try
                 (let [results
                       (mapv (partial
                               (fn eval-inst [locals [op & args]]
                                 (case op
                                   :sub (nth locals (- (count locals) (first args)))
                                   :pub (let [s (m/signal! (eval-inst locals (first args)))]
                                          (store :signal s)
                                          (eval-inst (conj locals s) (second args)))
                                   :apply (apply m/latest u/call (map (partial eval-inst locals) args))
                                   :input (do (run! (partial eval-inst locals) args)
                                              (input (partial store :input)))
                                   :output (output context frame (slot :output) (eval-inst locals (first args)))
                                   :target (store :target (partial eval-frame locals args))
                                   :global (let [x (resolve (first args) resolve)]
                                             (if (identical? x resolve)
                                               (partial u/failer
                                                 (doto (ex-info (str "Unable to resolve - "
                                                                  (symbol (first args))) {}) u/pst))
                                               (steady x)))
                                   :literal (steady (first args))
                                   :constant (constant context frame (slot :constant)
                                               (partial eval-frame locals args))
                                   :variable (switch (eval-inst locals (first args)))
                                   (throw (ex-info (str "Unsupported operation - " op) {:op op :args args}))))
                               locals) insts)]
                   (apply create context frame
                     (map (comp object-array (u/get-local slots))
                       [:input :target :signal]))
                   (peek results))
                 (finally (u/set-local slots prev)))))
           [] (peek nodes) 0))))))