(ns hfdl.impl.runtime
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref)))
  #?(:cljs (:require-macros [hfdl.impl.runtime :refer [with-ctx get-ctx]])))

(def context #?(:clj (ThreadLocal.)))

#?(:clj
   (defmacro get-ctx []
     (if (:js-globals &env)
       `context
       `(.get ~(with-meta `context {:tag `ThreadLocal})))))

#?(:clj
   (defmacro set-ctx [c]
     (if (:js-globals &env)
       `(set! context ~c)
       `(.set ~(with-meta `context {:tag `ThreadLocal}) ~c))))

#?(:clj
   (defmacro with-ctx [ctx & body]
     `(let [ctx# (get-ctx)]
        (set-ctx ~ctx)
        (try ~@body (finally (set-ctx ctx#))))))

(deftype ContextBound [context target]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (with-ctx context (target)))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (with-ctx context @target)))

(def bind ->ContextBound)

(defn bind-ctx [ctx flow]
  (fn [n t] (bind ctx (flow (bind ctx n) (bind ctx t)))))

(defn handler []
  (doto (object-array 3)
    (aset (int 0) 0)
    (aset (int 1) 0)
    (aset (int 2) {})))

(defn dispatch! [store id value]
  ((store id) value) store)

(defn dispatch-all! [handler changes]
  (reduce-kv dispatch! (aget handler (int 2)) changes) nil)

(defn out-id! [handler]
  (doto (aget handler (int 0))
    (->> inc (aset handler (int 0)))))

(defn in-flow [handler]
  (->> (fn [!]
         (let [id (aget handler (int 1))]
           (aset handler (int 1) (inc id))
           (aset handler (int 2) (assoc (aget handler (int 2)) id !))
           #(aset handler (int 2) (dissoc (aget handler (int 2)) id))))
    (m/observe)
    (m/relieve {})))

(defn collapse [x y]
  (cons (merge (first x) (first y))
    (concat (next x) (next y))))

(defn input [ctx]
  (aget ctx (int 0)))

(defn remote-cb [ctx]
  (aget ctx (int 1)))

(defn output-cb [ctx]
  (aget ctx (int 2)))

(defn client-handler [remote output boot handler in]
  (m/ap
    (m/amb=
      (cons {} (m/?> remote))
      (let [[k v] (m/seed (m/?= output))]
        (list (hash-map k (m/?> v))))
      (do (boot) (dispatch-all! handler (m/?> in)) (m/amb>)))))

(defn client [boot write >read]
  (m/reactor
    (let [handler (handler)
          context (doto (object-array 3)
                    (aset (int 0) (in-flow handler)))]
      (->> >read
        (bind-ctx context)
        (m/stream!)
        (client-handler
          (->> (m/observe (fn [!] (aset context (int 1) !) u/nop))
            (m/relieve concat))
          (->> (m/observe (fn [!] (aset context (int 2)
                                    (fn [f] (! {(out-id! handler) f}))) u/nop))
            (m/relieve merge))
          (bind context boot) handler)
        (m/relieve collapse)
        (m/stream!)
        (u/foreach write)
        (bind-ctx context)
        (m/stream!)))))

(defn eval! [resolve input node]
  ((fn walk! [locals [op & args]]
     (case op
       :input (list (m/signal! input))
       :apply (let [xs (mapv (partial walk! locals) args)]
                (cons (apply m/latest u/call (map first xs))
                  (apply concat (map next xs))))
       :share (let [x (walk! locals (first args))
                    y (walk! (conj locals (m/signal! (first x))) (second args))]
                (cons (first y) (concat (next x) (next y))))
       :local (list (nth locals (first args)))
       :global (-> (find resolve (first args))
                 (doto (when-not (u/pst (ex-info (str "Unable to resolve global : "
                                                   (name (first args)))
                                          {:op op :args args}))))
                 val u/pure list)
       :effect (let [x (walk! locals (first args))
                     y (walk! locals (second args))]
                 (m/stream! (first x))
                 (cons (first y) (concat (next x) (next y))))
       :output (let [x (walk! locals (first args))
                     y (walk! locals (second args))]
                 (cons (first y) (concat x (next y))))
       :literal (list (u/pure (first args)))
       :interop (throw (ex-info "Unsupported operation : interop" {:op op :args args}))
       :constant (let [x (walk! locals (first args))]
                   (cons (u/pure (first x)) (next x)))
       :variable (let [x (walk! locals (first args))]
                   (cons (switch (first x)) (next x)))))
   [] node))

(defn server-handler [node! handler in]
  (m/ap
    (let [[changes & nodes] (doto (m/?> in) prn)
          outputs (reduce node! nil nodes)]
      (dispatch-all! handler changes)
      (let [out (m/?= (m/seed outputs))]
        (doto (hash-map (out-id! handler) (m/?> out)) prn)))))

(defn server [resolve write >read]
  (m/reactor
    (let [handler (handler)
          input (in-flow handler)
          node! (fn [outputs node]
                  (concat outputs (eval! resolve input node)))]
      (->> (m/stream! >read)
        (server-handler node! handler)
        (m/relieve merge)
        (m/stream!)
        (u/foreach write)
        (m/stream!)))))