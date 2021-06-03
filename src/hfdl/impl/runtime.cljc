(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval])
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.switch :as s]
            [hfdl.impl.rfor :refer [rfor]]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref)))
  #?(:cljs (:require-macros [hfdl.impl.runtime :refer [with-ctx get-ctx]])))

(def prod)
(def node)

;; context
;; 0: root frame
;; 1: request callback
;; 2: publish callback

;; frame
;; 0: static vector of nested classes
;; 1: static vector of signals
;; 2: static vector of inputs
;; 3: static counter of outputs

;; inner
;; 0: dynamic map from frame id to frame
;; 1: id of next frame constructed
;; 2: frame constructor

(defn child [r x]
  (get (aget r (int 0)) x))

(defn child-in [context path]
  (reduce child (aget context (int 0)) path))

;; TODO
(defn cancel-frame [fr]
  (reduce
    (fn [_ in]
      (reduce-kv (fn [_ _ fr] (cancel-frame fr))
        nil (aget in (int 0))))
    nil (aget fr (int 0)))
  (reduce (fn [_ x]) nil (aget fr 1))
  (reduce (fn [_ x]) nil (aget fr 2)))

(defn inner [context path ctor]
  (let [frame (child-in context path)
        slots (aget frame (int 0))]
    (doto (object-array 3)
      (aset (int 0) {})
      (aset (int 1) 0)
      (aset (int 2) ctor)
      (->> (conj slots)
        (aset frame (int 0))))
    (conj path (count slots))))

(defn frame []
  (doto (object-array 4)
    (aset (int 0) [])
    (aset (int 1) [])
    (aset (int 2) [])
    (aset (int 3) 0)))

(defn allocate [context path]
  (let [in (child-in context path)
        id (aget in (int 1))]
    (aset in (int 0)
      (assoc (aget in (int 0))
        id (frame)))
    (aset in (int 1) (inc id))
    (conj path id)))

(defn steady [x]
  (m/observe (fn [!] (! x) u/nop)))

(defn spawn [context path & args]
  (let [inner (child-in context path)
        request (aget context (int 1))]
    (request path)
    (let [path (allocate context path)
          flow (apply (aget inner (int 2)) path (map steady args))]
      (fn [n t]
        (flow n
          #(do (cancel-frame (child-in context path))
               (request path) (t)))))))

(defn branch [context test choice]
  (->> (m/stream! test) ;; TODO register that stream for cancellation
    (m/eduction
      (map choice)
      (dedupe)
      (map (partial spawn context)))
    (s/switch)))

(defn product [context path ctor & inputs]
  (apply rfor (partial spawn context (inner context path ctor)) (mapv m/stream! inputs))) ;; TODO register these ones too

(def invoke (partial m/latest u/call))

(defn share [context path flow]
  (let [frame (child-in context path)]
    (doto (m/signal! flow)
      (->> (conj (aget frame (int 1)))
        (aset frame (int 1))))))

(defn output [context path flow]
  (let [frame (child-in context path)
        id (aget frame (int 3))]
    (aset frame (int 3) (inc id))
    ((aget context (int 2)) (conj path id (m/signal! flow))))) ;; TODO register that signal too

(defn input [context path]
  (let [frame (child-in context path)]
    (->> (fn [!]
           (let [slots (aget frame (int 2))
                 id (count slots)]
             (aset frame (int 2) (conj slots !))
             #(aset frame (int 2) (assoc slots id nil))))
      (m/observe)
      (m/relieve {})
      (m/signal!))))

(defn message
  ([] [{}])
  ([x] x)
  ([x y]
   (-> (pop x)
     (into (pop y))
     (conj (merge (peek x) (peek y)))))
  ([x y & zs]
   (reduce message (message x y) zs)))

(defn change [context path value]
  ((-> (child-in context (pop path))
     (aget (int 2))
     (get (peek path)))
   value) context)

(defn handle [context path]
  (if (even? (count path))
    (cancel-frame (child-in context path))
    ((aget (child-in context path) (int 2))
     (allocate context path))) context)

(defn peer [boot write >read]
  (m/reactor
    (let [ctx (doto (object-array 3)
                (aset (int 0) (frame))
                (aset (int 1) (m/mbx))
                (aset (int 2) (m/mbx)))]
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
                     (let [x (m/? (aget ctx (int 1)))]
                       (m/amb= [x {}] (recur))))
                   (loop []
                     (let [x (m/? (aget ctx (int 2)))]
                       (m/amb= [{(pop x) (m/?> (peek x))}] (recur))))))
        (m/relieve message)
        (m/stream!)
        (u/foreach write)
        (m/stream!)))))

(defn eval [resolve nodes]
  (fn [ctx]
    ((fn walk [path locals [op & args]]
       (case op
         :sub (nth locals (first args))
         :pub (walk path (conj locals (share ctx path (walk path locals (first args)))) (second args))
         :node (walk path (into [] (map (comp (partial share ctx path)
                                          (partial walk path locals))) (next args))
                 (nth nodes (first args)))
         :void (transduce (map (partial walk path locals)) {} nil args)
         :case (branch ctx (walk path locals (first args))
                 (let [[default & branches]
                       (into []
                         (map (fn [inst] (inner ctx path (fn [path] (walk path locals inst)))))
                         (cons (second args) (map peek (nnext args))))
                       dispatch (into {} cat
                                  (map (partial map vector)
                                    (map pop (nnext args))
                                    (map repeat branches)))]
                   (fn [test] (get dispatch test default))))
         :frame (inner ctx path (fn [path] (walk path locals (first args))))
         :input (do (walk path locals (first args)) (input ctx path))
         :output (output ctx path (walk path locals (first args)))
         :invoke (apply invoke (map (partial walk path locals) args))
         :global (-> (find resolve (first args))
                   (doto (when-not (u/pst (ex-info (str "Unable to resolve " (first args))
                                            {:op op :args args}))))
                   val steady)
         :product (apply product ctx path
                    (fn [path & ids] (walk path (into locals ids) (first args)))
                    (map (partial walk path locals) (next args)))
         :literal (steady (first args))
         :constant (steady (walk path locals (first args)))
         :variable (s/switch (walk path locals (first args)))
         (throw (ex-info (str "Unsupported operation " op) {:op op :args args}))))
     [] [] [:node (dec (count nodes))])))