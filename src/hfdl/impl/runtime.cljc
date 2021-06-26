(ns ^{:doc ""}
  hfdl.impl.runtime
  (:refer-clojure :exclude [eval])
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.impl.rfor :refer [rfor]]
            [missionary.core :as m]))

(def prod)
(def node)

;; context
;; 0: path -> frame
;; 1: request callback
;; 2: publish callback

;; frame
;; 0: static vector of nested classes
;; 1: static vector of signals
;; 2: static vector of inputs
;; 3: static counter of outputs

;; inner
;; 0: set of current child frame ids
;; 1: id of next frame constructed
;; 2: frame constructor

(defn inner [context path ctor]
  (let [frame (get (aget context 0) path)
        slots (aget frame (int 0))]
    (doto (object-array 3)
      (aset (int 0) #{})
      (aset (int 1) 0)
      (aset (int 2) ctor)
      (->> (conj slots)
        (aset frame (int 0))))
    (count slots)))

(defn frame []
  (doto (object-array 4)
    (aset (int 0) [])
    (aset (int 1) [])
    (aset (int 2) [])
    (aset (int 3) 0)))

(defn steady [x]
  (m/observe (fn [!] (! x) u/nop)))

;; TODO
(defn destruct [context path]
  (prn :destruct path)
  (let [store (aget context (int 0))
        frame (get store path)]
    (aset context (int 0) (dissoc store path))
    (reduce-kv
      (fn [_ slot inner]
        (reduce (fn [_ id] (destruct context (conj path slot id)))
          nil (aget inner (int 0))))
      nil (aget frame (int 0)))
    (reduce (fn [_ x] (x)) nil (aget frame (int 1))))
  (prn :destructed path))

(defn allocate [context path slot & args]
  (let [in (-> context
             (aget (int 0))
             (get path)
             (aget (int 0))
             (nth slot))
        id (aget in (int 1))
        path (conj path slot id)]
    (aset in (int 0) (conj (aget in (int 0)) id))
    (aset in (int 1) (inc id))
    (aset context (int 0)
      (assoc (aget context (int 0))
        path (frame)))
    (apply (aget in (int 2))
      path (map steady args))))

(defn active [context path flow]
  ((aget context (int 1)) (pop path))
  (fn [n t]
    (flow n
      #(do ((aget context (int 1)) path)
           (destruct context path) (t)))))

(defn branch [context path test choice]
  (->> (m/signal! test) ;; TODO register that stream for cancellation
    (m/eduction
      (map choice)
      (dedupe)
      (map (partial allocate context path)))
    (switch)))

(defn product [context path inputs slot]
  (apply rfor (partial allocate context path slot)
    (mapv m/signal! inputs))) ;; TODO register these ones too

(def invoke (partial m/latest u/call))

(defn share [context path flow]
  (let [frame (get (aget context (int 0)) path)]
    (doto (m/signal! flow)
      (->> (conj (aget frame (int 1)))
        (aset frame (int 1))))))

(defn output [context path flow]
  (let [frame (get (aget context (int 0)) path)
        id (aget frame (int 3))
        out (m/signal! flow)]
    (aset frame (int 3) (inc id))
    (aset frame (int 1) (conj (aget frame (int 1)) out))
    ((aget context (int 2)) (conj path id out))))

(defn input [context path]
  (let [frame (get (aget context (int 0)) path)]
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
  ((-> context
     (aget (int 0))
     (get (pop path))
     (aget (int 2))
     (nth (peek path)))
   value) context)

(defn handle [context path]
  (if (odd? (count path))
    (allocate context (pop path) (peek path))
    (destruct context path)) context)

(defn peer [boot write >read]
  (m/reactor
    (let [ctx (doto (object-array 3)
                (aset (int 0) {[] (frame)})
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
                       (m/amb= [{(pop x) (try (m/?> (peek x))
                                              (catch #?(:clj Throwable
                                                        :cljs :default) e
                                                (u/pst e)))}] (recur))))))
        (m/relieve message)
        (m/stream!)
        (u/foreach (-> write (u/log-args '>)))
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
         :case (branch ctx path (walk path locals (first args))
                 (let [[default & branches]
                       (into []
                         (map (fn [inst]
                                (inner ctx path
                                  (fn [path]
                                    (active ctx path
                                      (walk path locals inst))))))
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
         :product (product ctx path
                    (mapv (partial walk path locals) (next args))
                    (inner ctx path
                      (fn [path & ids]
                        (active ctx path
                          (walk path (into locals ids)
                            (first args))))))
         :literal (steady (first args))
         :constant (steady (walk path locals (first args)))
         :variable (switch (walk path locals (first args)))
         (throw (ex-info (str "Unsupported operation " op) {:op op :args args}))))
     [] [] [:node (dec (count nodes))])))