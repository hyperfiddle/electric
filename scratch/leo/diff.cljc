(ns leo.diff
  (:require [missionary.core :as m]))

(defn diff-map [curr prev]
  (reduce-kv
    (fn [[created changed removed] k v]
      (if-some [[_ w] (find created k)]
        [(dissoc created k)
         (if (= v w) changed (assoc changed k w))
         removed]
        [created changed (conj removed k)]))
    [curr {} #{}] prev))

(defn patch-map [m [created changed removed]]
  (reduce dissoc (merge m created changed) removed))

(comment
  (diff-map
    {:a 1
     :b 2}
    {:a 2
     :c 5})
  )

(defn diff [- init]
  (fn [rf]
    (let [p (volatile! init)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [r (rf r (- x @p))]
           (vreset! p x) r))))))

(def pick! (partial reduce-kv (fn [m k v] ((m k) v) m)))
(def done! (partial reduce (fn [m k] ((m k)) (dissoc m k))))
(def state
  (partial reduce-kv
    (fn [m k v]
      ;; TODO
      (assoc m k (fn
                   ([])
                   ([x])
                   ([n t]))))))

;; transducer[diff, [k, flow[v]]]
(def stabilizer
  (fn [rf]
    (let [alive (volatile! {})]
      (fn
        ([] (rf))
        ([r]
         (doseq [x (vals @alive)]
           (x)) (rf r))
        ([r [created changed removed]]
         (let [created (state {} created)]
           (vreset! alive
             (-> @alive
               (pick! changed)
               (done! removed)
               (merge created)))
           (reduce-kv rf r created)))))))

;; F[{K,V}] -> F[Pair[K,F[V]]]
(defn fan-out [>in]
  (m/transform (comp (diff diff-map {}) stabilizer) >in))

;; (F[A] -> F[B]) -> F[Pair[K, F[A]]] -> F[Pair[K, F[B]]]
(defn map-flow [f >v]
  (m/ap (f (m/?? >v))))

;; (A -> B) -> Pair[K,A] -> Pair[K,B]
(defn map-second [f [k v]]
  [k (f v)])

;; TODO termination
;; F[Pair[K,F[B]]] -> F[{K,B}]
(defn fan-in [>in]
  (->> (m/ap (let [[k >v] (m/?= >in)]
               {k (m/?! >v)}))
    (m/relieve merge)
    (m/integrate merge)))

(comment

  (def !input (atom {}))

  (defn item [>x]
    (dataflow (inc @>x)))

  (defn imap [f >m]
    (->> >m
      (fan-out)
      (map-flow (partial map-second f))
      (fan-in)))

  (dataflow @(imap item (m/watch !input)))

  )

