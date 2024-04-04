(ns hyperfiddle.incseq.diff-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.rcf :refer [tests]]))

(defn empty-diff [n]
  {:degree n :grow 0 :shrink 0 :permutation {} :change {} :freeze #{}})

(def patch-vec
  (let [grow! (fn [v n]
                (reduce conj! v (repeat n nil)))
        shrink! (fn [v n]
                  (loop [i 0, v v]
                    (if (< i n)
                      (recur (inc i)
                        (pop! v)) v)))
        change! (fn [r c]
                  (reduce-kv assoc! r c))
        cycles! (partial reduce
                  (fn [v c]
                    (let [i (nth c 0)
                          x (nth v i)]
                      (loop [v v
                             i i
                             k 1]
                        (let [j (nth c k)
                              v (assoc! v i (nth v j))
                              k (unchecked-inc-int k)]
                          (if (< k (count c))
                            (recur v j k)
                            (assoc! v j x)))))))]
    (fn
      ([] [])
      ([v d]
       (-> v
         (transient)
         (grow! (:grow d))
         (cycles! (p/decompose (:permutation d)))
         (shrink! (:shrink d))
         (change! (:change d))
         (persistent!))))))

(defn combine
  ([x] x)
  ([x y]
   (let [px (:permutation x)
         py (:permutation y)
         dx (:degree x)
         dy (:degree y)
         cx (:change x)
         cy (:change y)
         fx (:freeze x)
         fy (:freeze y)
         degree (unchecked-add dy (:shrink x))
         size-before (unchecked-subtract dx (:grow x))
         size-between (unchecked-subtract dy (:grow y))
         size-after (unchecked-subtract dy (:shrink y))]
     (loop [i size-after
            d degree
            p (p/compose py
                (p/split-swap size-between
                  (unchecked-subtract degree dy)
                  (unchecked-subtract degree dx)) px)
            c (reduce-kv assoc!
                (reduce-kv
                  (fn [r i j]
                    (if (contains? cx j)
                      (assoc! r i (cx j)) r))
                  (reduce dissoc! (transient cx)
                    (vals py)) py) cy)
            f (reduce conj!
                (reduce-kv
                  (fn [r i j]
                    (if (contains? fx j)
                      (conj! r i) r))
                  (reduce disj! (transient fx)
                    (vals py)) py) fy)]
       (if (< i d)
         (let [j (p i i)
               c (dissoc! c i)
               f (disj! f i)]
           (if (< j size-before)
             (recur (unchecked-inc i) d p c f)
             (recur i (unchecked-dec d)
               (p/compose (p/rotation i d)
                 p (p/rotation d j)) c f)))
         {:degree      d
          :permutation p
          :grow        (unchecked-subtract d size-before)
          :shrink      (unchecked-subtract d size-after)
          :change      (persistent! c)
          :freeze      (persistent! f)}))))
  ([x y & zs] (reduce combine (combine x y) zs)))

(tests "sequence diffs"
  (patch-vec [:a :b :c]
    {:grow 1
     :degree 4
     :permutation (p/rotation 3 1)
     :shrink 2
     :change {1 :e}}) :=
  [:a :e]
  (patch-vec [:a :e]
    {:grow 2
     :degree 4
     :permutation (p/rotation 1 3)
     :shrink 1
     :change {0 :f 1 :g 2 :h}}) :=
  [:f :g :h]

  (patch-vec [:a :b :c]
    {:grow 1
     :degree 4
     :permutation {}
     :shrink 1
     :change {0 :f, 1 :g, 2 :h}}) :=
  [:f :g :h]

  (combine
    {:degree 1 :grow 1 :permutation {} :shrink 0 :change {0 :a} :freeze #{}}
    {:degree 1 :grow 0 :permutation {} :shrink 1 :change {} :freeze #{}}) :=
  {:degree 0 :grow 0 :permutation {} :shrink 0 :change {} :freeze #{}}

  (combine
    {:grow 1 :degree 4 :permutation (p/rotation 3 1) :shrink 2 :change {1 :e} :freeze #{}}
    {:grow 2 :degree 4 :permutation (p/rotation 1 3) :shrink 1 :change {0 :f 1 :g 2 :h} :freeze #{}}) :=
  {:degree 5 :grow 2 :shrink 2 :permutation (p/compose (p/cycle 2 4) (p/cycle 1 3)) :change {0 :f, 1 :g, 2 :h} :freeze #{}})