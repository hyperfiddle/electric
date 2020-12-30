(ns hyperfiddle.incremental
  (:require
    [missionary.core :refer [latest relieve watch ap ?!]]
    [minitest #?@(:clj [:refer [tests]])])
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  )


; monad instance for Incremental values
(defn pureI [a] (watch (atom a)))
(defn fmapI [f & >as] (apply latest f >as))
(defn bindI [>a f] (relieve {} (ap (?! (f (?! >a))))))
(defn joinI [>>a] (bindI >>a identity))
(defn sequenceI [>as] (apply fmapI vector >as))
(defn sequence-mapI [k>vs] (apply fmapI #(zipmap (keys k>vs) %&) (vals k>vs)))
(defn capI "test primitive" [Ia] @(Ia #() #()))

(tests
  (capI (fmapI inc (pureI 1))) := 2
  (def >>a (pureI (pureI 1)))
  (capI (bindI >>a identity)) := 1
  (capI (joinI >>a)) := 1
  ; type error (capI (joinI (pureI 1)))
  (capI (sequenceI [(pureI 1) (pureI 2)])) := [1 2]
  (capI (sequence-mapI {:a (pureI 1) :b (pureI 2)})) := {:a 1 :b 2}
  (sequenceI (map sequence-mapI [{:a (pureI 1) :b (pureI 2)}
                                 {:a (pureI 1) :b (pureI 2)}]))
  ;(capI *1) := [{:a 1, :b 2} {:a 1, :b 2}]
  )

; extend-seqI :: I Seq a -> I Seq I a -- reactive list with reactive elements
(defn extend-seqI
  "this is not quite the opposide of sequence, it extends a layer"
  [>as]
  (->> >as
    (fmapI (fn [as]
             ; allocate inputs and introduce layer
             (map pureI as)))))

(def unsequenceI extend-seqI)

(tests
  (def >>as (extend-seqI (pureI [9 10 11])))
  (map capI (capI >>as)) := [9 10 11]
  (capI (bindI >>as sequenceI)) := [9 10 11]

  (capI (bindI (extend-seqI (pureI [1 2 3])) sequenceI)) := [1 2 3]
  (capI (capI (fmapI sequenceI (extend-seqI (pureI [1 2 3]))))) := [1 2 3]

  (->> >>as
    (fmapI (fn [>as]
             (->> >as
               (map (fn [>a]
                      (fmapI identity #_:dustingetz/email >a)))))))
  ;(capI (bindI *1 sequenceI)) := [9 10 11]
  )

(defn incr? [x] (fn? x))

(tests
  (incr? (pureI 1)) := true
  (fn? (pureI 1)) := true
  (incr? {}) := false
  (incr? []) := false
  (incr? :x) := false
  (incr? #{}) := false

  ;(type (pureI 1)) := 'missionary.core$watch$fn__12137
  ;(class (pureI 1)) := 'missionary.core$watch$fn__12137
  )

;(tests
;  (prewalk #(if (= :a %) :A %) [:a :b]) := [:A :b])

(defn pathwalk
  ([f e] (pathwalk f [] e))
  ([f path e]
   (let [e' (f path e)]
     (cond
       (map? e') (->> e'
                   (map (fn [[k x]] [k (pathwalk f (conj path k) x)]))
                   (into (empty e')))
       (coll? e') (->> e'
                    (map-indexed (fn [i x] (pathwalk f (conj path i) x)))
                    (into (empty e')))
       :else e'))))

(tests
  (pathwalk
    (fn [path v] (println path v) (if (= :a v) :A v))
    [{:cat "Garfield", :dog "DogeCoin"} [:a :b {:site "so"}]])
  := [{:cat "Garfield", :dog "DogeCoin"} [:A :b {:site "so"}]])

(defn scan [f? tree]
  (let [!a (atom [])]
    (pathwalk
      (fn [path v]
        (if (f? v)
          (swap! !a conj path))
        v)
      tree)
    @!a))

(tests
  (scan #(= % '_)
    '[{:dustingetz/gender {:db/id _, :db/ident _}}
      {:dustingetz/gender {:db/id _, :db/ident _}}])
  := [[0 :dustingetz/gender :db/id]
      [0 :dustingetz/gender :db/ident]
      [1 :dustingetz/gender :db/id]
      [1 :dustingetz/gender :db/ident]])

(defn sequence-some [tree]
  (let [paths (scan incr? tree)]
    (apply fmapI
      (fn [& vs]
        (reduce (fn [acc [path v]]
                  (assoc-in acc path v))
          tree (map vector paths vs)))
      (map #(get-in tree %) paths))))

(defn sequence-some [tree]
  (let [paths (scan incr? tree)
        >as (sequenceI (map #(get-in tree %) paths))]
    (fmapI (fn [as]
             (reduce (fn [acc [path a]]
                       (assoc-in acc path a))
               tree
               (map vector paths as)))
      >as)))

(tests
  (capI (sequence-some
          {:a (pureI :A)
           :b (pureI :B)}))
  := {:a :A, :b :B}

  )