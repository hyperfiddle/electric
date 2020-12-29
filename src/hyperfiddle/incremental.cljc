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