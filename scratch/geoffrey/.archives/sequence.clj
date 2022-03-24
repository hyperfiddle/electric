(ns geoffrey.sequence
  (:require [minitest :refer [tests]]))

(defn pure [v] {:Maybe/just v})

(defn fmap [f & fvs]
  (let [vs (map :Maybe/just fvs)]
    (if (every? identity vs)
      (pure (apply f vs)))))

(defn bind [{v :Maybe/just} cont] (if v (cont v)))

(tests
 (pure 1) := {:Maybe/just 1}
 (fmap inc (pure 1)) := (pure 2)
 (fmap inc nil) := nil
 (bind (pure 1) inc) := 2
 (bind (pure 1) (fn [v] (pure (inc v)))) := (pure 2)
 )

(defn sequence "[mv] -> m [v]" [mvs]
  (when mvs
    (apply fmap vector mvs)))

(tests
 (sequence nil) := nil
 (sequence []) := (pure [])
 (sequence [(pure 1) (pure 2)]) := (pure [1 2]))
