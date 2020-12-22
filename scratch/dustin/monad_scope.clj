(ns dustin.monad-scope
  (:refer-clojure :exclude [sequence])
  (:require
    [minitest :refer [tests]]))


(defn pure [a] (fn [scope] [scope a]))

(defn runScope [m scope] (m scope))

(tests
  (runScope (pure 42) {})
  := [{} 42]
  )

(defn bind [ma mf]                                          ; https://kseo.github.io/posts/2017-01-21-writer-monad.html
  (fn [scope]
    (let [[log-a a] (runScope ma scope)]
      (runScope (mf a) (merge scope log-a)))))

(tests
  (do (def ma (pure 42)) nil) := nil

  (runScope (bind ma pure) {})
  := [{} 42]

  (runScope (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  := '[{a 42} 43]

  (runScope (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  := '[{z 0} 1]

  (runScope (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  := '[{z 1} 1]

  )

(defn fmap [f & mas]                                           ; mv :: scope -> a; f :: a -> b
  (fn [scope]                                               ; *this
    (let [as (map (fn [ma] (second (ma scope))) mas)
          b  (apply f as)]                                          ; fmap neither reads nor writes
      [scope b])))

(tests
 (runScope (fmap inc (pure 1)) {}) := [{} 2]
 (runScope (fmap + (pure 1) (pure 2)) {}) := [{} 3]
 (runScope (fmap vector (pure 1) (pure 2)) {}) := [{} [1 2]])

(defn sequence "[mv] -> m [v]" [mvs]
  (when mvs
    (apply fmap vector mvs)))

(tests
 (runScope (sequence []) {}) := [{} []]
 (runScope (sequence [(pure 1) (pure 2)]) {}) := [{} [1 2]])

;; (defn fmap [f ma]                                           ; mv :: scope -> a; f :: a -> b
;;   (fn [scope]                                               ; *this
;;     (let [[scope' a] (ma scope)
;;           b (f a)]                                          ; fmap neither reads nor writes
;;       [scope' #_(assoc scope' '% b)                                  ; auto-set % ?
;;        b])))

(tests
  (runScope (fmap inc ma) {'x 99})
  := [{'x 99} 43]

  (runScope (fmap inc (fmap inc ma)) {'x 99})
  := [{'x 99} 44]
  )
