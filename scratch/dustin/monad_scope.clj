(ns dustin.monad-scope
  (:require
    [minitest :refer [tests]]))


(defn pure [a] (fn [scope] [scope a]))

(defn runScope [m scope] (m scope))

(tests
  (runScope (pure 42) {})
  => [{} 42]
  )

(defn bind [ma mf]
  ; https://kseo.github.io/posts/2017-01-21-writer-monad.html
  ; (Scope is not quite a writer as the state unwinds)
  (fn [scope]
    (let [[log-a a] (runScope ma scope)
          scope' (merge scope log-a #_{'% a})]                ; accumulate new vars that shadow old vars
      (runScope (mf a) scope'))))                              ; pass scope forward only (into the continuation)

(tests
  (do (def ma (pure 42)) nil) => nil

  (runScope (bind ma pure) {})
  => [{} 42]

  (runScope (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  => '[{a 42} 43]

  (runScope (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  => '[{z 0} 1]

  (runScope (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  => '[{z 1} 1]

  )

(defn fmap [f ma]                                           ; mv :: scope -> a; f :: a -> b
  (fn [scope]                                               ; *this
    (let [[scope' a] (ma scope)
          b (f a)]                                          ; fmap neither reads nor writes
      [scope' #_(assoc scope' '% b)                                  ; auto-set % ?
       b])))

(tests
  (runScope (fmap inc ma) {'x 99})
  => [{'x 99} 43]

  (runScope (fmap inc (fmap inc ma)) {'x 99})
  => [{'x 99} 44]
  )
