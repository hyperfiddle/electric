(ns dustin.monad-scope
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [hyperfiddle.api :as hf]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


(defn pure [a] (fn [scope] [scope a]))

(defn runRW [m scope] (m scope))

(tests
  (runRW (pure 42) {})
  => [{} 42]
  )

(defn bind [ma mf]
  ; https://kseo.github.io/posts/2017-01-21-writer-monad.html
  ; (Scope is not quite a writer as the state unwinds)
  (fn [scope]
    (let [[log-a a] (runRW ma scope)
          scope' (merge scope log-a {'% a})]                ; accumulate new vars that shadow old vars
      (runRW (mf a) scope'))))                              ; pass scope forward only (into the continuation)

(tests
  (def ma (pure 42))

  (runRW (bind ma pure) {})
  => [{} 42]

  (runRW (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  => '[{a 42} 43]

  (runRW (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  => '[{z 0} 1]

  (runRW (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  => '[{z 1} 1]

  )

(defn fmap [f ma]                                           ; mv :: scope -> a; f :: a -> b
  (fn [scope]                                               ; *this
    (let [[scope' a] (ma scope)
          b (f a)]                                          ; fmap neither reads nor writes
      [(assoc scope' '% b)                                  ; auto-set % ?
       b])))

(tests
  (runRW (fmap inc ma) {'x 99})
  => [{'x 99} 43]

  (runRW (fmap inc (fmap inc ma)) {'x 99})
  => [{'x 99} 44]
  )
