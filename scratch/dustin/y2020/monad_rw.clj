(ns dustin.y2020.monad-rw
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
  := [{} 42]
  )

(defn bind [ma mf]                                          ; this is not RW, this is Scope monad
  ; https://kseo.github.io/posts/2017-01-21-writer-monad.html
  (fn [scope]
    (let [[log-a a] (runRW ma scope)
          [log-b b] (runRW (mf a) (merge scope log-a))]     ; mappend here for scope that unwinds
      [log-b #_(merge scope log-a log-b)                    ; mappend here for state that carries forward
       b])))

(tests
  (def ma (pure 42))

  (runRW (bind ma pure) {})
  := [{} 42]

  (runRW (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  := '[{a 42} 43]

  (runRW (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  := '[{z 0} 1]

  (runRW (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  := '[{z 1} 1]

  )

(defn fmap [f ma]                                           ; mv :: scope -> a; f :: a -> b
  (fn [scope]                                               ; *this
    (let [[scope' a] (ma scope)
          b (f a)]                                          ; fmap neither reads nor writes
      [(merge scope scope')
       b])))

(tests
  (runRW (fmap inc ma) {'x 99})
  := [{'x 99} 43]

  (runRW (fmap inc (fmap inc ma)) {'x 99})
  := [{'x 99} 44]
  )

(defn discardW [ma]
  (fn [scope]
    (let [[_ b] (ma scope)]
      [{} b])))

(tests
  (runRW (discardW (fmap inc ma)) {'x 99})
  := [{} 43]

  )
