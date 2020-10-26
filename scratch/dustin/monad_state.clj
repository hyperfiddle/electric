(ns user.dustin.mstate
  (:require
    [contrib.do :refer [! via* *this]]))


(comment
  (defrecord State [s]
    Do-via
    (resolver-for [_]
      {:State.get   (fn [[_]] (:s (get-state)))
       :State.set!  (fn [[_ v]] (set! *this (->State v)))
       }))

  (defn foo []
    (! :State.set! (inc (! :State.get)))
    (! :State.get))

  (via* (->State 42)
    (let [s (! :State.get)]
      (println 42 s)
      (! :State.set! (inc s))
      (println 43 (! :State.get))
      (foo))
    (println *stack)
    )

  (defn bind [mv f]
    (f @mv))

  (defn foo [k]
    ; basically returns a env' alongside a v, except env is in a binding
    (let [env (! :State.get)
          v' (inc (get env k))])
    (! :State.set! k v')
    v')
  )
