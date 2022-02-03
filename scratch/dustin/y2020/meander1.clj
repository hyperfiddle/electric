(ns dustin.meander1
  (:require [meander.epsilon :as m]))


(comment

  (defn g [pat]
    (m/match pat
      {& (m/seqable [?edge ?pat])} 1
      ?else 2
      )
    )
  (g {:dustingetz/gender :y })
  (g {:dustingetz/gender [:y]})

  (defn f [pat]
    (m/match pat
      [!pats ...] (f (first !pats))
      {& (m/seqable [?edge ?pat])} 2
      ?else ?else
      ))

  (f [#:dustingetz{:gender [:db/id]}])
  (f [{:dustingetz/gender [:db/ident :db/id]}])
  )

(comment
  (m/rewrite [:a 1 2 3 :b 4 5 :c 6 7 8 9]
    [] [] ; The base case for no values left
    [(m/pred keyword? ?x) . (m/pred int? !ys) ... & ?more]
    {& [[?x [!ys ...]] & (m/cata ?more)]})

  )

(comment
  (def x `('z))
  (m/match x
    (quote ?s) :a
    ('quote ?s) :b
    ('quote & _ ) :bs
    _ :z)
  )