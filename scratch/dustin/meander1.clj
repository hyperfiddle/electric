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