(ns dustin.via-dag)


(comment

  ; dataflow ast
  (let [yyy :x]
    (via (->StaticDag)
      @{[>a -> inc -> >b]
        [>a -> dec -> >c]
        [>b, >c -> #(vector %1 %2 yyy) -> out]}))

  (def >out
    (via ()
      [(inc >a) (dec >a) :x]
      ))

  (via ()
    (fmap vector (fmap inc >a) (fmap dec >a) :x)
    )


  )