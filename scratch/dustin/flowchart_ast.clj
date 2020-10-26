(ns dustin.flowchart-ast)


(comment

  ; dataflow ast
  (def yyy :x)
  (via ()
    [[>a -> inc -> >b]
     [>a -> dec -> >c]
     [[>b >c] -> #(vector %1 %2 yyy) -> out]])

  (def >out
    (via ()
      [(inc >a) (dec >a) :x]
      ))

  (via ()
    (fmap vector (fmap inc >a) (fmap dec >a) :x)
    )


  )