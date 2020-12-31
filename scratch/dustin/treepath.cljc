(ns dustin.treepath
  )



(defn tree-seq-path
  "Like core's tree-seq but returns a lazy sequence of vectors of the
  paths of the nodes in a tree, via a depth-first walk. It optionally
  applies node-fn to each node before adding it to the path. branch?
  must be a fn of one arg that returns true if passed a node that can
  have children (but may not).  children must be a fn of one arg that
  returns a sequence of the children. Will only be called on nodes for
  which branch? returns true. Root is the root node of the tree."
  [branch? children root & [node-fn]]
  (let [node-fn (or node-fn identity)
        walk (fn walk [path node]
               (let [new-path (conj path (node-fn node))]
                 (lazy-seq
                   (cons new-path
                     (when (branch? node)
                       (mapcat (partial walk new-path) (children node)))))))]
    (walk [] root)))

(tests
  (tree-seq-path map? vals
    '{:dustingetz/gender {:db/id _, :db/ident _2}}
    #_(fn name [x]
        (if (map-entry? x) (first x))))
  := '([[1 [2 [3]]]]
       [[1 [2 [3]]] 1]
       [[1 [2 [3]]] [2 [3]]]
       [[1 [2 [3]]] [2 [3]] 2]
       [[1 [2 [3]]] [2 [3]] [3]]
       [[1 [2 [3]]] [2 [3]] [3] 3])

  )