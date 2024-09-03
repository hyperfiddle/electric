(ns hyperfiddle.domlike "
A mutable tree implementation with an API isomorphic to a subset of the DOM.
")

(defn node "
Return a fresh node.
" []
  (doto (object-array 3)
    (aset 2 [])))

(defn parent "
Return `node`'s current parent.
" [^objects node]
  (aget node 0))

(defn set-parent "
Assign `node`'s parent to `parent`.
" [^objects node parent]
  (aset node 0 parent))

(defn index "
Return `node`'s current index.
" [^objects node]
  (aget node 1))

(defn set-index "
Assign `node`'s index to `index`.
" [^objects node index]
  (aset node 1 index))

(defn children "
Return `node`'s current children.
" [^objects node]
  (aget node 2))

(defn set-children "
Assign `node`s children to `children`.
" [^objects node children]
  (aset node 2 children))

(defn nth-child "
Return `node`'s child in position `i`, or `nil` if out of bounds.
" [node i]
  (nth (children node) i nil))

(defn remove-at [node i]
  (let [v (children node)]
    (set-children node
      (into (subvec v 0 i)
        (map (fn [c] (set-index c (dec (index c))) c))
        (subvec v (inc i))))))

(defn remove-child "
Remove `child` from `node`'s children and return the removed node.
" [node child]
  (when-not (identical? node (parent child))
    (throw (#?(:clj Error. :cljs js/Error.) "not a child")))
  (remove-at node (index child))
  (set-parent child nil)
  (set-index child nil)
  child)

(defn replace-child "
Replace `old` by `child` in `node`'s children and return the removed node.
" [node child old]
  (when-not (identical? node (parent old))
    (throw (#?(:clj Error. :cljs js/Error.) "not a child")))
  (when-some [p (parent child)]
    (remove-at p (index child)))
  (set-parent child node)
  (set-index child (index old))
  (set-children node
    (assoc (children node)
      (index old) child))
  (set-parent old nil)
  (set-index old nil)
  old)

(defn insert-before "
Insert `child` before `sibling` in `node`s children and return the added node.
" [node child sibling]
  (when-not (nil? sibling)
    (when (identical? child sibling)
      (throw (#?(:clj Error. :cljs js/Error.) "insert before self")))
    (when-not (identical? node (parent sibling))
      (throw (#?(:clj Error. :cljs js/Error.) "not a child"))))
  (when-some [p (parent child)]
    (remove-at p (index child)))
  (let [v (children node)
        i (if (nil? sibling)
            (count v)
            (index sibling))]
    (set-parent child node)
    (set-index child i)
    (set-children node
      (-> []
        (into (subvec v 0 i))
        (conj child)
        (into (map (fn [c] (set-index c (inc (index c))) c))
          (subvec v i))))
    child))

(defn append-child "
Adds `child` at the end of `node`'s children and return the added node.
" [node child]
  (insert-before node child nil))

(defn tree "
Return a snapshot of the tree rooted at `node`.
" [node] (into [node] (map tree) (children node)))