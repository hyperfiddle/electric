(ns geoffrey.editscript
  (:require [minitest :refer [tests]]))

;;;* Assessing EditScript
;;;
;;; [[https://github.com/juji-io/editscript][EditScript]] is a diffing/patching library.
;;; It's clj/cljs compatible.
;;;
;;; Here is [[https://www.youtube.com/watch?v=n-avEZHEHg8][the talk]].
;;;
;;;** Key Points
;;;
;;;*** No shallow diffing
;;;
;;;    There's a [[https://github.com/juji-io/editscript/issues/7][PR]] opened for this missing feature. This is problematic to us
;;;    as we mostly don't need deep diffing.
;;;
;;;*** Not optimized for humans
;;;
;;;    Diffs are meant to be either as fast as possible, or as precise as
;;;    possible. Human readability of the diff data structure is not a concern.
;;;    A diff can still be rendered as a vector of edits for the REPL, but you
;;;    need to be a human parser/interpreter. I guess it's fine.
;;;
;;;*** Diffable types are explicit
;;;
;;;    Not all traversables are diffable, By default `IPersistentList`, `Map`,
;;;    `Vector`, `Set` are. Everything else is considered atomic values. You can
;;;    `extend-protocol Diffable` to flag a type as diffable. This is great
;;;    because it won't diff Strings, Datomic Entities and custom types by
;;;    default.
;;;
;;;*** CPU or RAM, pick one
;;;
;;;    EditScript provides two algorithms: `A*` and `quick`.
;;;
;;;    - quick :: fast but produce suboptimal diffs (noops, pairs of edits that
;;;      could cancel out, extra work for patch). Fast to diff, slower to patch,
;;;      bigger in RAM and on the network.
;;;    - A* :: will produce optimal diffs but is slower. Diffs will be minimal,
;;;      costly to compute, but fast to patch and transmit. `A*` is the default
;;;      algorithm.
;;;
;;;    [[https://github.com/juji-io/editscript#shopping-alternatives][Benchmark]].
;;;
;;;    In the end, the slowest thing we can't improve is the network, so I would
;;;    go for `A*` to:
;;;    - minimize network payloads,
;;;    - clearer debug traces,
;;;    - consistent patch cost.
;;;
;;;*** Optional String diff
;;;
;;;    EditScript don't diff strings by default, but an option exists to enable
;;;    it. It will diff strings only if they differ of at most ⅓ in size. This
;;;    is compelling for typeaheads and edit boxes.
;;;
;;;*** EditScripts forms a monoid
;;;
;;;    Terminology: an editscript is a diff value, represented by an IEditScript
;;;    instance. It can be created from a vector using the `edit->script` fn.
;;;
;;;    `editscript.core/combine` is mappend for diffs. It is not a mere concat,
;;;    it merges them properly, canceling out etc….
;;;
;;;*** EditScripts are inspectable
;;;
;;;    An EditScript instance provide interesting functions:
;;;    - get-size :: you might want to `combine` small diffs before patching,
;;;    - edit-distance :: if the distance between `a` and `b` is too big, you
;;;      might want to just replace `a` by `b`,
;;;    - get-edits :: gives you a vector of edits at the REPL,
;;;    - get-adds-num :: and its peers `get-dels-num` and `get-reps-num` informs
;;;      you about the number of adds, rets and replacements to perform,
;;;      respectively.
;;;
;;;** Opinion
;;;
;;;    It seems to be the most complete one in the Clojure ecosystem.
;;;
;;;    Looking at the implementation and comparing it with other diffing
;;;    libraries, I think I should spend a little bit of time adding shallow
;;;    diffing to it and it would be perfect for our use case.
