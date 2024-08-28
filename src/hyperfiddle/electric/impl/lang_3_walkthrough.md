# Compiler walkthrough

The electric compiler has 3 major components - expander, analyzer and emitter.
Each stage has its own complications and separating them aids in debugging and
reasoning. The final `compile` var calls them in the correct order to generate
electric runtime code.

## Expander

Expanding correctly across both clj and cljs is tricker than it should be.
Having it as a separate phase helped honing in on the differences and finding
the best solutions.

The expander expands all macros to electric built-ins. The analyzer can use the
expander to re-expand a built-in to a set of other built-ins.

Electric Clojurescript macroexpansion is different from stock expansion:
- cljs allows (defn foo) and (defmacro foo) to live alongside, since macroexpansion happens in a separate stage.
- cljs prefers the macro version wherever it can since it generates code GCC can better optimize.
- electric prefers defns since it generate smaller code which should be faster.

For this reason we have our own analyzer. It's not a full analyzer, just enough to find macros and vars.

We want to be able to source map electric code and clojure macroexpansion
doesn't forward line/column information since it's stored in the seq's metadata.
The expander takes care to forward metadata on re-expansion so we can later
source map.

## Analyzer

The analyzer is the hardest stage of all. It has to take in the expanded user
code and figure out what electric code we need to generate.

Since the analyzer is changing all the time, is complicated and I didn't
know/understand all of its requirements I chose to keep all information in a
single triple store. Using a triple store allows working with the data flexibly
and in multiple passes. I built my own simple triple store to gain speed and
customize it to my needs. The triple store has 3 parts

- o - an options map that can carry arbitrary extra data
- eav - the main index, for `{:db/id 1, :foo :bar}` it looks like `{1 {:db/id 1, :foo :bar}}`,
  i.e. we can get our hands on the inserted map through a single map lookup
- ave - the key-value index which allows traversing the graph in arbitrary ways.
  For `:foo` from the map before it looks like `{:foo {:bar (sorted-set 1)}}`. The
  sorted set is (ab)used in the analyzer to keep track of node ordering in the graph.
  
There's 4 main keys we use:
- `:db/id`, used by the triple store internally, as the entity key. We refer to
  this value as `e` in the codebase. Function returning the entity ID end with
  a `-e` suffix.
- `::type`, to categorize the nodes.
- `::parent`, a universal backreference key, holding the parent's `:db/id` value.
  This allows traversing the graph both ways easily. Reading it we can go to the
  parent, querying it in the :ave index we can find all children. Since the :ave
  index uses a sorted set for the values we get the ordering for free, provided
  the children's `:db/ids` are sorted. This is a strength during initial analysis
  and poses some problems when doing graph rewrites, as one has to take care to
  preserve the ordering during rewrites.
- `::uid`, used as a universal, unchanging ID. When I started implementing graph
  rewrites I realized backreferences can get stale. Instead of meticulously
  updating all of them I decided to create this unchanging ID which survives all
  rewrites.

The analyzer uses `->id` and `->uid` to generate a monotonically-increasing
integer. Together with the triple store's sorted maps we get node ordering for
free.

The analyzer operates in multiple passes over the triple store. The first pass
is `analyze`, which takes the expanded user code, potentially re-expands some
forms and produces the first triple store. There are some non-obvious node types:
- `::mklocal` and `::bindlocal` - `let` expands to these, but also `e/letfn` uses
  these. `::mklocal` introduces a local and `::bindlocal` binds it. Separating the
  creation and binding of the local allows circular and forward references. E.g.
  in `e/letfn` if one defines `Foo` and `Bar` we can first introduce the 2
  locals through `::mklocal` and bind them with `::bindlocal` afterwards.
- `::localref` - a reference to an electric local. E.g. the returning `x` in `(let [x 1] x)`.
- `::lookup` - in electric all vars are dynamic and can be rebound. This node type
  is a lookup into the dynamic binding of the vars. We allow binding through
  non-symbolic keys, e.g. we use keywords for some private bindings and numbers
  for passing positional arguments.
  
`analyze-electric` takes the output of `analyze` and performs deeper analysis
and graph rewrites. The current passes are:
- `compute-effect-order` - reachable nodes get an ::fx-order key with an
  increasing integer value denoting their evaluation order. The ordering is
  later used to generate side effecting code in correct order as required by the
  runtime.
- `mark-used-ctors` - marks and orders all used constructors (e/fns desugar to
  ctors). Used means we perform DCE, e.g. in (let [x 1, y (e/ctor 1)] x) we
  won't compile the ctor.
- `mark-used-calls2` - inside the marked ctors, marks and order all calls. It's
  safe to mark inside ctors since calls can't happen outside of a ctor.
- `reroute-local-aliases` - if a local just aliases another one, reroutes the
  references to the origin. E.g. a similar clojure pass would rewrite
  `(let [x 1, y x] [y y])` to `(let [x 1] [x x])`.
- `optimize-locals` - walking the code, finding all localrefs, decides whether the
  locals need to become runtime nodes. The compiler aggressively inlines when
  possible. This pass also has to handle closed over references (free variables).
- `inline-locals` - inlines locals 
- `order-nodes` - orders nodes based on compute-effect-order ordering
- `order-frees` - orders frees based on compute-effect-order ordering
- `collapse-ap-with-only-pures` - `(r/ap (r/pure x) (r/pure y) (r/pure z))` can
  optimize to 2 cases:
  - `(r/ap (r/pure (fn* [] (x y z))))` if `x` is an impure fn
  - `(r/pure (x y z))` if `x` is a pure fn
  This pass handles both cases. We list pure fns in a hash-map.

## Emitter

This is the simplest part of the compiler. It takes the final triple store as
input has a straightforward mapping from the graph to the final runtime code.
`emit` is the main var which is currently ~40 lines of simple code. `emit-ctor`
is the glue which ties together `emit` and other parts of the emitter to
generate code for a single ctor.
