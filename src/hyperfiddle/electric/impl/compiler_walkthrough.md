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
- electric prefers defns since
  - it generate smaller code which should be faster
  - cljs macros expand to `js*` which would break under electric

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
- `::lookup` - electric vars are dynamic and can be rebound. This node type
  is a lookup into the dynamic binding of the vars. We allow binding through
  non-symbolic keys, e.g. we use keywords for some private bindings, and numbers
  for passing positional arguments.
  
`analyze-electric` takes the output of `analyze` and performs deeper analysis
and graph rewrites. The current passes are:
- `mark-used-ctors` - marks and orders all used constructors (e/fns desugar to
  ctors). We perform DCE, e.g. in (let [x 1, y (e/ctor 1)] x) we won't compile the ctor.
- `rewrite` calls to mark which ctor they belong to
- `index-calls` assigns an ordering index to each call
- `reroute-local-aliases` - if a local just aliases another one, reroutes the
  references to the origin. E.g. a similar clojure pass would rewrite
  `(let [x 1, y x] [y y])` to `(let [x 1] [x x])`.
- `locals` - walking the code, finding all localrefs, decides whether the
  locals need to become runtime nodes. The compiler aggressively inlines when
  possible. This pass also has to handle closed over references (free variables).
- `inline-locals` - inlines locals 
- `inline-literals` - if a node has a literal value (keyword, string, number etc) we can
  inline the node, potentially saving needless roundtrips
- `rewrite ap-of-pures` - `(r/ap (r/pure x) (r/pure y) (r/pure z))` can optimize to 2 cases:
  - `(r/ap (r/pure (fn* [] (x y z))))` if `x` is an impure fn
  - `(r/pure (x y z))` if `x` is a pure fn
  This pass handles both cases. We list pure fns in a hash-map.
  Additionally an `r/ap` can have *some* arguments as `r/pure`. If it's more than 1 we rewrite
  it to an `init-fn` inlining the pures. `(r/ap (r/pure +) (r/pure 3) foo)` becomes roughly
  `(let [init-fn (fn [x] (+ 3 x))] (r/ap (r/pure init-fn) foo))`
- `inline-return-nodes` - sometimes we compile the return flow to a node and reference it.
  If this is the only reference it's wasted abstraction, so we inline it.
- `order-nodes` - orders nodes based on causal ordering
- `order-frees` - orders frees based on causal ordering
- `expand-cannot-resolve` - if a symbol like `foo` was unresolvable we resolved it to `r/cannot-resolve`.
  Here we expand it to `(r/cannot-resolve-fn 'foo)` so the runtime exception is more informative.

## Foreign code analysis

Foreign clojure(script) code, e.g. `cc/fn` or `cc/letfn` may contain references
to electric locals and vars. To find them and compile correct code we need to
analyze these as well. These require different expansion, analysis and emission,
so we have separate functions for them: `-expand-all-foreign`,
`analyze-foreign`, `emit-foreign` and finally `wrap-foreign-for-electric` which
the electric analyzer calls. These work largely the same but cater to interop
needs. A major difference is the triple store now encodes ordering instead of
relying on `:db/id` order in the store, which proved to be problematic for graph
rewrites. The keys are shorter too, `::u` for unique id, `::p` for parent,
`::pi` for parent index (the discussed ordering).

With a bit of effort this analysis chain could be generalized and extracted into
a separate clojure(script) library for clj(s). The triple store lends itself
great for analysis and rewrites.

## Emitter

This is the simplest part of the compiler. It takes the final triple store as
input and has a straightforward mapping from the graph to the final runtime code.
`emit` is the main var which is currently ~40 lines of simple code. `emit-ctor`
is the glue which ties together `emit` and other parts of the emitter to
generate code for a single ctor.

## Compiler knobs

The compiler understands several keyword metadata on `e/defn`s or in the opts map of `e/single` and `e/local`. One can use these to debug the compilation pipeline.

### `::print-expansion`

Pretty prints the macroexpansion. Good for checking how we expand and convert clojure source code to electric primitives. Note that some forms might re-expand in the analysis phase, i.e. this is fully macroexpanded code but might not be broken down to electric primitives only.

### `::print-analysis`

Pretty prints the triple store after the initial, bare analysis. At this point the store is big but there weren't tricky optimizations happening. This is a good place to check the state of the triple store if one was doing changes in the `analyze` phase.

### `::print-db`

Pretty prints the final triple store, after all passes. Due to the optimization passes this store should be smaller/simpler. Since the passes are the trickiest, they might contain the most bugs. This view has been helpful in checking the state of the triple store when modifying the passes.

### `::print-clj-source` / `::print-cljs-source`

Pretty prints the final source code, the electric runtime code.

### `::trace` / `::e/trace`

Wraps all interop (clj/cljs) invocations with a trace logger that prints to stdout the ns/line/column/form and the returned value. Has a hardcoded blacklist for runtime3, missionary and incseq.
