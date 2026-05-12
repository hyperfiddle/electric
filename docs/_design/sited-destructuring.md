# Frame

## Situation
Destructuring in Electric `let` inherits the call-site for the generated `nth`/`get`/`first`/`next` calls.
The accessors run wherever the binding form is evaluated, regardless of where the init expression's value lives.

## Complication
`(e/client (let [[a b] (e/server [(Object.) (Object.)])] …))` breaks.
The `(nth x 0 nil)` for `a` runs on the client; `x` is a server-only, non-serializable Object; transfer throws.

## Question
Add a simple, explicit annotation to force the destructuring accessors onto a specific peer, ahead of a general destructuring overhaul.

## Constraints
- Annotation on the toplevel destructuring pattern only.
- Symmetric: both `^::e/server` and `^::e/client`.
- All pattern shapes (sequential, associative, `:keys`/`:syms`/`:strs`, `:as`, `:or`, `&`).
- Annotation sites the accessors and the resulting locals — there is no other siting to do.
- Unannotated pattern: behavior unchanged (inherits call-site).
- Symbol pattern (`^::e/server x`): noop (no accessors generated).
- Both annotations on one pattern: compile error.

## Non-goals
- General destructuring overhaul.
- Inferred siting from init-expression shape (e.g. wrapping in `(e/server …)`).
- Nested-pattern siting (toplevel only; nested accessors inherit toplevel site).
- Site-aware destructuring for `loop`, `binding`, `e/fn`/`e/defn` params, `e/for`, `for-by`, `letfn` — maybe in a future iteration.

# Research

## Destructure code path
`electric/src/hyperfiddle/electric/impl/destructure.cljc` exports `destructure*`.
`pvec` generates `(nth gvec n nil)` and the rest-seq `first`/`next` calls.
`pmap` generates `(get gmap bk …)` and `(r/get-destructure-map gmap)`.
All accessor calls converge in this one file — single chokepoint.

## Where binding forms call destructure
Only `clojure.core/let` (and `cljs.core/let`) trigger `destructure*`.
Callsites: `lang3.clj:197` (`-expand-all-foreign`), `lang3.clj:312` (`-expand-all`).

## Site representation
Internal AST form: `(::lang/site site bform)` handled at `lang3.clj:823`.
`(e/server body)` macroexpands to `(::lang/site :server body)` (`electric3.cljc:206`); `e/client` symmetric (line 205).
The handler updates `::current` in the analyzer env so child nodes inherit the site.

## Reader behavior
`^::e/server [a b]` resolves at read time via the user-ns's `:as` alias for `hyperfiddle.electric3`.
Metadata lands on the vector pattern; verified: `(meta (read-string "^:e/server [a b]"))` → `{:e/server true}`.

# Design

## Locked choices

| Decision                        | Choice                                                                                   | Reason                                                                                                                                                              |
|---------------------------------|------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Annotation form                 | `^::e/server` / `^::e/client` metadata on the toplevel pattern                           | User pivoted to metadata after considering then rejecting init-expression inference; metadata works for the `let` case where init may not be syntactically wrapped. |
| Binding forms covered           | `let` only                                                                               | Other binding forms in a future iteration, perhaps.                                                                                          |
| Site threading                  | B2 — `destructure*` reads pattern meta itself                                            | `destructure.cljc` is already Electric-specific (requires `runtime3`), so no module-leakage concern. Keeps the change in one file.                                  |
| Wrap scope                      | C1 — accessor calls only (`nth`, `first`, `next`, `seq`, `get`, `r/get-destructure-map`) | Electric transfers values across sites automatically at access; wrapping the init RHS would risk overriding user-sited inits.                                       |
| Recognized key                  | D1 — only `:hyperfiddle.electric3/server` / `:hyperfiddle.electric3/client`              | Strict, one canonical form; avoids typo attack surface.                                                                                                             |
| Nested-pattern site             | F1 — toplevel annotation propagates to all accessors generated for that pair             | Coheres with the spec's "site the destructure as a unit".                                                                                                           |
| Symbol-only pattern             | Noop — no accessors to site, annotation silently ignored                                 | Matches spec §2.4.                                                                                                                                                  |
| Both annotations on one pattern | Compile-time error from `destructure*`                                                   | Fail loudly; avoid silent precedence rule.                                                                                                                          |

## Rejected paths

- **Init-expression inference** (detect `(e/server …)` syntactically at init position and site the destructure to match): seemed simpler but breaks for `fn`/`defn` params (no init in form) and for inits sited indirectly through macros. Reverted to metadata after this gap surfaced.
- **B1** (expander splits per-pair, calls `destructure*` once per pair): more callsite changes for no benefit.
- **B3** (parallel `[site site …]` vector): awkward API.
- **C2** (also wrap `gvec`/`gmap` init binding RHS): risks overriding user-sited inits.
- **D2** (also accept short `:server`/`:client`): adds typo surface.
- **F2** (nested accessors stay at call-site): re-creates the original bug one level in.

# Spec

## In scope
- Annotation on toplevel pattern only.
- Symmetric `^::e/server` and `^::e/client`.
- All pattern shapes.
- Unannotated path unchanged.

## Out of scope (won't)
- General destructuring overhaul.
- Implicit siting from init shape.
- Nested-pattern siting.
- New diagnostics for site mismatch (matches existing behavior — `try`/`catch` is TODO in Electric).
- Coverage of binding forms that don't destructure today.

## Tasks
- 4.1 `destructure.cljc` reads pattern meta at the toplevel pair (`process-entry`).
- 4.2 Site threaded through `pb` → `pvec` / `pmap` so nested accessors inherit it.
- 4.3 Local `wrap` helper emits `(::lang/site site form)` when site non-nil, else identity.
- 4.4 Wrapped calls: `(seq gvec)`, `(first gseq)`, `(next gseq)`, `(nth gvec n nil)`, `(r/get-destructure-map gmap)`, `(get gmap bk)` / `(get gmap bk default)`.
- 4.5 Mutex check in `pattern-site` throws `Exception` (clj) / `js/Error` (cljs) when both keys are truthy.
- 4.6 `:as-alias` for `hyperfiddle.electric.impl.lang3` (avoids cycle with `lang3` which requires `destructure`).
- 4.7 No changes to `lang3.clj`.

## Verification
Six tests in `electric/test/hyperfiddle/electric3_test.cljc` (after line 1076):
1. Sequential `^::e/server` destructure of server-only Objects.
2. Sequential `^::e/client` destructure of client-only Objects.
3. Associative `^::e/server` (`:keys`) destructure.
4. Nested pattern under `^::e/server` (site propagates inward).
5. Unannotated destructure regression guard.
6. Mutex compile-time error (`:clj`-gated via `eval`).

Non-serializable values written as `#?(:clj (Object.) :cljs (js/Object.))` for `.cljc` portability.

# Code

## Files changed
- `electric/src/hyperfiddle/electric/impl/destructure.cljc` — `pattern-site` helper added; `pb` takes a `site` arg; `pvec`/`pmap` accessors wrapped via local `wrap`; `:as-alias` for `lang3`.
- `electric/test/hyperfiddle/electric3_test.cljc` — six `(tests …)` blocks appended after the existing `fn destructuring` test.

## Verification performed
- All six new tests pass individually on the JVM nREPL.
- Existing destructure tests (`Sequential destructuring`, `Associative destructuring`, `Associative destructuring with various keys`, `fn destructuring`) still pass.
- `electric/test/hyperfiddle/electric/impl/compiler_test.cljc` reload — all-green.
- Full CI-mode CLI run not performed in-session.

## Known follow-ups (not blocking this change)
- `destructure.cljc` short-circuits when every pattern is a symbol, silently dropping `^::e/server` on symbol patterns. Matches spec §2.4 but worth a one-line comment for future readers.
- pvec `:as v` binds the original (unsited) value; pmap `:as m` binds the sited normalized map (`r/get-destructure-map gmap`). Pre-existing asymmetry, not introduced here.
- Each accessor wrapped in `(::lang/site …)` triggers an Electric transfer when the init lives at a different site than the destructure site. Free in the motivating case (server-sited init + `^::e/server`); pays a transfer per accessor in cross-site cases.
