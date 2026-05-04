# Frame

## Situation
hyperfiddle-agent is a backend-only Maven artifact (`com.hyperfiddle/hyperfiddle-agent`).
It transitively pulls `org.clojure/clojurescript` through `com.hyperfiddle/hyperfiddle` → `com.hyperfiddle/electric`.
Electric's `e/defn` macroexpansion invokes `cljs.analyzer` to walk embedded `e/client` forms.

## Complication
A consumer JVM with an arcane ClojureScript on its classpath (typically pinning old Guava) can crash the analyser when hyperfiddle-agent loads.
The crash has not yet been reproduced locally, so any fix is currently unverifiable.

## Question
Make hyperfiddle-agent's published jar resilient to arcane ClojureScript on the consumer classpath, by AOT-compiling it so analyser invocation happens at build time and the runtime ships only `.class` files.

## Constraints
The deliverable is the published `com.hyperfiddle/hyperfiddle-agent` jar.
The fix must not require consumers to alter their CLJS dependency.
Verification requires a reproduction: a JVM that loads hyperfiddle-agent with an arcane CLJS pinned, observed to crash pre-fix and pass post-fix.
Selective per-namespace AOT is not required — full AOT is acceptable as long as the runtime works.

## Non-goals
Selective per-namespace AOT.
Lazy CLJS namespace loading (separate, parallel workstream — Electric not analysing the entire classpath).
Dev-time / browser-time CLJS handling.
Fixing arcane CLJS / Guava conflicts on consumer classpaths.

## Tasks
Task 1 — Reproduction (analyzer-run-time mode). Build a minimum failing setup pinning an arcane CLJS version against the current hyperfiddle-agent and capture the crash. Done — see Findings.
Task 1.5 — Reproduction (require-time / Guava ABI mode). Reproduce Dustin's `NoSuchMethodError` at `cljs_analyzer2.clj:1:1` ns-form macroexpansion. Candidate vectors: `traversy/traversy 0.5.0` pin, forced Guava version.
Task 2 — Fix. AOT compile the agent + lift cljs.* requires out of ns forms at four sites + delay-gate `lang3.clj:77`'s eager `!a` construction + shape the published POM to exclude / scope-`provided` CLJS.

Both modes now reproduced — Task 2 design begins next. Verification requires both modes crash pre-fix and pass post-fix.

# Research

## Dep chain to ClojureScript
hyperfiddle-agent → `com.hyperfiddle/hyperfiddle` → `com.hyperfiddle/electric` → `org.clojure/clojurescript {:mvn/version "1.11.121"}`.
Source: `electric/deps.edn:22`.
Therefore any consumer of the published agent jar pulls a CLJS coordinate transitively unless they exclude it.

## Where `cljs.analyzer` is loaded
`hyperfiddle.electric.impl.lang3` requires `cljs.analyzer`, `cljs.analyzer.api`, `cljs.env` at the top of its `ns` form.
Source: `electric/src/hyperfiddle/electric/impl/lang3.clj:3-5`.
`hyperfiddle.electric3` requires `lang3` unconditionally (no `:as-alias`).
Source: `electric/src/hyperfiddle/electric3.cljc:4`.
Agent prod and runtime namespaces require `hyperfiddle.electric3`.
Sources: `hyperfiddle-agent/src/hyperfiddle/navigator_agent_entrypoint.cljc:5`, `hyperfiddle-agent/src/hyperfiddle/navigator_agent.clj:29`, `hyperfiddle-agent/src-prod/prod.cljc:16`.
On a consumer JVM, loading any agent entrypoint triggers `(require 'cljs.analyzer)` at namespace-load time, independent of macroexpansion.

## Where `lang3` is `:as-alias` only
`runtime3.cljc:10` and `missionary_util.cljc:5` reference lang3 via `:as-alias` (no load).
`electric3.cljc:4` and `electric_local_def3.cljc:10` are real loads.
lang3 is therefore reachable from runtime ns load through `electric3` itself.

## Build tooling has no AOT step today
`build/src/hyperfiddle/build.clj` produces a thin jar via `tools.build/copy-dir` + `tools.build/jar` — source files only.
`hyperfiddle-agent/src-build/build.clj` uses the same `build` for the Maven artifact and `tools.build/uber` for the Fly.io uberjar — neither calls `tools.build/compile-clj`.
Adding AOT requires inserting a `compile-clj` invocation and pointing the jar at the resulting class-dir.

## Published artifact vs. internal uberjar
The Maven artifact (`com.hyperfiddle/hyperfiddle-agent`) is the thin jar produced by `clj -X:build build`.
The Fly.io uberjar (`uberjar` task) is internal infrastructure and not the deliverable.
Only the thin-jar path is what consumers receive.

## Unknowns

Where in the cljs-load lifecycle the crash actually happens — unknown.
If the failure is at `cljs.analyzer` require time (a static call into Guava during ns load), AOT alone is *not sufficient*: the consumer JVM still loads `cljs.analyzer` because lang3's required-namespaces list survives AOT.
If the failure is only when the analyser *runs* (i.e. processes forms during `e/defn` macroexpansion), AOT *is* sufficient: agent macroexpansion happens at AOT time on a controlled build classpath, and the consumer JVM never invokes the analyser.
Task 1 (repro) will distinguish these.

What "arcane" CLJS version reproduces the crash — unknown.
Codebase pins `1.11.121` for development; no known-bad pair recorded.
Two candidate strategies for the repro: pin an old CLJS (e.g. 1.10.x) or pin an old direct Guava (`com.google.guava/guava`).

Whether AOT requires shipping `cljs.analyzer` classes inside the agent jar — unknown.
If require-time loading of `cljs.analyzer` is itself the failure point, candidate moves are: ship CLJS classes inside the agent jar so consumer-classpath order is irrelevant; eliminate the require of lang3 from electric3's runtime path; shade/relocate.
None of these are pure AOT and may be deferred to Task 2 design once the repro lands.

Whether AOT'ing the agent has known-incompatible code paths — unknown, not investigated.

# Design

## Factors
Factor A — Pinning vector: what we mutate on the consumer classpath. Values considered: old CLJS pinned, old direct Guava, new direct Guava.
Factor B — Trigger depth. B1 = `(require 'hyperfiddle.navigator-agent-entrypoint)`. B2 = B1 + macroexpand `(e/defn Repro [] (e/client 1))`. B3 = boot agent main.
Factor C — Observation criterion: any throw, Guava-shape stack, customer-stack match.
Factor D — Housing: scratch fixture, standalone repo, alias inside hyperfiddle-agent.
Factor E — Search strategy: linear sweep, bisect, Guava-anchor jumps, calendar-anchor jumps, customer mirror.

Couplings: A↔E (E is the search over A). B↔C (Guava-shape only meaningful given a stack from B). B is load-bearing — only B1 vs B2 separates require-time from analyse-time crash.

## Candidates considered
Cand-1: CLJS-version sweep with B-ladder.
Cand-2: Guava-direct override sweep.
Cand-3: Customer mirror against a captured stack.
Cand-4: Newer-Guava override (steel-man of the inverse hypothesis).
Cand-5: Bug-tracker / Slack search before any synthetic sweep.

## Rejections
Cand-2, Cand-4 — Guava axis: user judged old-CLJS-pinning the more typical production failure mode.
Cand-3 — customer mirror: no captured stack available.
Cand-5 — internal-channel search: same; treated as escalation backup only.

## Choice
Cand-1 — CLJS-version sweep at yearly cadence backward from the current Electric pin (1.11.121, 2023-09-01), B1→B2 ladder, auto-narrow to date-midpoints on clean exhaustion.
Housing inside `hyperfiddle-agent/deps.edn` as alias `:repro-arcane-cljs`; driver as `bb` script in `src-build/`.
Repro is mandatory — no negative terminal state; clean exhaustion escalates to ladder rung 2 (Guava axis re-admitted) or rung 3 (internal-channel search).

# Spec

## Goal
Deterministic, re-runnable artifact that captures the crash, sufficient to gate Task 2 and verify it post-fix.

## Deliverables built
`.gitignore` — `hyperfiddle-agent/.repro-arcane-cljs-logs/` (captured stacks, not git-tracked).
Consumer fixture at `hyperfiddle-agent/consumer-repro/deps.edn` — consumer-of-jar setup, depends on the locally-installed `com.hyperfiddle/hyperfiddle-agent {:mvn/version "v0-alpha-SNAPSHOT"}` with `:cljs-current` and `:cljs-arcane` aliases (originally lived at `~/tmp/agent-consumer-repro/`; relocated into the monorepo so the deliverable is self-contained — see Resolution).

The original yearly-sweep driver (`hyperfiddle-agent/src-build/repro_arcane_cljs.bb`) and its companion alias `:repro-arcane-cljs` were used to discover the arcane-CLJS witness (CLJS 1.10.773 / 1.10.520 — see Findings §Witness) and removed once the witness was baked into the consumer fixture as a single-version `:cljs-arcane` probe.

## Acceptance
Witness captured at `hyperfiddle-agent/.repro-arcane-cljs-logs/1.10.773-B1.log` (gitignored) — see Findings §Witness for the stack and §Crash window for the version sweep that found it.
Current verification criteria (post-fix) live in Resolution §Acceptance — measured.

## Non-goals preserved from Frame
Selective per-namespace AOT.
Lazy CLJS namespace loading.
Dev-time / browser-time CLJS handling.
Fixing arcane CLJS / Guava conflicts on consumer classpaths.
Re-architecting how Electric loads `cljs.analyzer`.

# Findings

## Witness
First crash on yearly sweep at CLJS 1.10.773 (2020-05-26), trigger B1.
Stack: `:cannot-expand [cljs.core] (#'cljs.core/and (exists? js/Symbol) ...)` then `:failed-to-analyze [cljs.core]` then `NullPointerException at cljs.analyzer/excluded? (analyzer.cljc:3813)` — `Future.get()` invoked on a null `fut`.
Manual smoke at CLJS 1.10.520 reproduces identically.
Captured at `hyperfiddle-agent/.repro-arcane-cljs-logs/1.10.773-B1.log` (gitignored).

## Crash window
Clean: 1.11.121 (2023-09-01, baseline), 1.11.60 (2022-06-24), 1.10.879 (2021-07-19).
Crash: 1.10.773 (2020-05-26), 1.10.520 (2019-02-14).
Boundary lies in (2020-05-26 .. 2021-07-19]. Yearly sweep hit on third probe; auto-narrow code path therefore did not execute.

## Mechanism — answers Research §Unknowns line 66-69
The crash is at analyser **run-time**, not analyser require-time.
`:cannot-expand [cljs.core]` and `:failed-to-analyze [cljs.core]` are emitted by `cljs.analyzer` while *running* on `cljs.core` forms — `excluded?` is invoked during analysis of `(cljs.core/and ...)`.
The analyser runs because the agent's namespace-load chain includes `e/defn` forms whose macroexpansion invokes the analyser on embedded `e/client` content; that analysis pulls in `cljs.core` macros and trips the NPE in old CLJS.
This is the second branch of Research line 68: "If the failure is only when the analyser runs, AOT is sufficient."

## Implication for Task 2
Superseded — see "## Implication for Task 2, revised" below; Dustin's stack reveals a require-time / Guava-ABI mode this conclusion does not cover.

AOT alone is sufficient.
At AOT build time on a controlled classpath (current pinned CLJS), all `e/defn` macroexpansions complete and emit class bytecode before publication.
The consumer JVM loads the AOT'd jar and executes pre-expanded bytecode; the analyser never runs on the consumer classpath.
The consumer's arcane CLJS sits on classpath but is never exercised by agent ns-load.
Task 2 does **not** require shipping `cljs.analyzer` classes inside the agent jar, eliminating `lang3`'s require from `electric3`, or shading.

## Jar-shape verification
Bug shape is identical between source-on-classpath and jar-on-classpath consumer setups, confirming the published artifact (not a dev-tree artefact) is what fails.
Consumer fixture (`hyperfiddle-agent/consumer-repro/`) under current CLJS: B1 succeeds; `clojure -M -m prod http-port 18080` boots Jetty (`curl localhost:18080` returns HTTP 404).
Same fixture under arcane CLJS 1.10.520: same NPE at `cljs.analyzer/excluded?` reproduces at both B1 and Bprod (crashes before Jetty start).

## Base-setup viability for Task 2
`(require 'dev)` and `(require 'prod)` both load cleanly under current pin.
`clj -X:build jar` produces `target/hyperfiddle-agent-v0-alpha-SNAPSHOT.jar` (559K thin jar, 26 entries, 0 warnings) — Task 2's AOT build can piggyback on this path.
shadow-cljs license gate fires on `:watch` (development), not `:release` (production); Task 2's `compile-clj` invocation is unaffected.
`clj -A:dev (dev/-main)` is blocked on Electric license browser-activation; not relevant to Task 2 since the build path is `:release`.

## Side notes
B2's actual analyser invocation is unverified — across the version sweep that produced the witness, every B2 ran on a clean CLJS where it succeeded; we have no positive evidence `(e/defn Repro [] (e/client 1))` exercises the analyser deeper than B1 does. Moot for the verdict above (B1 already crashes at analyser-run-time).
`prod/-main`'s `log/info` lines emit nothing visible in the consumer-of-jar setup; only the shutdown hook's `println` is seen. Jetty is genuinely up (curl confirms). Pre-existing observability gap, not caused by Task 1 work.

## Second failure mode — Dustin's stack
Failure mode 2 is require-time of cljs-analyzer2, distinct from the analyzer-run-time witness at CLJS 1.10.773.
Stack: `NoSuchMethodError com.google.javascript.jscomp.deps.ModuleLoader/createRootPaths` requiring `java.util.stream.Collector com.google.common.collect.ImmutableSortedSet.toImmutableSortedSet(java.util.Comparator)`.
Site: `Syntax error macroexpanding at (hyperfiddle/electric/impl/cljs_analyzer2.clj:1:1)` — the `ns` form.
Mechanism: Guava ABI mismatch in CLJS internals (Closure Compiler `ModuleLoader`).
Suspected vector per Dustin: two CLJS versions on the consumer classpath with conflicting Guava transitives, or a pinned direct Guava.
Reproduced locally — see `## Witness — Guava ABI mode` below.

## Witness — Guava ABI mode
Failure mode 2 reproduced on first probe at Guava 20.0 + CLJS 1.11.121 (current pin), trigger B1.
Site: `Syntax error macroexpanding at (hyperfiddle/electric/impl/cljs_analyzer2.clj:1:1)`.
Error: `NoSuchMethodError com.google.javascript.jscomp.deps.ModuleLoader/createRootPaths` requiring `java.util.stream.Collector com.google.common.collect.ImmutableSortedSet.toImmutableSortedSet(java.util.Comparator)`.
Stack file: `hyperfiddle-agent/.repro-guava-abi-logs/guava-20.0-B1.log` (gitignored).
Classpath shape: jar-on-classpath — agent consumed as `com.hyperfiddle/hyperfiddle-agent {:mvn/version "v0-alpha-SNAPSHOT"}` from local Maven repo, matching the deliverable.

## Mechanism — Guava bundling and the override knob
`closure-compiler-unshaded v20220502` bundles Guava classes inside its own jar at `com.google.common.*` rather than declaring `com.google.guava/guava` as a Maven dep.
Confirmed by `clojure -Stree -A:cljs-current`: no `com.google.guava/guava` node appears in the resolved tree under CLJS 1.11.121 alone.
Therefore `:override-deps` on `com.google.guava/guava` is a no-op — the dep is not in the tree to override.
Working knob is `:extra-deps`, which adds Guava as a top-level coordinate; classpath order then decides which `ImmutableSortedSet` the JVM resolves at class-init of `com.google.javascript.jscomp.deps.ModuleLoader`.
"Unshaded" in this artifact name means "Guava classes ship at their public package, unrelocated", not "Guava ships as an external Maven dep" — these are distinct shading strategies and the artifact name is misleading.
`toImmutableSortedSet(Comparator)` was added in Guava 21.0; injecting Guava 20.0 (or older) wins classpath order against the bundled-newer copy and removes the method, hence the NSME.

## Pinning vector chosen — Cand-3.1
Direct old-Guava `:extra-deps` injection on the consumer classpath, Guava 20.0.
Sufficient on first probe; downgrade ladder unused.
Rejected without trying:
  Cand-3.2 `traversy/traversy 0.5.0` pin — superseded by 3.1 hitting.
  Cand-3.3 dual CLJS coordinates — superseded by 3.1 hitting.
  Task 1-style sweep driver — single-probe hit obviated the ladder.

## Deliverables built — Task 1.5
`hyperfiddle-agent/consumer-repro/deps.edn` — added `:guava-20`, `:guava-19`, `:guava-18` aliases (via `:extra-deps`), composable with `:cljs-current`. The 19/18 aliases stand by for manual re-probes; not exercised.
`hyperfiddle-agent/.repro-guava-abi-logs/guava-20.0-B1.log` — captured stack (gitignored).
`.gitignore` — added `hyperfiddle-agent/.repro-guava-abi-logs/`.
No alias added to `hyperfiddle-agent/deps.edn` — repro lives jar-side; in-monorepo source-side mirror was not requested for this axis.

## Acceptance — Task 1.5
Met. NSME with exact missing method `ImmutableSortedSet.toImmutableSortedSet(Comparator)` at `cljs_analyzer2.clj:1:1` macroexpansion observed under jar-on-classpath consumer setup.
Re-run command: `cd hyperfiddle-agent/consumer-repro && clojure -M:cljs-current:guava-20 -e "(require 'hyperfiddle.navigator-agent-entrypoint) (println :B1-OK)"`.
Negative-control re-run command (clean, for post-fix verification): same command without `:guava-20`.

## Mechanism reading, refined
Existing witness mechanism is confirmed by source.
`lang3.clj:15` requires cljs-analyzer2 alias `cljs-ana`.
`lang3.clj:415` calls `cljs-ana/analyze-nsT` from a macro body.
lang3 macros invoked during agent ns-load expand `e/defn` forms and drive analyzer work on cljs.core, consistent with the doc's prior "analyzer-run-time" reading.

The "AOT alone is sufficient" conclusion does not survive Dustin's stack.
That failure mode is require-time and is unaddressed by AOT alone.
There are two failure modes: analyzer-run-time (witnessed at CLJS 1.10.773) and require-time / Guava ABI (Dustin's stack).

## Eager analysis at `lang3.clj:77`
`(def !a (cljs-ana/->!a))` runs at lang3 class-init and analyzes eagerly (user-confirmed).
Lifting cljs.* out of `:require` lists alone is insufficient — line 77 itself triggers cljs-analyzer2 + cljs.* loading at class-init.
Task 2 must gate `!a`'s construction behind a delay so it does not run at class-init.

## Implication for Task 2, revised
AOT alone is not sufficient — the fix has four required parts.
AOT compile the agent's namespaces so `e/defn` macroexpansion runs at build time on a controlled classpath.
Lift cljs.* requires out of ns forms at four sites: `lang3.clj:3-5`, `cljs_analyzer2.clj:3-8`, `electric3.cljc:4` (currently `:as`), `electric_local_def3.cljc:10` (currently `:as`).
Gate `lang3.clj:77`'s eager `!a` construction behind a delay.
Shape the published POM to exclude or scope-`provided` `org.clojure/clojurescript`, so consumers do not inherit CLJS / closure-compiler-unshaded / Guava transitively from the agent jar.

POM-shaping is what makes consumer-side Guava conflicts no longer the agent's problem.
`requiring-resolve` alone does not alter Maven dep resolution; it controls Clojure namespace loading at runtime, not the Maven dep graph.

Surface audit for the requiring-resolve lift, per source check.
Top-level cljs.* requires live in two namespaces: `lang3.clj` (cljs.analyzer, cljs.analyzer.api, cljs.env) and `cljs_analyzer2.clj` (those plus cljs.core, cljs.repl, cljs.tagged-literals).
`electric3.cljc:4` and `electric_local_def3.cljc:10` currently load lang3 via `:as` — both are entry points into the chain from agent code.
`runtime3.cljc:10` and `missionary_util.cljc:5` already use `:as-alias` and do not contribute to the load chain (Research §"Where lang3 is :as-alias only").

# Resolution

## Outcome
Both failure modes pass under the published agent jar.
Analyzer-runtime mode (CLJS 1.10.520) and Guava-ABI mode (Guava 20.0 + current CLJS) both clear B1 and Bprod.
Negative control (no arcane pin) also clears.

## What was built — file-by-file
`electric/src/hyperfiddle/electric3.cljc` — line 4 `:as lang` → `:as-alias lang`; six `lang/normalize-env` and `lang/->source` call sites rewritten as ``((requiring-resolve `lang/<sym>) ...)`` (syntax-quote performs alias resolution at read time, producing the fully-qualified symbol via the registered `:as-alias`).
`electric/src/hyperfiddle/electric_local_def3.cljc` — same lift; eight call sites including macro-body unquotes (`~(lang/->source ...)` inside backquote).
`electric-secret/src/hyperfiddle/electric/impl/entrypoint.clj` — same lift; two call sites.
`hyperfiddle-agent/deps.edn` — `:exclusions [org.clojure/clojurescript]` on the `com.hyperfiddle/hyperfiddle` dep.
`hyperfiddle-agent/src-build/build.clj` — `tools.build/compile-clj` invocation with `:ns-compile` over 8 agent entry points; post-compile prune of `hyperfiddle/electric/impl/lang3*`, `hyperfiddle/electric/impl/cljs_analyzer2*`, and the `cljs/` subtree from class-dir; dual basis (`build-basis` includes `:build` for shadow-cljs → CLJS at AOT, `publish-basis` for the consumer-facing POM with no aliases activated).
`hyperfiddle-agent/consumer-repro/deps.edn` — relocated from `~/tmp/agent-consumer-repro/` so the deliverable is self-contained inside the monorepo; `:cljs-current` and `:cljs-arcane` aliases switched from `:override-deps` to `:extra-deps` (the new POM exclusion removes CLJS from the base resolution so `:override-deps` would no-op).

## Decisions taken
Q1 — Drop delay-gate at `lang3.clj:77`.
Reason: post-fix, lang3 is unreachable at consumer load (its only loaders are removed via `:as-alias`); the `(def !a (cljs-ana/->!a))` form never runs at consumer.
Defense-in-depth judged not worth the churn.

Q2 — `:ns-compile` over 8 agent entry points (`prod`, `hyperfiddle0`, `hyperfiddle.navigator-agent`, `hyperfiddle.navigator-agent-entrypoint`, `hyperfiddle.agent-server-adapter`, `hyperfiddle.cloud-proxy`, `hyperfiddle.nav-cloud-sessions`, `hyperfiddle.reverse-proxy`); transitive walk covers electric runtime.

Q3 — Strategy A (per-call `((requiring-resolve 'sym) args)`) over Strategy B (top-of-file shim defns); applied uniformly to all three lifted nses.

## Lift sites discovered beyond the spec cut
The Q1–Q3 cut narrowed the spec's four-site list to one (electric3 only) on the reasoning that the others would not load at consumer post-fix.
First post-build acceptance run contradicted that.
Two additional sites surfaced and were lifted.

`hyperfiddle.electric.impl.entrypoint` (in `electric-secret/`) is AOT'd into the published electric jar by `electric/src-build/build.clj:19`; its ns-form had `:as lang` and the AOT'd init class fired `(require 'cljs.analyzer)` at consumer load.
`electric3.cljc:6` requires entrypoint unconditionally on `:clj`.

`hyperfiddle.electric-local-def3` is reached from agent code via `electric_scroll0:9` (`:as l`) → `electric_forms5` → `navigator6` → `navigator-agent-entrypoint`.
local-def3's ns-form had `:as lang`; same AOT'd-init crash on consumer load.

## Build mechanics — load-bearing detail
`tools.build/compile-clj` follows `:require` (real loads) statically, **not** `:as-alias`.
With electric3's lang3 alias changed to `:as-alias`, compile-clj's static walk does not attempt to AOT lang3.
At AOT time, agent macros invoke `requiring-resolve` which dynamically loads lang3 (and cljs.*); compile-clj writes `lang3__init.class` and `cljs/*__init.class` into class-dir as a side effect of `*compile-files* true`.
Post-compile prune removes those classes before `tools.build/jar`.

`:filter-nses` was tried and rejected.
It is a prefix-keep filter; expressing "keep all transitive Clojure deps except lang3 + cljs_analyzer2 + cljs.*" requires enumerating the full surviving prefix list, which is fragile.
Manual prune of three known-bad paths is shorter and explicit.

The dual-basis split is required.
`build-basis` activates `:build` alias to bring shadow-cljs and its transitive CLJS onto the AOT classpath; without it `compile-clj` cannot run lang3's analyzer.
`publish-basis` is built without aliases so the generated POM reflects consumer-facing coordinates (CLJS exclusion intact, no shadow-cljs leakage).

## Acceptance — measured
B1 (`(require 'hyperfiddle.navigator-agent-entrypoint) (println :B1-OK)`) passes under: baseline (no CLJS), `:cljs-arcane`, `:cljs-current:guava-20`.
Bprod (`clojure -M -m prod http-port <port>`) boots Jetty under all three; `curl localhost:<port>` returns HTTP 404 in each case.
Published POM verified by direct inspection to contain `<exclusion>org.clojure/clojurescript</exclusion>` on the `com.hyperfiddle/hyperfiddle` dep.
Built jar verified to contain `electric3__init.class`, `runtime3__init.class`, `prod__init.class`; verified to contain no `lang3*`, no `cljs_analyzer2*`, no `cljs/` entries.

## Re-running verification

When code in `/electric`, `/electric-secret`, or `/hyperfiddle-agent` changes, rebuild the agent jar and re-run the probes below.
The `:local` alias is required so the build picks up local sources for hyperfiddle / electric / electric-secret rather than Maven-resident copies.

### Build + install (one-shot)
All paths below are relative to the monorepo root.
```bash
cd hyperfiddle-agent
clj -X:build:local build   :aliases '[:local]'   # produces target/hyperfiddle-agent-v0-alpha-SNAPSHOT.jar
clj -X:build:local install :aliases '[:local]'   # installs to ~/.m2/repository
```

### B1 probes — `(require 'hyperfiddle.navigator-agent-entrypoint)` succeeds
Run from the consumer fixture directory.
```bash
cd hyperfiddle-agent/consumer-repro

# Baseline (no CLJS on classpath)
clojure -M -e "(require 'hyperfiddle.navigator-agent-entrypoint) (println :B1-baseline-OK)"

# Analyzer-runtime failure mode (arcane CLJS 1.10.520)
clojure -M:cljs-arcane -e "(require 'hyperfiddle.navigator-agent-entrypoint) (println :B1-arcane-OK)"

# Guava-ABI failure mode (Guava 20.0 + current CLJS)
clojure -M:cljs-current:guava-20 -e "(require 'hyperfiddle.navigator-agent-entrypoint) (println :B1-guava-OK)"
```
Each must print its `:B1-…-OK` marker. Pre-fix, the arcane probe throws `NullPointerException at cljs.analyzer/excluded?` and the guava probe throws `NoSuchMethodError ImmutableSortedSet.toImmutableSortedSet(Comparator)` at `cljs_analyzer2.clj:1:1`.

### Bprod probes — `clojure -M -m prod` boots Jetty
```bash
cd hyperfiddle-agent/consumer-repro

# Baseline
timeout 20 clojure -M                       -m prod http-port 18083 &
sleep 10 && curl -s -o /dev/null -w "HTTP %{http_code}\n" localhost:18083  # expect: HTTP 404

# Arcane CLJS
timeout 20 clojure -M:cljs-arcane           -m prod http-port 18081 &
sleep 10 && curl -s -o /dev/null -w "HTTP %{http_code}\n" localhost:18081  # expect: HTTP 404

# Guava 20 + current CLJS
timeout 20 clojure -M:cljs-current:guava-20 -m prod http-port 18082 &
sleep 10 && curl -s -o /dev/null -w "HTTP %{http_code}\n" localhost:18082  # expect: HTTP 404
```
HTTP 404 confirms Jetty is up. The route table simply has no `/` handler.

### Jar-shape sanity check
```bash
cd /home/peter/git/monorepo/hyperfiddle-agent
unzip -l target/hyperfiddle-agent-v0-alpha-SNAPSHOT.jar > /tmp/agent-jar.log

# Must NOT appear (would re-introduce CLJS load chain at consumer):
grep -E '(lang3|cljs_analyzer2|^\s*[0-9]+.*\scljs/)' /tmp/agent-jar.log   # expect: no output

# Must appear (proves AOT covered the load chain):
grep -E '(electric3__init|runtime3__init|prod__init)' /tmp/agent-jar.log
```

### POM-shape sanity check
```bash
unzip -p target/hyperfiddle-agent-v0-alpha-SNAPSHOT.jar \
  META-INF/maven/com.hyperfiddle/hyperfiddle-agent/pom.xml \
  | grep -A2 'exclusions'
# Expect: <exclusion><groupId>org.clojure</groupId><artifactId>clojurescript</artifactId></exclusion>
```

### Consumer-fixture reference
`hyperfiddle-agent/consumer-repro/deps.edn` declares `com.hyperfiddle/hyperfiddle-agent {:mvn/version "v0-alpha-SNAPSHOT"}` and these aliases:
- `:cljs-current` — `:extra-deps {org.clojure/clojurescript {:mvn/version "1.11.121"}}`
- `:cljs-arcane`  — `:extra-deps {org.clojure/clojurescript {:mvn/version "1.10.520"}}`
- `:guava-20` / `:guava-19` / `:guava-18` — `:extra-deps {com.google.guava/guava {:mvn/version "<n>.0"}}`

`:extra-deps` (not `:override-deps`) is required because the agent's POM exclusion removes CLJS from the base resolution; there is nothing for `:override-deps` to override.

### Pre-fix witness, for future re-validation
The pre-fix witness was captured against `v0-alpha-SNAPSHOT` of the agent before this Resolution section landed.
To re-witness either failure mode (e.g. when validating an alternative fix), revert the changes in `/electric` + `/electric-secret` + `/hyperfiddle-agent`, rebuild + reinstall, then run the corresponding probe — the analyzer-runtime probe with `:cljs-arcane` and the Guava-ABI probe with `:cljs-current:guava-20`.

## Items not done
`mvn dependency:tree` from a clean consumer not run; POM exclusion verified by direct inspection only.
Pre-fix re-baseline before the edits not run; relied on prior witness logs (§Witness, §Witness — Guava ABI mode).
Other electric source-mode consumers (starter-app, electric-fiddle, hyperfiddle) not smoke-tested under the lift; recommended as a follow-up before merging electric changes.

## Defensive lifts not applied
`electric-secret/src/hyperfiddle/electric/shadow_cljs/hooks3.clj:6` retains `:as lang` and is AOT'd by electric's build.
Not in the agent's runtime require chain (referenced only as a symbol in `electric3.cljc:382` and via shadow-cljs.edn config), so not load-bearing for this fix.
Lift if hooks3 ever ends up in a consumer load chain.

## Reach beyond the agent jar
The lifts in `electric3.cljc`, `electric_local_def3.cljc`, and `entrypoint.clj` live in `/electric` and `/electric-secret`.
Until those packages republish to Maven, the fix relies on the agent's `clj -X:build:local` build path picking up local source via `:local/root` overrides; the resulting agent jar carries fixed `entrypoint__init.class` etc. and Clojure's loader prefers them over the older Maven-resident copies.
A clean Maven-only build (without `:local`) would AOT the fixed-source's transitive chain only after `electric` and `electric-secret` are republished.

Cross-consumer reach is intentionally out of scope for this Task 2.
Other Electric backend deployments (e.g. apps using `(e/boot-server ...)` directly without the agent) remain vulnerable to both failure modes until `com.hyperfiddle/electric` itself is republished with the lifts; their classpath has electric.jar's stale `entrypoint__init.class` and no agent.jar to shadow it.
Decision: ship Task 2 for the agent now; Electric / electric-secret republish is a separate workstream.

## `:class`-vs-`:clj` precedence — `:reload` caveat
The fix relies on Clojure's loader preferring `.class` over `.clj` when both are present.
Agent jar ships AOT'd `electric3__init.class`, `entrypoint__init.class`, etc.; the Maven `electric.jar` still ships the un-republished `.cljc` sources.
At consumer JVM cold-start, `(require 'hyperfiddle.electric3)` finds the agent's `.class` first and loads it — no lang3, no cljs.* loaded.
However, `(require 'hyperfiddle.electric3 :reload)` and equivalents (e.g. `tools.namespace/refresh`) explicitly bypass the `.class` path and recompile from `.cljc`.
On a consumer that performs `:reload` against `hyperfiddle.electric3` while having arcane CLJS or Guava 20 on classpath, the failure mode returns.
Edge case for dev/REPL workflows; not relevant for backend production where the agent is the deliverable.

## Known issues — third-party AOT bloat
`tools.build/compile-clj` AOT-compiles every transitively-loaded namespace (`*compile-files* true` recompiles `.clj` deps that don't already have a `__init.class` on classpath).
Result: the agent jar contains AOT'd classes for ~3,000 third-party namespaces in addition to the ~7,400 first-party ones it should contain.

Audit (run `unzip -l target/<jar> | awk '/\.class$/ {print $4}' | sed -E 's@^([^/]+)/.*@\1@' | sort | uniq -c | sort -rn`):

| Top-level prefix | Class count | Provenance |
|---|---|---|
| `hyperfiddle/*` | 7,399 | first-party |
| `clojure/*` | 1,220 | third-party — `clojure/tools/analyzer` (578), `clojure/tools/reader` (242), `clojure/core` (143), `clojure/data` (56), `clojure/tools/logging` (29) |
| `dustingetz/*` | 479 | first-party (monorepo-internal) |
| `instaparse/*` | 414 | third-party |
| `ring/*` | 374 | third-party |
| `edamame/*` | 140 | third-party |
| `applied_science/*` | 110 | third-party |
| `cloroutine/*` | 105 | third-party |
| `missionary/*` | 99 | third-party |
| `cognitect/*` | 70 | third-party |

Mechanism: `:ns-compile` lists agent entry points; compile-clj walks `:require` graph; every reachable third-party namespace gets re-compiled because none ship `__init.class` from Maven. Only `:filter-nses` (currently unused) limits what survives in `class-dir` — its absence is the gap.

Consequences:
- **Bloat** — pre-fix thin jar 559 K with 8 source files; post-fix ~10 MB with 10,439 classes.
- **Classpath shadowing** — consumer's pinned `org.clojure/tools.analyzer` / `tools.reader` / `instaparse` etc. is shadowed by the agent's embedded AOT'd version. Silent compatibility risk if a consumer expects a different version of any of those libs.
- **POM-vs-reality drift** — the POM advertises CLJS-excluded backend-only contract, but the jar embeds 3 K third-party classes the POM does not declare.
- **AOT-fragility risk** — `clojure/tools/logging` is famously AOT-fragile (electric-secret already ships `clojure_tools_logging_aot_fix.clj` per `electric/src-build/build.clj:16`); ring middleware and missionary macros have similar known issues. Each AOT'd third-party lib is a potential silent runtime regression.

Why it didn't surface in acceptance: probes test failure-mode resilience (B1 / Bprod under arcane CLJS / Guava 20). They do not test classpath-shadowing of consumer-pinned versions, jar size, or AOT-fragile lib behavior.

Mitigation candidates considered: (a) `:filter-nses` keep-list on `compile-clj`; (b) post-prune of known third-party prefixes in addition to lang3/cljs_analyzer2/cljs.

## Partial resolution — `:filter-nses` keep-list landed (Cand-A)
`hyperfiddle-agent/src-build/build.clj` now passes `:filter-nses aot-keep-prefixes` to `tools.build/compile-clj`.
Keep-list: `[hyperfiddle dustingetz geoffreygaillard prod hyperfiddle0 clojure.tools.logging]`.
Class count: 10,439 → 7,974 (≈2,465 third-party classes stripped).

### Why each prefix is in the list
First-party shadow set: `hyperfiddle`, `dustingetz`, `geoffreygaillard`, `prod`, `hyperfiddle0`.
`geoffreygaillard` was discovered by sub-audit of `hyperfiddle-contrib/src/geoffreygaillard/` — referenced by `hyperfiddle.navigator6.*` in the agent load chain; first-party even though not under `hyperfiddle/*`.
AOT-fragile third-party: `clojure.tools.logging`.

### `clojure.tools.logging` discovery
Removing tools.logging from the keep-list breaks Bprod with `ClassNotFoundException: clojure.tools.logging.impl.LoggerFactory`.
Mechanism: `tools.logging-1.2.4.jar` ships source-only; `LoggerFactory` is `gen-interface`-generated when `clojure.tools.logging.impl` loads; AOT'd `prod__init.class` embeds direct bytecode references to that class; JVM resolves the symbol at link time, before the requiring `__init.class` body has run, so the class doesn't exist yet.
Fix applied: include `clojure.tools.logging` in keep-list — agent jar ships ~67 AOT'd tools.logging classes.

### Cost of the tools.logging inclusion
Pins tools.logging 1.2.4 in any consumer that doesn't override.
Conflicts with the principle stated in `electric-secret/.../clojure_tools_logging_aot_fix.clj:9`: "We must not ship a library with a pinner logger version."

### Open path (not taken in this iteration)
The cleaner fix is the existing indirection: route first-party `[clojure.tools.logging :as log]` through `hyperfiddle.electric.shadow-cljs.clojure-tools-logging-aot-fix` (source-only, never AOT'd, defers `LoggerFactory` resolution to runtime).
Scope: extend wrapper API beyond `trace`/`debug` to cover `info`/`warn`/`error`/`spy`/`spyf`; lift `[clojure.tools.logging :as log]` → wrapper alias across ~12 first-party files (5 in `hyperfiddle-agent/`, 7 in `/hyperfiddle/`); drop `clojure.tools.logging` from keep-list.
Reaches into `/hyperfiddle` — same shape as the doc's "Reach beyond the agent jar" deferral.
User is consulting the team on whether to remove the tools.logging dependency entirely, which would obviate both the keep-list entry and the wrapper-refactor option.

### Acceptance status under the new keep-list
B1 probes (baseline / `:cljs-arcane` / `:cljs-current:guava-20`) all pass.
Bprod probes not re-run after `clojure.tools.logging` was added to the keep-list — pending team decision on the dependency.

### Other AOT-fragile candidates not investigated
The doc previously flagged ring middleware and missionary macros as candidates with similar AOT-fragility risk.
Neither was exercised by the current B1/Bprod probes after their classes were stripped, but absence-of-failure ≠ proof-of-safety.

## Explicit deferrals
The following review items were considered and consciously dropped for this Task 2:

**No regression guard against future `:as lang` introductions** (review finding #4).
A future PR adding `[hyperfiddle.electric.impl.lang3 :as l]` to any electric ns reachable from the agent silently re-introduces the bug.
Decision: wont-fix. No CI assertion or clj-kondo hook added.
Mitigation candidates if revisited: post-build assertion that `target/classes/hyperfiddle/electric/impl/lang3*` is empty before jaring; or a clj-kondo lint flagging non-`:as-alias` requires of lang3 from electric runtime nses.

**`:override-deps` aliases bypass the `:exclusions` in main deps** (review finding #7).
`:latest`, `:dev`, `:local` aliases all use `:override-deps` for `com.hyperfiddle/hyperfiddle`, which replaces the coordinate wholesale — including dropping our `:exclusions [org.clojure/clojurescript]`.
A build via `clj -X:build:latest build` produces a POM whose hyperfiddle dep has no exclusion; the consumer would inherit CLJS transitively.
Decision: wont-fix. The canonical `clj -X:build build` path is what gets published; alternative-alias builds are dev-only and not the deliverable.
Mitigation if revisited: replicate the `:exclusions` clause inside each override-deps coordinate, or post-process the generated POM unconditionally.
