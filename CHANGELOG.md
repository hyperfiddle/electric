# Dev Changelog — Electric Clojure

[![Clojars Project](https://img.shields.io/clojars/v/com.hyperfiddle/electric.svg)](https://clojars.org/com.hyperfiddle/electric)
 
* alpha (new feature work and enhancements in development)
* minor breaking changes allowed at this time, please join #hyperfiddle @ Clojurians slack
* major versions ("v2-alpha") are marketing numbers and will increment with major milestones

# Roadmap

* wip: Electric v3 (major Electric upgrades to e/for-by and e/fn in complex client/server topologies)

# v2-alpha-540-ga4699532 — 2024 Jan 10

- Simpler websocket integration
  - Electric Jetty and HTTPKit adapters are now based on latest Ring (1.11+), supporting websockets out of the box. It is now simpler to integrate electric into existing ring apps, and extend     the electric adapter over new HTTP servers.
  - **breaking**: `hyperfiddle.electric-jetty-adapter` has been replaced by the simpler, generic `hyperfiddle.electric-ring-adapter`.
    The API is simpler, smaller, but is not retrocompatible.
    Code depending on `electric-jetty-adapter` must be updated.
  - **breaking**: `hyperfiddle.electric-httpkit-adapter` is now based on `electric-ring-adapter`
    and exposes the same API. The two namespaces are equivalent, they only differ on
    the underlying HTTP server.
  - To migrate, look at the integration for:
    - [Jetty](https://github.com/hyperfiddle/electric-fiddle/blob/cf6134adb6aa0a4ae2f678961e54036f867cd4d4/src/electric_fiddle/server_jetty.clj#L15-L33)
    - [HTTPKit](https://github.com/hyperfiddle/electric-fiddle/blob/cf6134adb6aa0a4ae2f678961e54036f867cd4d4/src/electric_fiddle/server_httpkit.clj#L14-L39)
    - update your ring middlewares accordingly
  - Note latest ring-jetty depends on Jetty 11, which requires Java 11+. If you need Java 8 compat, see
      [electric-server-java-8-jetty-9](https://github.com/hyperfiddle/electric/blob/cc55772f18bc46373f131e092fc20055c8062b59/src-docs/electric_server_java8_jetty9.clj)
      and [electric-jetty-adapter](https://github.com/hyperfiddle/electric/blob/cc55772f18bc46373f131e092fc20055c8062b59/src/hyperfiddle/electric_jetty_adapter.clj)

# v2-alpha-536-g0c582f78 — 2024 Jan 10
- **Incremental compilation (IC):**
  - Electric compiled the whole program at once, meaining changing a single file in a large project caused long recompilaiton times. With this release electric compiles per definition (`e/def` and `e/defn`), reducing recompilation. Note: since clojurescript compiles on file granularity electric also recompiles the whole file(s). This is a clojurescript limitation.
  - **breaking**: shadow-cljs build hook required for dev setup. [See example shadow-cljs.edn.](https://github.com/hyperfiddle/electric-fiddle/blob/795c5edc29d57c9b037199cfe66996947d894d8d/shadow-cljs.edn#L12)
  - increased security. Before IC client sent server code as Intermediate Representation (IR) over websocket and server had to eval it. With IC eval is gone and server compiles its own code.
  - **breaking**: a production build requires cljs on the classpath or AOT compilation. We removed runtime server eval of electric IR (the client doesn’t send the program over the websocket), increasing security. The server now compiles electric code and it needs cljs to macroexpand client code.
  - **breaking**: client and server programs are now compiled separately and their compiled output shapes must match. Some macros expand conditionally and generate a different program on client vs server. Example: `clojure.core/*assert*`, `clojure.spec.alpha/*compile-asserts*` and cljs compile option `:elide-asserts`. Check for conditional macros or compiler options if your program works in dev mode but crashes under advanced compilation or with a different setup.
  - **breaking**: more coloring required. `(e/defn X [] (js/alert 1))` fails to compile on server, one needs to add an `e/client` for the server to understand `js/...` is client only. E.g. `(e/defn X [] (e/client (js/alert 1)))`.
  - **breaking**: stricter definition order. `(e/def Y (inc X)) (e/def X 1)` used to work, now one has to reorder → `(e/def X 1) (e/def Y (inc X))`. Before IC, electric deferred all compilation and could toposort the definitions. With IC definitions have to come in correct dependency order. Note: this is now the same as in clojure.
  - **breaking**: server compiler requires presence of the `#js` reader tag. Only the client (cljs compiler) used to compile electric code. Now the server compiles its own code, and therefore must be able to parse (read) code in `(e/client ...)` blocks. One must now either: install a reader extension for `#js`, or use reader conditionals to guard usage of `#js`. The simplest is to add a `data_readers.clj` to one's classpath with content `{js cljs.tagged-literals/read-js}`. We don’t provide one OOTB because it’s a global tag and users might have their own installed already, causing unresolvable conflicts.
  - **breaking**: cljs file cannot contain electric definitions anymore. With IC the server compiles its own portion of the definition, i.e. it needs to see the code, so it must be cljc.
  - **breaking**: 2 entrypoints, `e/boot-client` and `e/boot-server`, that need to match. Take `opts Main & args` instead of arbitrary electric code to minimize user errors.
    - Examples of [server entrypoint](https://github.com/hyperfiddle/electric-fiddle/blob/cf6134adb6aa0a4ae2f678961e54036f867cd4d4/src-dev/dev.cljc#L42), [client entrypoint](https://github.com/hyperfiddle/electric-fiddle/blob/cf6134adb6aa0a4ae2f678961e54036f867cd4d4/src-dev/dev.cljc#L54-L57).
  - **breaking**: http adapters' `electric-ws-message-handler` take an additional argument `entrypoint` which is a function of 1 argument expecting the ring request. See starter app changes for how to update entrypoints.
  - **breaking**: `e/*http-request*` removed, ring request passed as positional argument to entrypoint. Provided electric dynamic `e/http-request` which [examples](https://github.com/hyperfiddle/electric-fiddle/blob/795c5edc29d57c9b037199cfe66996947d894d8d/src-fiddles/hello_fiddle/fiddles.cljc#L15-L17) binds in entrypoint. Users can pass additional clojure arguments through the electric entrypoint.
  - **breaking**: program starts on the server due to above.
  - **breaking**: reactive stacktrace now prints on server, because the program starts on the server. Client warns about server print in console.
  - reader conditionals work if there's no transfer inside. E.g. `#?(:cljs #js {:x 1})` will compile, `#?(:cljs #js {:x server-value})` will compile but fail at runtime, `(let [x server-value] #?(:cljs #js {:x x}))` will work.
- **Electric:**
  - **breaking**: rename `HYPERFIDDLE_ELECTRIC_CLIENT_VERSION` to `ELECTRIC_USER_VERSION`
  - improved error message when an electric function is called as a clojure function (without `new`).
  - improved error message when `dom/node` is `nil`
  - fix: `e/dom-mousemove` now works
  - fix: `e/apply` branching code is pre-compiled, reducing callsite expansion and compilation time
  - fix: varargs code expansion is much smaller, reducing compilation time

# v2-alpha-469-gb6d9865c — 2023 Nov 22

- **Electric:**
  - fix: too much recursion error
  - fix: support `e/fn` map varargs. `(e/fn X [& {:keys x}])`
  - fix: remove more false positive cljs inference warnings
  - fix: websocket disconnects. Chromium-based browsers throttle timers, causing heartbeat misses. We now send heartbeats from the server
  - reduce generated code size
- **hyperfiddle.history:**
  - fix: intercept link clicks when `<a>` is not the direct target (e.g. `<a><button></button></a>'`)
  - fix: bind `!history` on link click
  - add `Navigate!` to inject navigation logic
- **hyperfiddle.electric_dom2:**
  - add `col` and `colgroup`
  - support setting CSS vars
  - fix: add missing parameter in `styles` (thanks @eneroth)

# v2-alpha-428-g22937f75 — 2023 August 24

- **Electric Clojure/Script compatibility improvements:**
  - add e/fn multi-arity support
  - add e/fn varargs support
  - add e/apply to call e/fn with list of arguments (i.e. now possible to call e/fn where the number of arguments is only known at runtime)
  - allow e/fn to call itself recursively by name
  - allow #js tagged literals in client code
  - implement/fix Java interop forms (e.g. `(e/server (.getName h))`) in Electric
  - core.match support
  - clojure.logging support
  - fix: allow js/alert, js/prompt etc. in electric client code
  - fix: allow using shorthand syntax on imported js enums, e.g. `EventType.CLICK`
  - fix: `set!` should support an Electric expression in the value position
- **Electric compiler improvements:**
  - fix: false positive ClojureScript warnings using goog modules
    - "undeclared Var goog.color/hslToHex"
    - "No such namespace: goog.color"
    - "cljs var: hyperfiddle.electric-dom2-test/hyperfiddle.electric_dom2_test.goog$module$goog$object.get is undeclared, passing through."
  - fix: false positive ClojureScript warnings due to missing type hints (e.g. `^js`). Hints are now properly propagated from Electric code to ClojureScript. We expect more type hint bugs, please file an issue for any such warnings you see after this release.
  - fix: JVM class type hints (e.g. `^File`) were wrongly sent to ClojureScript, breaking core.match usage in e/server context
  - fix: clojure.lang.Namespace objects (like `*ns*`) in e/server failed to compile (fixes clojure.tools.logging Electric compatibility)
  - fix: build failures on goog `:refer`s like `(:require [goog.string :refer (blabla)])`. This temporary fix disables warnings on all `:refer`s, so e.g. will *not* produce a compile-time warning. We anticipate that these hacks can be removed when incremental compilation lands.
- **Error message improvements:**
  - improve error messages (new on nil, e/watch on non-watchable value)
  - fix: rethrow `throw` in `catch` reports the wrong async stack trace
  - fix: send uncaught server-side error to default logger
- **Example app improvements:**
  - electric-starter-app: improve logger config and document the working logger configuration in readme
  - electric-xtdb-starter: move blocking queries to threadpool (see [commit](https://github.com/hyperfiddle/electric-xtdb-starter/commit/99d9aac82aba3c7d14a81ee928ef031ea776a22b)) so as not to block the server, this is necessary for correctness. This is now possible because the e/offload cycle bug was fixed in v2-349.
- feature: add [Fulcro](https://github.com/hyperfiddle/fulcro-electric-template) and [HTTPKit](https://github.com/hyperfiddle/fulcro-electric-template/blob/master/src/main/app/server_components/http_server.clj) adapters
- hyperfiddle.history router improvements
- add clj-kondo library-level configuration
- fix: electric.jar accidentally bundles public/index.html and other demo resources which conflict with userland classpath
- fix: blank-page-after-sleep (clients will now reconnect after server heartbeat timeout)
- fix: dom2/parse-class regressions
- fix: reagent interop broken in e/for
- fix: ui4/date throws NumberFormatException on blank input
- fix: electric binding not seen from cc/fn (Clojure fn closing over an initially nil e/def causes Clojure fn to not react to changes to rebinding the Electric binding)
- fix: remove org.corfield.build, rewrite uberjar task, fix pom.xml
- deps: Clojure 1.12-alpha, missionary b.31, shadow-cljs, tools.build

# v2-alpha-349-ge9996713 — 2023 June 19

- fix: `e/offload` no longer introduces unintended re-runs of electric code
- fix: regression on using keywords/symbols as DOM classes
- fix: DOM class parsing
- fix: regression on dom2/props improperly setting some DOM attributes 

# v2-alpha-338-geec2f3df — 2023 June 9

- introduce new internal DOM event handling strategies, used internally by `electric-dom2` and `electric-ui4`. This fixes long-standing bugs:
  - **breaking**: remove `dom2/Event`, `dom2/event*` (likely no userland impact)
  - remove `dom2/on` (1-arity only, other arities remain). Likely no userland impact.
  - fix: `dom2/on`’s e/fn callback can unintentionally see multiple events under conditions of rapid successive dom events. Note: due to the quirks of this API, userland `dom2/on` usages might possibly rely on the above broken behavior, which means you may need to debug some call sites.
  - fix: console error "two events in the same frame"
  - fix: ui4/input focus glitch under latency
- fix: Two Clocks demo drains battery life even if tab is hidden.
  - Server-streamed clocks now pause when the tab is blurred, thus not holding network stream open and draining battery life on mobile when the tab is running in the background. Note, pausing network in userland based on visibility is a workaround for the underlying issue which is that websockets don't have native backpressure in all browsers yet.
- introduce `e/dom-visibility-state`, used by [e/system-time-ms to pause server clock streaming on tab blur](https://github.com/hyperfiddle/electric/blob/fef8b661966a26e09f3c0683b0a18c759182747f/src/hyperfiddle/electric.cljc#L192-L195).
- introduce `e/dom-mousemove` singleton for current mouse coordinates. Note: for efficiency and glitch-prevention, singletons should be used for all globally shared signals like this.
- error handling: detect missing HYPERFIDDLE_ELECTRIC_SERVER_VERSION environment variable in production build
- fix: docker files HYPERFIDDLE_ELECTRIC_SERVER_VERSION misconfiguration
- fix: websocket no longer fails to reconnect in presence of URL fragment
- fix: dom :class prop prematurely removed in edge case where that prop had been shadowed from subsequent (dom/props) call
- fix: input type range displaying knob in wrong position on max > 100
- fix: "cannot infer target" shadow warning on js field/method interop
- fix: TodoMVC commit edit changes on blur
- fix: "`inst_[...].lause` is not a function" error on hot code reload
- fix: missionary-contrib no longer requires core.async on classpath; this ns is used in the datomic client missionary adapters.
- bump shadow-cljs to 2.22.10

# v2-alpha-263-g89da9d11 — 2023 April 8

**Warning!** If you cloned the starter app before March 2, your hot code reloads are unreliable. Fix: in your user.cljs, change `(ns user ^:dev/always` to `(ns ^:dev/always user` as in [this starter-app commit](https://github.com/hyperfiddle/electric-starter-app/commit/a8810accfdd96f82eefc2d976645538223a88de9#diff-06a7049242ecf7dac4e22b30db9faa22ebae4e22e72d1bfbb916e03e3075e5c1). We had it backwards on launch day for about two weeks. Sorry!

TLDR: Deployment, hot code reloading fixes, cljs advanced mode fixed, bugfixes. **There are breaking changes, upgrade steps required!**

- deployment guide, uberjar, dockerfile, github actions CD scripts - see [electric-starter-app](https://github.com/hyperfiddle/electric-starter-app)
- Electric support for continuous deployment
  - Fingerprinted client build output for cache busting on deploy
  - Client/server version mismatch forces client to refresh the page: [screenshot](docs/electric-cd-version-mismatch-refresh.png)
- ClojureScript advanced mode compilation enabled for demos
- Add support for SVG :xlink attribute alias
- transit handlers are now configurable: [demo_custom_types.cljc](https://github.com/hyperfiddle/electric-examples-app/blob/main/src/wip/demo_custom_types.cljc), **todo move out of wip**
- **breaking**: `hyperfiddle.electric-jetty-server` has moved out of electric and into electric-starter-app.
  - **Action required: you must copy one of the example servers into your app, because it is part of the starter app now!** 
    - [`electric_server_java8_jetty9.clj`](https://github.com/hyperfiddle/electric-starter-app/blob/main/src/electric_server_java8_jetty9.clj)
    - [`electric_server_java11_jetty10.clj`](https://github.com/hyperfiddle/electric-starter-app/blob/main/src/electric_server_java11_jetty10.clj)
  - Q: Why is it not included in the library? A: because it has hardcoded http routes and auth examples
- **breaking: must add "resources" to classpath and update electric-server-config**, the example jetty server loads resources from classpath not filesystem now. see [starter app change](https://github.com/hyperfiddle/electric-starter-app/commit/3ab72a62d3be66b2567e7ad3c1a88a21db409fc2)
- **breaking: ring-basic-auth is no longer included in electric deps. Add this to your project** if you are using the example jetty server: `ring-basic-authentication/ring-basic-authentication {:mvn/version "1.1.1"}` (or copy/paste the example server into your project and remove basic auth)
- **breaking**: move `hf/http-request` to `e/http-request`
- **breaking**: remove unused electric <> core.async adapters to eliminate hard dep on core.async. the unused fns are [moved to contrib.missionary-contrib and commented out](https://github.com/hyperfiddle/electric/commit/ca10f5beb5df60a7be3d6f45f1c546e8864b04dd#diff-30ca46c3b78b7d5dd9db533c8639c307f68e5c1d5b9788ae24d9f34b188b56d0) until we revisit.
- **breaking**: remove hyperfiddle.logger; migrate to clojure.tools.logging (which uses Log4J internally) and js/console.log; drop `com.taoensso/timbre` dep. note: the Jetty server example still uses log4j internally as well. So you at least need a logback.xml to silence jetty log spam.
- fix: custom `hyperfiddle.electric-client/*ws-server-url*` binding is lost on reconnect
- fix: compilation with advanced optimization
- fix: m/sleep deadlock in missionary (impacts Two Clocks demo)
- fix: regressions related to "reactive exceptions no longer spam the console"
- fix: new hot-code reloading strategy breaks XTDB starter app
- fix: ns auto reloading should not reload external libs (fixes protocol redefinition issues exhibited in XTDB-starter-app)
- fix: ui/input glitch after blur under latency
- fix: `*ns*` bound incorrectly during macroexpansion
- fix: SVG elements don't get class prop
- fix: async traces lose original exception on transfer
- fix: reactive exceptions inside a `dom/on` callback can be silently swallowed
- fix: shadow-css live css reload causes NPE in electric-dom
- fix: xtdb starter app prints “Datomic libs detected: #{}” at console
- fix: hyperfiddle.history incorrectly stacking paths on pushState and replaceState
- fix: electric-codemirror mount/unmount handling
- electric-codemirror upgraded to latest codemirror version. adds readonly mode and custom theme support, relax default css
- HFQL tree grid css now uses `em` units to be embeddable
- electric-starter-app: remove package.json, no more npm dependencies
- example jetty server: better error messages on missing index.html or manifest.edn

# v2-alpha-123-ga7fa624f — 2023 March 2

* hot code reloading stability improvements. **note: you MUST run Shadow and your CLJ REPL from the same JVM! Do NOT use shadow from node_modules** as this results in two JVMs because your Electric client/server code versions will desync.
* drop need for `#?(:cljs (:require-macros ...))` in electric src namespaces
* `e/wrap` is now `e/offload`, note the signature changed it takes a clojure thunk now, [example](https://github.com/hyperfiddle/electric/blob/3fb66e9da5ee96a3efc81c56d2af8dd3b090486e/src-docs/user/demo_3_system_properties.cljc#L24)
* zero config entrypoint – please compare to the [starter app entrypoint](https://github.com/hyperfiddle/electric-starter-app/blob/a8810accfdd96f82eefc2d976645538223a88de9/src/user.cljs#L7-L10) to see if any boilerplate can be removed
* added `e/on-unmount`, [example usage in presence demo](https://github.com/hyperfiddle/electric/blob/3fb66e9da5ee96a3efc81c56d2af8dd3b090486e/src-docs/user/demo_4_chat_extended.cljc#L55). Notes: (a) this interface is going to change; (b) e/mount has been removed, you [don't need it](https://github.com/hyperfiddle/electric/commit/dfcfe505e8142f06b5001d0eda00f7b406d1bb95)
* introduced dom/on! optimized callback for fast events like [mouse-move](https://github.com/hyperfiddle/electric/blob/3fb66e9da5ee96a3efc81c56d2af8dd3b090486e/src-docs/user/demo_reagent_interop.cljc#L70) (note: usage will change soon, this atom is pure overhead)
* legacy `photon-ui` and `photon-dom` are removed, you must upgrade
* electric-goog-history example [published](https://github.com/hyperfiddle/electric/blob/3fb66e9da5ee96a3efc81c56d2af8dd3b090486e/src/contrib/electric_goog_history.cljc), this is going to move and have breaking changes but we do commit to maintaining it somewhere
* SVG support
* async stack trace improvements
* clj-kondo config is exported
* fix: clojure deep def is now supported (for debugging)
* fix: shadow-css is now supported
* fix: reactive exceptions no longer spam the console
* fix: `Pending` bugs

# v2-alpha-0-g40c3384e — 2023 Feb 12

Initial release.

- Production ready for, let's say back office apps, after 8 months of private user testing and extreme dogfooding in the Hyperfiddle sister project.
- As a maturity indicator, the only low level bug in recent memory was a hash collision triggered by scrolling a server-paginated grid over thousands of server-streamed elements.
- Stack traces aren't great; we do have async stack traces already but they need work

Current development priorities:
* developer experience improvements
* network planner improvements
* language semantics improvements

To date we have focused on correct semantics over syntax and performance. Now that we are useful in production, we are using production learnings to drive our priorities.
