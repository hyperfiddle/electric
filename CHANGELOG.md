# Dev Changelog — Electric Clojure

[![Clojars Project](https://img.shields.io/clojars/v/com.hyperfiddle/electric.svg)](https://clojars.org/com.hyperfiddle/electric)
 
* alpha (new feature work and enhancements in development)
* minor breaking changes allowed at this time
* major versions ("v2-alpha") are marketing numbers and will increment with major milestones

# next up

* low-level runtime & missionary design improvements to unblock the next Electric major version
* Electric UI control improvements (high level UI controls with optimistic updates that account for sync/latency/failure state)
* documentation and demos

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
