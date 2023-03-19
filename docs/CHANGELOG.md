# Dev Changelog — Electric Clojure

[![Clojars Project](https://img.shields.io/clojars/v/com.hyperfiddle/electric.svg)](https://clojars.org/com.hyperfiddle/electric)
 
* alpha (new feature work and enhancements in development)
* minor breaking changes allowed at this time, pay attention to the slack
* major versions ("v2-alpha") are marketing numbers and will increment with major milestones.

# v2-next

* if you cloned the starter app before March 2, your hot code reloads are broken. Fix: in your user.cljs, change `(ns user ^:dev/always` to `(ns ^:dev/always user` as in [this starter-app commit](https://github.com/hyperfiddle/electric-starter-app/commit/a8810accfdd96f82eefc2d976645538223a88de9#diff-06a7049242ecf7dac4e22b30db9faa22ebae4e22e72d1bfbb916e03e3075e5c1). We had it backwards on launch day for about two weeks. Sorry!

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