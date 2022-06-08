# Hyperfiddle Photon – a streaming Clojure/Script dialect with compiler-managed client/server datasync

Photon lets you express a frontend/backend web application as a single unified Clojure/Script expression that transcends the client/server boundary.

```clojure
(p/defn View [db state]
  (p/client
    (dom/div
      (let [email (dom/input)]
        (dom/h1 "Your orders")
        (dom/table
          (p/for [x (p/server xs (query-database db email))]
            (dom/tr (pr-str x))))))))

; Note: This is our target future syntax, we're not quite there yet.
```

It's called Photon because every point in a Photon form can be thought of as simultaneously a reactive flow and a value.

> Albert Einstein, on the wave-particle duality: *"It seems as though we must use sometimes the one theory and sometimes the other, while at times we may use either. We are faced with a new kind of difficulty. We have two contradictory pictures of reality; separately neither of them fully explains the phenomena of light, but together they do."*

# Setup
```bash
git clone ...
cd photon
clojure -T:build compile-java

# Sanity check that it's working:
clj -A:dev -X user/main     # healthcheck app http://localhost:8080
```

# IDE setup
Cursive:
* new deps.edn project from existing sources
* mark directory as source root (they will turn blue):
  * src-docs
  * src-dev
* Add Configuration
  * new Clojure REPL - Local
  * run with Deps
  * alias: `dev`
* Run configuration

Emacs:

* .dir-locals.el has some project-specific config.
  * When you first open a Clojure file, you'll be prompted to allow that config to be applied. 
  * Answer `y`. 
* Then, to jack in:
  * `M-x sesman-start`
  * Choose `cider-jack-in-clj`
  * Your Clojure REPL is connected
* Starting a CLJS REPL:
  * Start a CLJS build from the Clojure REPL `(user/main)`
  * `M-x cider-connect-sibling-cljs`
  * Select `shadow` and the name of the shadow build `:devkit`
  * http://localhost:8080
  * Your ClojureScript REPL is connected!

When inside of a CLJC file, evaluations will be sent to both REPLs. So be sure to
wrap anything platform specific in reader conditionals before evaluating them.
Inline results only show a single result, so whichever REPL returns slower wins.
`prn` is your friend if you want to see evaluation results, as it will print in
each REPL buffer the result from that environment.

To prevent being prompted to apply the local variables on every file you open:
* Run `M-x customize` and search for `safe-local-variable-values`
* Add a new entry for `cider-jack-in-nrepl-middlewares`, with the value that is in `.dir-locals.el`. E.g. `("cider.nrepl/cider-middleware" "shadow.cljs.devtools.server.nrepl/middleware")`
* Apply and save in the customize buffer

# Photon Demos and tutorials

* see [src-docs/user/](https://github.com/hyperfiddle/photon/tree/master/src-docs/user) for demos and tutorials
* Start a JVM REPL and jack-in in your usual way
* App entrypoint: src-dev/[user.clj](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.clj), [user.cljs](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.cljs)

Please ask questions in slack! src-docs is a "cookbook" of examples and tutorials that we build in response to your questions.

# Project status = private technical alpha

Updated: 2022 May 21

Current focus is validating functionality through user trials.

May:

- [x] Photon core language at CLJ REPL
- [x] Photon core language at CLJS REPL [x] Cursive [x] Emacs [ ] Calva
- [x] Photon client/server transfer [x] Cursive [x] Emacs [ ] Calva
- [x] Photon-dom basic tutorial level usage (bugs allowed)
- [x] hot code reloading - [x] Cursive, [ ] Emacs, [ ] Calva
- [x] Photon-dom webview with client/server transfer (read only)
- [x] Photon-dom uncontrolled text input
- [x] Concurrent sessions/tabs with shared server state
- [ ] Todos basic demo
- [ ] Basic Photon/Missionary interop for common patterns (buttons, ...)

June:

- [ ] photon-dom todo app (no bugs)
- [ ] tee-shirt orders example app with select options
- [ ] TodoMVC (full example) [ ] client only [ ] client/server transfer 
- [ ] Photon-dom controlled text input [ ] stabilized for network latency
- [ ] 7 GUIs [ ] cljs only [ ] client/server transfer
- [ ] Hyperfiddle staging area

July:

- [ ] client/server transfer improvements
  - [ ] reactive-for with midflight transfer - todo
  - [ ] let bindings not needlessly transferred (workaround - use dynamic bindings instead)
  - [ ] prove transfer correctness with traced IO unit tests

Q2:
- [ ] Photon idioms and component library for common patterns
- [ ] client/server transfer stress tests
- [ ] HFQL spec-driven forms with user interaction and staging area

# Known issues, gaps, gotchas

Updated: 2022 May 21

Broadly, patterns that work are documented with examples and test cases. No reference pattern = expect issues

Photon
- No fn destructuring yet (let destructuring works)
- No recursion yet, but you can hack it with dynamic lambda bindings
- No variable fn arity yet
- no clojure.core/fn inside Photon blocks yet (it is well defined)
- no interop special forms in Photon blocks, including no `js/` access
- Current transfer syntax `~@` is challenging, you should start with a working example. Improving this is June's development priority
- Malformed programs can hang the JVM (including HFQL which is WIP)
  - Open MacOS Activity Monitor and filter by "java" to see if your JVM is hung
  - best to run with Activity Monitor open until we can mitigate it

Photon-dom
- photon-dom renders dom lists in reverse, stable dom rendering is WIP, eta May
- Pending try/catch is half baked

Photon/Missionary interop:
* The Photon/Missionary boundary is currently challenging and undocumented. 
* If you want to do something and don't see an example, please just ask us in Slack and we will provide an example. The idioms are not established yet, your intuition is unlikely to be correct! Example: "How do I connect a button click to a server callback?"
* Missionary needs a lot of work in the docs/examples/errors area. We recommend you follow working examples, otherwise ask and we will create new examples.

# Errors

* `Execution error (ClassNotFoundException) hyperfiddle.photon.Pending` - run the Clojure build step, see setup instructions
* Cursive says `Dependency cycle: hyperfiddle.api -> hyperfiddle.hfql -> hyperfiddle.hfql.impl -> hyperfiddle.api` – Cursive is not correctly handling Clojure 1.11 :as-alias, load the file form by form instead
* `clojure -T:build compile-java` error `-T is no longer supported, use -A with repl, -M for main, or -X for exec` – clojure CLI is not up to date, `brew upgrade clojure`. If you can't update, try `clojure -A:build -X build/compile-java`
* :eval opcode - probably interop syntax, or a macro like assert that expands to interop syntax
* `Unbound var.` Usually means wrong peer, i.e. accessed server-only var on client