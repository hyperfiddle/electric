# Hyperfiddle Photon – a multi-tier Clojure/Script dialect for fullstack web apps with managed server data-fetching

Photon lets you express a frontend/backend web application as a single unified Clojure/Script expression that transcends the client/server boundary.

```clojure
(p/defn Teeshirt-orders [db]
  (p/client
    (dom/div
      (let [!email (atom "") email (p/watch !email)]
        (ui/input {::ui/input-event (p/fn [e] (reset! !email (:value dom/node)))})
        (dom/h1 "Your tee-shirt orders")
        (dom/table
          (p/server
            (p/for [x (query-teeshirt-orders db email)]
              (p/client (dom/tr (pr-str x))))))))))
```

It's called Photon because every point in a Photon form can be thought of as simultaneously a reactive flow and a value.

> Albert Einstein, on the wave-particle duality: *"It seems as though we must use sometimes the one theory and sometimes the other, while at times we may use either. We are faced with a new kind of difficulty. We have two contradictory pictures of reality; separately neither of them fully explains the phenomena of light, but together they do."*

# Setup and healthcheck
```bash
clj -A:dev -X user/main     # healthcheck app http://localhost:8080
```
* `dev` alias
* `(user/main)` compiles assets and serves app

Check tests:
```bash
yarn
ci/run_tests_all.sh
```
[![JVM](https://github.com/hyperfiddle/electric/actions/workflows/tests_clj.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/electric/actions/workflows/tests_clj.yml)
[![Node](https://github.com/hyperfiddle/electric/actions/workflows/tests_node.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/electric/actions/workflows/tests_node.yml)
[![Browser](https://github.com/hyperfiddle/electric/actions/workflows/tests_browser.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/electric/actions/workflows/tests_browser.yml)

# Examples
* see [src-docs/user/](https://github.com/hyperfiddle/photon/tree/master/src-docs/user) for demos and tutorials
* Start a JVM REPL under `:dev` alias and jack-in in your usual wayl; `(user/main)`
* App entrypoint: src-dev/[user.clj](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.clj), [user.cljs](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.cljs)

Please ask questions in slack so we can turn them into examples!

# IDE setup
Cursive:
* new deps.edn project from existing sources
* `dev` alias
* Add Configuration
  * new Clojure REPL - Local
  * run with Deps
  * alias: `dev`
* Run configuration

Emacs:
* C-u M-x cider-jack-in-clj
* append ":dev" to end of the command string like "-M:cider/nrepl:dev"

# Project status = private technical alpha
Updated: 2022 August 9

Current focus is TodoMVC and odopms

- [x] Photon core language at CLJ REPL
- [x] Photon core language at CLJS REPL [x] Cursive [x] Emacs [x] Calva
- [x] Photon client/server transfer [x] Cursive [x] Emacs [x] Calva
- [x] Photon-dom basic tutorial level usage (bugs allowed)
- [x] Photon-dom hot code reloading - [x] Cursive, [x] Emacs, [x] Calva
- [x] Photon-dom webview with client/server transfer (read only)
- [x] Concurrent sessions/tabs with shared server state
- [x] Photon-dom controlled text input [*] stabilized for network latency
- [x] TodoMVC [x] client/server transfer
- [x] client/server transfer stress tests
- [x] Photon/Missionary interop
- [*] 7 GUIs
- [*] pending states and error handling - robust idioms
- [ ] tee-shirt orders example app with select options
- [ ] HFQL spec-driven forms with user interaction and staging area

# Known issues, feature gaps, gotchas
Updated: 2022 August 9

- no `clojure.core/fn` yet inside Photon blocks, it's coming. Use `partial` for now
- no fn destructuring yet, no variable fn arity yet
- no recursion yet - see workaround in [demo_7_explorer.cljc](https://github.com/hyperfiddle/photon/tree/master/src-docs/user/demo_7_explorer.cljc)
- no interop special forms in Photon blocks, including no `js/` access

Errors
* Cursive says `Dependency cycle: hyperfiddle.api -> hyperfiddle.hfql -> hyperfiddle.hfql.impl -> hyperfiddle.api` – Cursive is not correctly handling Clojure 1.11 :as-alias, load the file form by form instead
* :eval opcode - probably interop syntax, or a macro like assert that expands to interop syntax
* `Unbound var.` Usually means wrong peer, i.e. accessed server-only var on client

# References
* http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf
* 