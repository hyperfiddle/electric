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

# Setup and healthcheck
```bash
git clone ...
cd photon
clj -A:dev -X user/main     # healthcheck app http://localhost:8080
```

* `dev` alias
* `(user/main)` compiles assets and serves app

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

Updated: 2022 August 4

Current focus is addressing feedback from beta users.

- [x] Photon core language at CLJ REPL
- [x] Photon core language at CLJS REPL [x] Cursive [x] Emacs [ ] Calva
- [x] Photon client/server transfer [x] Cursive [x] Emacs [ ] Calva
- [x] Photon-dom basic tutorial level usage (bugs allowed)
- [x] hot code reloading - [x] Cursive, [ ] Emacs, [ ] Calva
- [x] Photon-dom webview with client/server transfer (read only)
- [x] Photon-dom uncontrolled text input
- [x] client/server transfer stress tests
- [x] Concurrent sessions/tabs with shared server state
- [x] Todos basic demo
- [x] Basic Photon/Missionary interop for common patterns (buttons, ...)
- [x] photon-dom todo app (no bugs)
- [x] TodoMVC (full example) [x] client/server transfer 
- [x] Photon-dom controlled text input [ ] stabilized for network latency
- [*] Photon idioms and component library for common patterns
- [ ] 7 GUIs [ ] cljs only [ ] client/server transfer
- [ ] tee-shirt orders example app with select options
- [ ] HFQL spec-driven forms with user interaction and staging area

# Known issues, gaps, gotchas

Updated: 2022 August 4

Broadly, patterns that work are documented with examples and test cases. No reference pattern = expect issues

Current issues and missing features:
- no clojure.core/fn yet inside Photon blocks, it's coming. Use `partial` for now
- no fn destructuring yet
- no recursion yet - see workaround in [demo_7_explorer.cljc](https://github.com/hyperfiddle/photon/tree/master/src-docs/user/demo_7_explorer.cljc) 
- vno ariable fn arity yet
- no interop special forms in Photon blocks, including no `js/` access
- "duplicate effects" – you'll know it when you see it, fix is active WIP
- `Pending` state is getting redesigned

Errors
* Cursive says `Dependency cycle: hyperfiddle.api -> hyperfiddle.hfql -> hyperfiddle.hfql.impl -> hyperfiddle.api` – Cursive is not correctly handling Clojure 1.11 :as-alias, load the file form by form instead
* :eval opcode - probably interop syntax, or a macro like assert that expands to interop syntax
* `Unbound var.` Usually means wrong peer, i.e. accessed server-only var on client
