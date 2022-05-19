# Hyperfiddle Photon – a streaming Clojure/Script dialect with compiler-managed client/server datasync

Photon lets you express a frontend/backend web application as a single unified Clojure/Script expression that transcends the client/server boundary.

```clojure
(p/defn View [db state]
  (p/client
    (dom/div
      (let [email (dom/input)]
        (dom/h1 "Your orders")
        (dom/table
          (p/for [x (p/server xs (query-database db (p/client (:filter state))))]
            (dom/tr (pr-str x))))))))

; Note: This is our target future syntax, we're not quite there yet.
```

It's called Photon because every point in a Photon form can be thought of as simultaneously a reactive flow and a value.

> Albert Einstein, on the wave-particle duality: *"It seems as though we must use sometimes the one theory and sometimes the other, while at times we may use either. We are faced with a new kind of difficulty. We have two contradictory pictures of reality; separately neither of them fully explains the phenomena of light, but together they do."*

# Project status = private technical alpha

Updated: 2022 May 19

Validated functionality:

- [x] Photon core language at CLJ REPL
- [x] Photon core language at CLJS REPL [x] Cursive [x] Emacs [ ] Calva
- [x] Photon client/server transfer [x] Cursive [x] Emacs [ ] Calva
- [x] Photon-dom basic tutorial level usage (bugs allowed)
- [x] hot code reloading - [x] Cursive, [ ] Emacs, [ ] Calva
- [x] Photon webview with client/server transfer (read only)
- [x] Photon webview with client/server transfer and dom/input
- [ ] photon-dom todomvc level usage (no bugs)
- [ ] crud forms [ ] query/view [ ] form interaction [ ] staging area
- [ ] Transfer improvements
  - [ ] reactive-for with midflight transfer - todo
  - [ ] let bindings not needlessly transferred (workaround - use dynamic bindings instead)
  - [ ] validate transfer correctness with traced IO unit tests
- [ ] TodoMVC [ ] cljs only [ ] client/server transfer
- [ ] 7 GUIs [ ] cljs only [ ] client/server transfer
- [ ] HFQL - not close

# Setup
```bash
git clone ...
cd photon
git submodule update --init --recursive
yarn       # or npm install

# Sanity check that it's working:
clj -X:devkit :main user.demo-healthcheck/main
```

# Photon Demos

Updated: 2022 May 18

* demo-healthcheck
* demo-server-toggle
* demo-system-properties
* demo-webview
* ~~orders-ui~~ not working
* ~~hytradboi~~ not working
* ~~todomvc~~ not working
* ~~browser~~ not working

Instructions: todo

# Photon Language Tutorial
* Start a JVM REPL and jack-in in your usual way
* Read `src-docs/user/photon_*`, follow along in your REPL and see the tests pass

# Standalone Examples to run
* ~~tee-shirt orders: `clj -X:devkit :main user.orders-ui/main`~~ (broken)
* clj -X:devkit :main user.demo-system-properties/main (buggy dom cleanup)

# Known Issues

updated: 2022 May 18

What works?

- See Photon test coverage; tests are passing. 
- try/catch, case, if, p/fn, p/defn, p/def, binding, all work with test coverage
- No passing test = expect possible issues

Photon issues and language gaps
- No fn destructuring yet (let destructuring works)
- No recursion yet
- No variable fn arity yet
- no clojure.core/fn inside Photon blocks yet
- Pending will replay effects which is not always what you want
- photon-dom renders dom lists in reverse, stable dom rendering is WIP, eta May
- Malformed programs can hang the JVM (including HFQL which is WIP)
  - Open MacOS Activity Monitor and filter by "java" to see if your JVM is hung
  - best to run with Activity Monitor open until we can mitigate it
- Current transfer syntax `~@` is challenging
  - Start with a working example, there are edge cases and the error messages are often unhelpful
  - Much better syntax is coming soon (after stable dom rendering)

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

Emacs
* ? cider-jack-in with deps alias `dev`
* ? (I think you need to run cider-jack-in with prefix argument to configure the alias?)
* None of Team Hyperfiddle is currently running emacs and there are diverse setups
* todo