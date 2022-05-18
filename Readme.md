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

Updated 2022 May 18:

- [x] Photon core language at CLJ REPL
- [x] Photon core language at CLJS REPL with client/server transfer - JS/JVM [x] Cursive [ ] Emacs [ ] Calva
- [x] Photon-dom basic tutorial level usage (bugs allowed)
- [ ] photon-dom todomvc level usage (no bugs)
- [ ] hot code reloading - [ ] Cursive, [ ] Emacs, [ ] Calva
- [x] Photon webview with client/server transfer (query only)
- [x] Photon webview with dom/input and transfer
- [ ] crud forms [ ] query/view [ ] form interaction [ ] staging area
- [ ] Transfer improvements
  - [ ] Photon core language instrumented for traced IO to understand client/server transfer at REPL
  - [ ] reactive-for with midflight transfer - todo
  - [ ] validate transfer correctness
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

Updated 2022 May 18:

* demo-server-toggle
* demo-system-properties
* orders-ui
* ~~hytradboi~~ not working
* ~~todomvc~~ not working

Instructions: todo

# Photon Language Tutorial
* Start a JVM REPL and jack-in in your usual way
* Read `src-docs/user/photon_*`, follow along in your REPL and see the tests pass

# Standalone Examples to run
* ~~tee-shirt orders: `clj -X:devkit :main user.orders-ui/main`~~ (broken)
* clj -X:devkit :main user.demo-system-properties/main (buggy dom cleanup)

# Known Issues
(updated 2022 May 14)

What works?

- See Photon test coverage; tests are passing. 
- try/catch, case, if, p/fn, p/defn, p/def, binding, all work with test coverage
- No passing test = expect possible issues

Photon issues and language gaps
- No destructuring yet
- No recursion yet
- No variable fn arity yet
- no clojure.core/fn inside Photon blocks yet
- Pending will replay effects which is not always what you want
- HFQL is half baked (what works has test coverage)
- Malformed programs can hang the JVM (including HFQL which is WIP)
  - Open MacOS Activity Monitor and filter by "java" to see if your JVM is hung
  - best to run with Activity Monitor open until we can mitigate it
- photon-dom renders dom lists in reverse, stable dom rendering is WIP, eta May
- Current transfer syntax `~@` is challenging
  - Today you must start with working examples, there are tons of surprising edge cases and the errors are bad
  - if you there isn't a test documenting it, don't assume it works
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