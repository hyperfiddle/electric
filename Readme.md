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

It's called Photon because every named binding in a Photon program can be thought of as simultaneously a reactive flow and a value.

> Albert Einstein, on the wave-particle duality: *"It seems as though we must use sometimes the one theory and sometimes the other, while at times we may use either. We are faced with a new kind of difficulty. We have two contradictory pictures of reality; separately neither of them fully explains the phenomena of light, but together they do."*

# Project status = private technical alpha
(Updated 2022 May 15)
* Currently we are testing the core Photon language experience (Reactive Clojure). If that goes well we will layer in basic usage of Photon-dom and client/server transfer, at the basic tutorial example level of sophistication.
* Next — after core language rough edges are identified and worked out – we will provide TodoMVC and 7GUIs examples using photon-dom and client/server transfer. ETA = June?
* Finally, we will test "HFQL", a UI query/render DSL built in Photon. HFQL is currently half-baked.

# Setup
```bash
git clone ...
cd photon
git submodule update --init --recursive
brew install maven
pushd vendor/missionary && mvn clean && mvn compile && mvn install && popd
yarn       # or npm install

# Sanity check that it's working:
clj -A:dev -X user.photon-5-entrypoint/main
```

# Photon Tutorial
* Start a JVM REPL and jack-in in your usual way
* Read `src-docs/user/photon_*`, follow along in your REPL and see the tests pass

# Standalone Examples to run
* (wip) tee-shirt orders: `clj -X:devkit :main user.orders-ui/main`

# Known Issues
(updated 2022 May 14)

What works?

- See Photon test coverage; tests are passing
- try/catch, case, if, p/fn, p/defn, p/def, binding, all work

Photon issues and language gaps
- No destructuring yet
- No recursion yet
- No variable fn arity yet
- no clojure.core/fn inside Photon blocks yet
- Pending will replay effects 
- What works has test coverage
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
* cider-jack-in with deps alias `dev`
    * (I think you need to run cider-jack-in with prefix argument to configure the alias?)