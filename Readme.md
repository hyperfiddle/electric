# Electric Clojure – a signals DSL for fullstack web UI, with compiler-managed network sync

Electric (formerly known as Photon) is a **reactive and network-aware Clojure/Script DSL** that fully abstracts over client/server state sync at the programming language layer, in order to achieve **strong composition** across the frontend/backend boundary in dynamic web apps. With Electric, backend expressions (i.e. queries) and frontend expressions (i.e. views) compose directly. There is no incidental network divide. The Electric macros will, at compile time, perform deep graph analysis of your hollistic program's data flow in order to transparently partition and distribute it across the client/server distributed system. There is no client/server dichotomy from the programmer's perspective. All IO and effects are managed.

![](docs/electric-explainer-5.png)

*Figure: This is not RPC or client-side ORM. The Electric compiler performs deep graph analysis of your unified frontend/backend program to automatically determine the optimal network cut, and then compile it into separate client and server target programs that cooperate and anticipate each other's needs.*


- **Fully reactive:** unlike javascript frameworks, in Electric, reactivity is built directly into the programming language itself. Reactive-if, reactive-for, reactive try/catch. When everything is reactive, it feels like nothing is reactive. No observables! No async types! De-load your mind and relax.


- **Multi-tier**: frontend and backend are defined in the same expression, same function, same file. It's not code sharing, it's code *splitting*. Let the compiler infer the boundary from your code, instead of contorting your code — nay, your entire architecture — to fit the boundary.


- **Network-transparent**: Electric closures close over server and client scope bindings, all in the same expression. The Electric compiler uses compile-time static knowledge of your source code to slice your expressions into client and server portions. Right through closures, loops and deeply nested function calls.


- **Strong composition:** Network-transparent Electric functions are true functions. They follow function laws and work at the Clojure/Script REPL. You have lambda, recursion, HOFs, closures, dynamic scope, macros, etc: the full undamaged composition power of Lisp. Goodbye "functional core imperative shell"; with Electric the *entire system is a function*.


- **Multiplayer-native:** everything is automatically multiplayer, 0 LOC cost.


Our mission is to raise the abstraction ceiling in web development in the same way that garbage collection did for functional programming, paving the way for something new.

## Demos, examples, tutorials

Demos

* [Proof of client/server transfer](https://gist.github.com/dustingetz/35f0e036283c49605f73917132931414)
* [Two Clocks – streaming lexical scope](https://gist.github.com/dustingetz/13c99420fe9bf75dd8178c1a633d3bbe)
* [Frontend treeview as direct recursion over filesystem](https://gist.github.com/dustingetz/89ca122af0175933042e481ee9aa59f8)
* [Folder explorer: node_modules paginated from frontend (virtual scroll)](https://gist.github.com/dustingetz/dd67a35d818e3a1bf6733147cf5cdea7)
* [Datomic database browser](https://github.com/hyperfiddle/electric-datomic-browser)
* [TodoMVC](https://gist.github.com/dustingetz/2c1916766be8a61baa39f9f88feafc44)
* [TodoMVC Composed – network transparent composition](https://gist.github.com/dustingetz/bba2aa18acc5de8d2685d3de23bad515)
* [React.js/Reagent interop](https://gist.github.com/dustingetz/9854d23037b55bfab3845539f3e66e02)
* [Electric Painter, by Dag](https://gist.github.com/dustingetz/d58a6134be310e05307ca0b586c30947)
* [Electric Soduku, by s-ol](https://github.com/s-ol/electric-sudoku)
* [Map demo - OpenLayers webcomponents](https://twitter.com/tatut/status/1625192601354641408)

Tutorials

* [Todos Basic](https://github.com/hyperfiddle/electric/blob/master/src-docs/user/todos_simple.cljc)
* [Full-stack webview with datascript](https://github.com/hyperfiddle/electric/blob/master/src-docs/user/demo_4_webview.cljc)
* [Routing integration - goog.history](https://github.com/hyperfiddle/electric/blob/master/src/contrib/electric_goog_history.cljc)
* [Multiplayer chat with presence](https://gist.github.com/dustingetz/3e0761f51137cbf945b701c3ce9e3c74)
* [SQL data backend](https://gist.github.com/dustingetz/1960436eb4044f65ddfcfce3ee0641b7)
* [virtual scroll internals](https://github.com/hyperfiddle/electric/blob/master/src-docs/user/demo_scrollview.cljc)
* Electric Y-Combinator: network-transparent composition

How it works
* [UIs are streaming DAGs (2022)](https://hyperfiddle.notion.site/UIs-are-streaming-DAGs-e181461681a8452bb9c7a9f10f507991)
* [You don't need a web framework, you need a web language (2021)](https://hyperfiddle.notion.site/Reactive-Clojure-You-don-t-need-a-web-framework-you-need-a-web-language-44b5bfa526be4af282863f34fa1cfffc)

## Getting Started

First run the demos:

```shell
git clone git@github.com:hyperfiddle/electric.git
cd electric
yarn                       # optional, only needed for demo of React interop
clj -A:dev -X user/main    # serves demos at http://localhost:8080
```

From the REPL:
* `dev` alias; `(user/main)` compiles assets and serves app. 
* see [src-dev/user.clj](https://github.com/hyperfiddle/electric/blob/master/src-dev/user.clj) & [user.cljs](https://github.com/hyperfiddle/electric/blob/master/src-dev/user.cljs)
* demo source code at [src-docs/user/](https://github.com/hyperfiddle/electric/tree/master/src-docs/user)

Standalone starter repo to fork: 
* https://github.com/hyperfiddle/electric-starter-app

## IDE setup

* [docs/ide_emacs.md](docs/ide_emacs.md)
* [docs/ide_cursive.md](docs/ide_cursive.md)
* [docs/ide_calva.md](docs/ide_calva.md)

## Dependency

```clojure
{:deps {com.hyperfiddle/electric {:mvn/version "v2-alpha-123-ga7fa624f"}}}
```
[![Clojars Project](https://img.shields.io/clojars/v/com.hyperfiddle/electric.svg)](https://clojars.org/com.hyperfiddle/electric)

- Production ready for, let's say back office apps, after 8 months of private user testing and extreme dogfooding in the Hyperfiddle sister project.
- As a maturity indicator, the only low level bug in recent memory was a hash collision triggered by scrolling a server-paginated grid over thousands of server-streamed elements.
- Stack traces aren't great; we do have async stack traces already but they need work

Current development priorities:
* developer experience improvements
* network planner improvements
* language semantics improvements

To date we have focused on correct semantics over syntax and performance. Now that we are useful in production, we are using production learnings to drive our priorities.

**Community**: #hyperfiddle @ clojurians.net for support; follow https://twitter.com/dustingetz for progress updates

## Clojure compat matrix

We target full Clojure/Script compatibility (say 99%). That means you can take a pre-existing Clojure snippet and copy/paste it into an Electric function body and it will "work" and produce the correct result. Including host interop syntax, use of pre-existing macros, etc.

Gaps:

- no variable e/fn arity yet
- no recursion yet - see workaround in [src-docs/user/electric/electric_recursion](https://github.com/hyperfiddle/electric/blob/master/src-docs/user/electric/electric_recursion.cljc)
- reactive multimethods
- reactive protocols
- ...

## Errors and issues
* Requires -Xss2m to compile. The default of 1m ThreadStackSize is exceeded by the Electric compiler due to large macroexpansions resulting in false StackOverflowError during analysis.
* :eval opcode - probably interop syntax, or a macro like assert that expands to interop syntax
* `Unbound var.` Usually means wrong peer, i.e. accessed server-only var on client

## Contributing

* PRs require a signed contributors agreement (like Clojure), DM dustingetz on slack.
* No typo fixes please, we are not all native English speakers and we decided it's not worth it.

## References and reading

Electric Clojure implements a form of arrowized continuous time dataflow programming with extensions for network-transparent function composition.

* [The Haskell School of Music: From Signals to Symphonies (2018)](https://www.amazon.com/Haskell-School-Music-Signals-Symphonies/dp/1108416756) – best starting point
* [The Essence of Dataflow Programming (2006)](http://cs.ioc.ee/~tarmo/papers/essence.pdf) – great category theory overview of dataflow programming
* [Push-pull functional reactive programming, Elliot (2009)](http://conal.net/papers/push-pull-frp/)
* [Breaking down FRP, Jane Street (2014)](https://blog.janestreet.com/breaking-down-frp/)
* [Seven Implementations of Incremental (lecture), Jane Street, 2016](https://www.youtube.com/watch?v=G6a5G5i4gQU)
