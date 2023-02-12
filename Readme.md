# Electric Clojure – a signals DSL for fullstack web UI, with compiler-managed network sync

Electric (formerly known as Photon) is a **reactive and network-aware Clojure/Script DSL** that fully abstracts over client/server data sync at the programming language layer, in order to achieve **strong composition** across the frontend/backend boundary in dynamic web apps. With Electric, backend expressions (i.e. queries) and frontend expressions (i.e. views) compose directly. There is no artificial network divide. The Electric macros will, at compile time, perform deep graph analysis of your unified program's data flow in order to transparently partition and distribute it across the client/server distributed system. There is no client/server dichotomy from the programmer's perspective. All IO and effects are managed.

![](docs/electric-explainer-5.png)

*Figure: This is not RPC or client-side ORM. The Electric compiler performs deep graph analysis of your unified frontend/backend program to automatically determine the optimal network cut, and then compile it into separate client and server target programs that cooperate and anticipate each other's needs.*


- **Fully reactive:** unlike javascript frameworks, in Electric, reactivity is built directly into the programming language itself. Reactive-if, reactive-for, reactive try/catch. When everything is reactive, it feels like nothing is reactive. No async types! No function coloring problem!


- **Multi-tier**: frontend and backend are defined in the same expression, same function, same file. It's not code sharing, it's code *splitting*. Let the compiler infer the boundary from your code, instead of contorting your code — nay, your entire architecture — to fit the boundary.


- **Network-transparent**: Electric closures close over server and client scope bindings, all in the same expression. The Electric compiler uses compile-time static knowledge of your source code to slice your expressions into client and server portions, right through closures, loops and deeply nested function calls.


- **Multiplayer-native:** everything is automatically multiplayer, 0 LOC cost.


Our mission is to raise the abstraction ceiling in web development in the same way that garbage collection did for functional programming, paving the way for something new.

## Demos
* [Photon progress update Dec 2022](https://hyperfiddle.notion.site/Photon-progress-Dec-2022-5416dda526e24e5ab7ccb7eb48c797ed)
* [Photon progress update June 2022](https://hyperfiddle.notion.site/Photon-progress-June-2022-57aee367c20e45b3b80366d1abe4fbc3)

## How it works
* [UIs are streaming DAGs (2022)](https://hyperfiddle.notion.site/UIs-are-streaming-DAGs-e181461681a8452bb9c7a9f10f507991)
* [You don't need a web framework, you need a web language (2021)](https://hyperfiddle.notion.site/Reactive-Clojure-You-don-t-need-a-web-framework-you-need-a-web-language-44b5bfa526be4af282863f34fa1cfffc)

## Dependency

```clojure
; stable
{:deps {com.hyperfiddle/electric {:mvn/version "v2-alpha-0-g40c3384e"}}}
```

- Production ready for, let's say back office apps, after 8 months of private user testing and extreme dogfooding in the Hyperfiddle sister project.
- As a maturity indicator, the only low level bug in recent memory was a hash collision triggered by scrolling a server-paginated grid over thousands of server-streamed elements.
- Stack traces aren't great; we do have async stack traces already but they need work

Roadmap
* developer experience
* network planner improvements
* language semantics improvements

## Community

* #hyperfiddle @ clojurians.net for support
* follow https://twitter.com/dustingetz for progress updates

## Getting Started

Standalone starter repo to fork:
* https://github.com/hyperfiddle/electric-starter-app

Demos, examples, tutorials are in this repo, see [src-docs/user/](https://github.com/hyperfiddle/photon/tree/master/src-docs/user).

* `clj -A:dev -X user/main` serves demos at `http://localhost:8080`
* `dev` alias;  `(user/main)` compiles assets and serves app. see [src-dev/user.clj](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.clj) & [user.cljs](https://github.com/hyperfiddle/photon/blob/master/src-dev/user.cljs)

## IDE setup

* [docs/ide_emacs.md](docs/ide_emacs.md)
* [docs/ide_cursive.md](docs/ide_cursive.md)

## Clojure compat matrix

We target full Clojure/Script compatibility (say 99%). That means you can take a pre-existing Clojure snippet and copy/paste it into an Electric function body and it will "work" and produce the correct result. Including host interop syntax, use of pre-existing macros, etc.

Gaps:

- no variable e/fn arity yet
- no recursion yet - see workaround in [src-docs/user/photon/photon_recursion](https://github.com/hyperfiddle/electric/blob/master/src-docs/user/photon/photon_recursion.cljc)
- reactive multimethods
- reactive protocols
- ...

## Errors and issues
* Requires -Xss2m to compile. The default of 1m ThreadStackSize is exceeded by the Electric compiler due to large macroexpansions resulting in false StackOverflowError during analysis.
* :eval opcode - probably interop syntax, or a macro like assert that expands to interop syntax
* `Unbound var.` Usually means wrong peer, i.e. accessed server-only var on client
