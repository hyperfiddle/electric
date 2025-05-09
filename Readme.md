# Electric Clojure – full-stack differential dataflow for UI

`com.hyperfiddle/electric {:mvn/version "v3-alpha-SNAPSHOT"}` · 2024 Dec 11

> [!NOTE]
> Electric v3 is now in private beta! Request beta access here: https://www.hyperfiddle.net/early-access.html


Electric is a new way to build rich, interactive web products that simply have too much interactivity, realtime streaming, and too rich network connections to be able to write all the frontend/backend network plumbing by hand. With Electric, you can compose your client and server expressions directly (i.e. in the same function), and the Electric compiler macros will **infer at compile time the implied frontend/backend boundary** and generate the corresponding full-stack app.

![](docs/electric3-explainer.png)

*Figure: In Electric, client and server expressions compose directly, and the Electric compiler transparently solves the network boundary through a straightforward dataflow graph analysis.*

**How it works:** Unlike request/response frameworks, frontend ORMs, and client-side databases that suffer from request waterfalls, over/under fetching, large payload deserialization and other performance issues that get worse as your codebase and database grows, Electric uses a **custom Clojure/Script compiler to perform deep graph analysis** of your unified frontend/backend program and automatically determine the implied network cut, and then compile your program into separate reactive client and server target programs that cooperate and anticipate each other's needs. See [UIs are streaming DAGs (Getz 2022)](https://hyperfiddle.notion.site/UIs-are-streaming-DAGs-e181461681a8452bb9c7a9f10f507991) for a quick 10 minute video explainer of how this works and why it **actually doesn't result in the request waterfalls you might expect,** even in the presence of deep nesting, loops and control flow.

- **Fully reactive:** unlike javascript frameworks, in Electric, reactivity is built directly into the programming language itself. Reactive-if, reactive-for, reactive lambda. When everything is reactive, it feels like nothing is reactive. No observables! No async types! De-load your mind and relax.


- **Multi-tier**: frontend and backend are defined in the same expression, same function, same file. It's not code sharing, it's code *splitting*. Let the compiler infer the boundary from your code, instead of contorting your code — nay, your entire architecture — to fit the boundary.


- **Network-transparent**: Electric closures close over server and client scope bindings, all in the same expression. The Electric compiler uses compile-time static knowledge of your source code to slice your expressions into client and server portions. Right through closures, loops and deeply nested function calls.


- **Strong composition:** Network-transparent Electric functions are true functions. You have lambda, recursion, HOFs, closures, dynamic scope, macros, etc: the full undamaged composition power of Lisp. Goodbye "functional core imperative shell"; with Electric the **entire system is a function**.

Our mission is to raise the abstraction ceiling in web development in the same way that managed memory did in the 90s, paving the way for something new.


# Lightning talk – 5 minutes

<div align="center">
  <a href="https://share.descript.com/view/nJkFVXo15lM" target="_blank">
    <img alt="lightning talk video" src="./docs/lightning_talk_cover.png" width="480"/>
  </a>
</div>

# Tutorial and live examples

- v3 live tutorial: https://electric.hyperfiddle.net/
- v3 starter app: not yet generally available, [request early access here](https://www.hyperfiddle.net/early-access.html)

# Talks and essays
- [Talk: Electric Clojure v3: Differential Dataflow for UI (Getz 2024)](https://hyperfiddle-docs.notion.site/Talk-Electric-Clojure-v3-Differential-Dataflow-for-UI-Getz-2024-2e611cebd73f45dc8cc97c499b3aa8b8)
- [Talk: Electric Clojure: compiler managed datasync for rich web apps (Getz 2023)](https://hyperfiddle-docs.notion.site/Talk-Electric-Clojure-compiler-managed-datasync-for-rich-web-apps-Getz-2023-e089a8c0caeb456daaf2f9675e3ac4e7)
- [Talk: UIs are streaming DAGs (Getz 2022)](https://hyperfiddle.notion.site/UIs-are-streaming-DAGs-e181461681a8452bb9c7a9f10f507991)
- [Talk: Missionary: a functional approach to massively concurrent application design (Noel 2023)](https://hyperfiddle-docs.notion.site/Talk-Missionary-a-functional-approach-to-massively-concurrent-application-design-Noel-2023-a74748f610c044328d19d038a6daffa1)
- [You don't need a web framework, you need a web language (Getz 2021)](https://hyperfiddle.notion.site/Reactive-Clojure-You-don-t-need-a-web-framework-you-need-a-web-language-44b5bfa526be4af282863f34fa1cfffc)
- [Talk: Functional effects and streaming systems in Clojure (Noel 2021)](https://hyperfiddle-docs.notion.site/Talk-Functional-effects-and-streaming-systems-in-Clojure-Noel-2021-f3f907e5e9b04d08a3be33d53a3cd18e)
- https://clojureverse.org/t/electric-clojure-a-signals-dsl-for-fullstack-web-ui/9788
- https://clojureverse.org/t/signals-vs-streams/9840/1

# Community

* slack support chatroom: #hyperfiddle @ [clojurians.net](https://clojurians.net/)
* follow https://twitter.com/dustingetz for progress updates
* **Contributing:** we do not currently accept PRs against Electric itself as this has historically not been productive for us. However, **we'd love for you to contribute demos!** Many of our coolest demos were started by early users, this is an amazing and productive way for you to get involved and collaborate with us. This will require a signed contributors agreement (like Clojure), DM dustingetz on slack.

# License

Electric v3 is free for bootstrappers and non-commercial use, but is otherwise a commercial project, which helps us continue to invest and maintain payroll for a team of 4. See [license change announcement](https://tana.pub/lQwRvGRaQ7hM/electric-v3-license-change).
