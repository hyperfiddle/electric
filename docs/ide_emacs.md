# Emacs setup

# simple configuration
* C-u M-x cider-jack-in-clj
* append ":dev" to end of the command string like "-M:cider/nrepl:dev"

# alternate configuration

Create `.dir-locals.el`
```
((nil
  (cider-clojure-cli-global-options . "-A:dev")
  (cider-jack-in-nrepl-middlewares . ("cider.nrepl/cider-middleware" "shadow.cljs.devtools.server.nrepl/middleware")))) 
```

* .dir-locals.el has some project-specific config. When you first open a Clojure file, you'll be prompted to allow that config to be applied. Answer `y`.
* Then, to jack in: `M-x sesman-start`; Choose `cider-jack-in-clj`; "Your Clojure REPL is connected"
* Starting a CLJS REPL:
 * Start a CLJS build from the Clojure REPL `(user/main)`
 * `M-x cider-connect-sibling-cljs`
 * Select `shadow` and the name of the shadow build `:devkit`
 * http://localhost:8080 ; "Your ClojureScript REPL is connected!"

When inside of a CLJC file, evaluations will be sent to both REPLs. So be sure to wrap anything platform specific in reader conditionals before evaluating them. Inline results only show a single result, so whichever REPL returns slower wins. `prn` is your friend if you want to see evaluation results, as it will print in each REPL buffer the result from that environment.

To prevent being prompted to apply the local variables on every file you open:
* Run `M-x customize` and search for `safe-local-variable-values`
* Add a new entry for `cider-jack-in-nrepl-middlewares`, with the value that is in `.dir-locals.el`. E.g. `("cider.nrepl/cider-middleware" "shadow.cljs.devtools.server.nrepl/middleware")`
* Apply and save in the customize buffer
