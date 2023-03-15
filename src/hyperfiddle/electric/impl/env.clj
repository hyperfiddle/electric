(ns hyperfiddle.electric.impl.env
  "Logic to resolve cljs or clj vars (and macros) and to reload source files in dev mode."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [cljs.analyzer :as cljs]
            [cljs.env :as env]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.electric.impl.compiler :as-alias c])
  (:import (clojure.lang Var)
           (java.io FileNotFoundException)))

;; Dedicated namespace because:
;; - used by both electric compiler and HFQL
;; - non trivial logic which requires the reader to load all the knowledge in their mind
;;   Many moving parts which are easier to think about in isolation.
;;   Used to be inlined in the compiler namespace - required a lot of scrolling and navigation.


;; About ShadowCljs and Hot Code reloading:

;; By design, shadow recompiles and reload:
;; - the current saved file
;; - all direct dependents of the saved file
;; Shadow does not recompile all transitive dependents by default.

;; Shadow has a setting `:devtools {:reload-strategy :full}` which will
;; recompile and reload all transitive dependents.
;; https://code.thheller.com/blog/shadow-cljs/2019/08/25/hot-reload-in-clojurescript.html#recompile-logic

;; This actually doesn’t help us because:
;; - we are custom compiler inside of the cljs compiler compilation phase
;; - we want the current ns to recompile
;; - we want the compiler entrypoint to recompile (and rerun the electric compiler)
;; - recompiling namespaces in between the current ns and the entrypoint won’t impact the compilation.

;; This is why we need ^:dev/always on the compiler entrypoint.



;; Why did we use to need :require-macros on every namespace?

;; There are two independent memory environments:
;; - Clojure environnement — result of eval at the REPL
;; - ClojureScript compiler environnement — result of running the CLJS compiler on the codebase


;; Given this program:
;; ``` ; foo.cljc
;; (ns foo)              ; note: not using a (comment …) form for this example, ns form confuses clj-kondo
;; (def bar 1)
;; ```

;; - If loaded at the REPL, will create a `foo` ns and a `bar` var in the  Clojure environnement.
;; - If compiled by CLJS compiler, will create a `foo` ns and a `bar` var in the CLJS compiler environment.


;; The following program will create `foo` and `bar` in both environments. There
;; will be two `foo` and two `bar`.

;; ``` ; foo.cljc
;; (ns foo #?(:cljs (:require-macros [foo])))
;; (def bar 1)
;; ```

;; => The electric compiler always resolves vars on the peer defined by
;; `e/client` or `e/server`. But `e/def` is a special case and should be
;; resolved in either environnement no matter the peer.


;; When a e/server block is analyzed, it resolves symbols to check if they are
;; macros or e/defs. Vars needs to be loaded in the Clojure Environnement JVM
;; side to be resolvable. :require-macros trigger a clojure `require
;; ns :reload`, loading vars into the Clojure environnement. Instead of
;; requiring users to write :require-macros in every ns, the electric compiler
;; can call `require` just before resolving a var in a clojure ns. This
;; requiring strategy only requires the ns if necessary (if the corresponding
;; source file has been touched).


;; How to resolve an alias:
;; - Server
;;   - try to resolve a clj alias definition (might be behind a #?:clj flag)
;;     - this requires loading the current clj namespace
;;   - try to resolve a cljs alias (we might be in cljs file within an e/server block)
;;   - try to resolve a cljs macro alias (require-macros)
;; - Client
;;   - try to resolve a cljs alias
;;   - try to resolve a cljs macro alias (require-macros)


;; How to resolve a var:
;; - Server
;;   - try to resolve a cljs macro var
;;     - if macro found in cljs compiler state, return macro var
;;       - only if it’s not a twin var
;;   - otherwise
;;     - resolve symbol alias based on
;;       - cljs compiler current ns info
;;       - if not found, retry based ond resolved current clojure namespace
;;     - find target ns fully qualified name
;;     - call `(require target-ns :reload)` if necessary
;;     - find var in target ns
;; - Client
;;   - try to resolve symbol in cljs compiler state
;;   - otherwise, try to resolve a macro var in cljs compiler state
;;   - if var is a local binding var (yes this exists), return nil
;;   - try to find a twin clojure var (e.g. inc)
;;     - if it exists, return it (why is it needed)
;;     - otherwise return current found var


;; How to resolve a runtime symbol:
;; - Server
;;   - resolve the var
;;     - if var is a macro, or an e/def -> return nil
;;     - otherwise return the fully qualified var name
;; - Client
;;   - if it resolves to a cljs var or clj class
;;     - return the fully qualified symbols
;;   - if it resolves to a clj var (twin var)
;;    - return the cljs var (not twin clj var) fully qualified name


;; GG: Abstraction on resolved vars.
;;     A symbol may resolve to:
;;     - nil
;;     - a cljs var description (a map)
;;     - a cljs macro var description (a map)
;;     - a clojure var (clojure.lang.Var)
;;       - which can be a twin of a cljs var (e.g. cljs.core/inc has two versions: a cljs runtime function and a clj macro)
;;     - a JVM Class
(defprotocol IVar
  (get-var [this])
  (var-name [this])
  (var-meta [this])
  (is-macro [this])
  (is-node [this]))

(defprotocol ICLJSTwinVar (is-twin [this]))

(deftype CljVar [var twin?]
  IVar
  (get-var  [_this] var)
  (var-name [_this] (let [m (meta var)] (symbol (name (ns-name (:ns m))) (name (:name m)))))
  (var-meta [_this] (meta var))
  (is-macro [_this] (.isMacro ^Var var))
  (is-node  [this] (contains? (var-meta this) ::c/node))
  ICLJSTwinVar
  (is-twin  [_this] twin?))

(deftype CljsVar [var]
  IVar
  (get-var  [_this] var)
  (var-name [_this] (:name var))
  (var-meta [_this] (:meta var))
  (is-macro [this] (:macro (or (var-meta this) var)))
  (is-node  [this] (contains? (or (var-meta this) var) ::c/node)))

(deftype CljClass [klass]
  IVar
  (get-var  [_this] klass)
  (var-name [_this] (symbol (.getName ^Class klass)))
  (var-meta [_this] nil)
  (is-macro [_this] false)
  (is-node  [_this] false))

(defn resolve-ns
  "Builds a description of a namespace. Returns nil if no namespace can be found for the given symbol.
  Does not compute `:interns` because it’s expensive and the Electric compiler
  cannot cache it. Use `find-interned-var` to lookup for interns."
  ;; `ns-interns` is a filter over `ns-map` (a huge map) while `find-interned-var` is a direct lookup.
  [ns-sym]
  (when-let [ns (find-ns ns-sym)]
    {:mappings (assoc (ns-map ns)
                 'in-ns #'clojure.core/in-ns
                 'ns    #'clojure.core/ns)
     :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                 {} (ns-aliases ns))
     :ns       (ns-name ns)}))

(defn ns-filename "Given a symbol identifying a namespace, return the corresponding file path"
  [sym]
  (-> (name sym)
    (str/replace #"\." "/")
    (str/replace #"-" "_")))

(defn find-file [relative-path]
  (when-let [res (io/resource relative-path)]
    (try (io/file res)
         (catch IllegalArgumentException _
           ;; resource is not a file on the classpath. E.g. jar:// sources are
           ;; not files. We also don’t want to reload them.
           nil))))

(defn resolve-file
  "Resolve a source file from namespace symbol.
  Precedence:
  - cljc,
  - cljs if we are compiling clojurescript,
  - clj otherwise."
  [env ns-sym]
  (let [file-name (ns-filename ns-sym)
        cljc      (find-file (str file-name ".cljc"))
        clj       (find-file (str file-name ".clj"))
        cljs      (find-file (str file-name ".cljs"))]
    (if (and cljc (.exists cljc))
      cljc
      (if (:js-globals env)
        (when (and cljs (.exists cljs))
          cljs)
        (when (and clj (.exists clj))
          clj)))))

(defn file-timestamp [^java.io.File file] (when (some? file) (.lastModified file)))
(defn file-extension [^java.io.File file] (when (some? file) (peek (str/split (.getName file) #"\."))))

(def !macro-load-cache (atom {})) ; records the last time a source file was loaded

(defn load-ns! [ns-sym & args]
  (try (apply require ns-sym args) ; will throw if source code is invalid CLJ(C)
       (catch FileNotFoundException _) ; Some namespaces don’t map to files (e.g. Math)
       (catch Throwable e              ; TODO improve error messages
         (log/warn e "Failed to load" ns-sym))))

(defn maybe-load-clj-ns! [env ns-sym]
  (when-not (re-find #"^hyperfiddle\.electric" (str ns-sym)) ; electric doesn't reload itself
    (let [last-loaded (get-in @!macro-load-cache [ns-sym ::last-loaded])]
      (if (find-ns ns-sym) ; ns is already loaded on JVM?
        (when-let [file (resolve-file env ns-sym)] ; if source is in userland
          (when (not= "cljs" (file-extension file)) ; cljs files are not reloadable, homonym reloading is handled by shadow with :require-macros
            (let [current-timestamp (file-timestamp file)]
              (if (some? last-loaded)
                (when (< last-loaded current-timestamp) ; source is newer
                  (log/debug "reloading" ns-sym)
                  (load-ns! ns-sym :reload))
                (when (empty? (ns-interns (find-ns ns-sym)))  ; FIXME support macros defined in cljs (no need to load corresponding clj ns if cljs macro is available in cljs space)
                  (log/trace "initial load" ns-sym)
                  (load-ns! ns-sym)))
              (swap! !macro-load-cache assoc-in [ns-sym ::last-loaded] current-timestamp))))
        ;; ns is not loaded
        (if-let [file (resolve-file env ns-sym)]  ; try to load it from userland source
          (when-not (= "cljs" (file-extension file)) ; cljs files are not loadable on the JVM
            (let [current-timestamp (file-timestamp file)]
              (log/trace "loading source" ns-sym)
              (load-ns! ns-sym)
              (swap! !macro-load-cache assoc-in [ns-sym ::last-loaded] current-timestamp)))
          (do (log/trace "loading external" ns-sym)  ; maybe ns-sym refers to a library
              (load-ns! ns-sym)))))))

(defn peer-language
  "Given a compiler environment, return :clj or :cljs, according to the peer-config map (set by `e/boot`)"
  [env]
  (case (::c/local env)
    (true nil) (get-in env [::c/peers-config ::c/local])
    false      (get-in env [::c/peers-config ::c/remote])))

(defn resolve-clj-alias
  "Given a symbol, resolve it to a fully qualified symbol according to the current ns definition in `env`."
  [env sym]
  (if (simple-symbol? sym) sym
      (let [ns-sym (if (:js-globals env) (:name (:ns env)) (:ns env))]
        (maybe-load-clj-ns! env ns-sym) ; can only resolve a clj alias if the namespace is loaded
        (as-> sym $
          (symbol (namespace $)) ; extract namespace part of sym
          (get-in (resolve-ns ns-sym) [:aliases $] $) ; expand to fully qualified form
          (symbol (str $) (name sym))
          (with-meta $ (meta sym))
          ))))

(defn resolve-alias "Expand a qualified symbol to its fully qualified form, according to ns aliases."
  [env sym]
  (case (peer-language env)
    :clj  (resolve-clj-alias env sym)
    :cljs (or (cljs/resolve-ns-alias env sym) (cljs/resolve-macro-ns-alias env sym))))

(defn is-cljs-file? [env] (and (:js-globals env) (some-> env :ns :meta :file (str/ends-with? ".cljs"))))

(defmacro no-warn
  "Localy disable a set of cljs compiler warning.
  Usage: `(no-warn #{:undeclared-ns} (cljs/resolve env sym))`"
  [disabled-warnings & body]
  `(binding [cljs/*cljs-warnings* (reduce (fn [r# k#] (assoc r# k# false)) cljs/*cljs-warnings* ~disabled-warnings)]
     ~@body))

(defn resolve-cljs-var ; GG: adapted from cljs.analyzer.api/resolve, return an IVar.
  "Given an analysis environment resolve a var. Analogous to
   clojure.core/resolve"
  [env sym]
  {:pre [(map? env) (symbol? sym)]}
  ;; First try to resolve a cljs var, then fallback to resolving a cljs macro var
  (when (:js-globals env)
    (let [!found? (volatile! true)
          ns-sym (:name (:ns env))
          var     (when-some [ns (find-ns ns-sym)]
                    (binding [cljs/*private-var-access-nowarn* true]
                      (let [klass (clojure.lang.Compiler/maybeResolveIn ns sym)]
                        (if (class? klass)
                          (CljClass. klass)
                          (CljsVar. (no-warn #{:undeclared-ns} (cljs/resolve-var env sym
                                                                 (fn confirm [env prefix suffix]
                                                                   (cljs/confirm-var-exists env prefix suffix
                                                                     (fn missiing-fn [_env _prefix _suffix]
                                                                       (vreset! !found? false)))))))))))]
      (if (and @!found? var)
        (if (instance? CljClass var)
          var
          (cond
            (= :local (:op (get-var var))) nil ; ignore lexical bindings

            ;; If the symbol is unqualified, the var will resolve in current ns.
            ;; The returned value could therefor describe
            ;; a :use, :refer, :use-macro, :refer-macro, or :rename. This var
            ;; description would only contain the var :name and :ns, missing all
            ;; other var info - especially if it is a macro or not. In this case,
            ;; we resolve the var again, using the fully qualified name. This
            ;; ensures the returned var definition includes all info about the
            ;; var.
            (and (simple-symbol? sym) (not= ns-sym (namespace (var-name var))))
            (resolve-cljs-var env (var-name var))

            :else var))
        (when-some [v (cljs/resolve-macro-var env sym)]
          (CljsVar. v))))))

(defn resolve-cljs
  "Resolve a cljs var, which can be either
  - a regular cljs var (a map)
  - a cljs macro var (a map)
  - a cljs twin var (e.g. cljs.core/inc)"
  ([env sym] (resolve-cljs env sym true))
  ([env sym include-potential-twin-var?]
   (when (:js-globals env)
     (let [var (resolve-cljs-var env sym)] ; resolve cljs var decription (a map)
       (if-not include-potential-twin-var?
         var
         (if-let [expander (cljs/get-expander (if (some? var) (var-name var) sym) env)] ; find corresponding clojure var
           (CljVar. expander true) ; mark this clj var as a twin
           var))))))

(defn find-interned-var
  "Look up for an interned var in a namespace. Efficient direct lookup."
  [^clojure.lang.Namespace ns var-sym]
  (let [^clojure.lang.Symbol var-name (if (simple-symbol? var-sym) var-sym (symbol (name var-sym)))]
    (.findInternedVar ns var-name)))

(defn clj-env "Given a cljs compiler env, return a clojure compiler compatible env."
  [?cljs-env]
  (if (:js-globals ?cljs-env)
    (-> ?cljs-env (dissoc :js-globals) (assoc :ns (:name (:ns ?cljs-env))))
    ?cljs-env))

(defn resolve-clj-var [env sym]
  (if (is-cljs-file? env)
    (throw (ex-info "Cannot resolve a Clojure expression from a cljs namespace. Use a .cljc file." {:file (:file (:meta (:ns env)))}))  ; TODO is this constraint still up to date?
    (let [clj-env  (clj-env env)
          resolved (if (simple-symbol? sym)
                     (do (maybe-load-clj-ns! env (:ns clj-env))
                         (get-in (resolve-ns (:ns clj-env)) [:mappings sym]))  ; resolve in current clj ns
                     (let [full-ns-sym (resolve-alias env sym)  ; expand alias
                           ns          (symbol (namespace full-ns-sym))
                           nom         (symbol (name full-ns-sym))]
                       (maybe-load-clj-ns! env ns)
                       (when-some [clj-ns (find-ns ns)] ; resolve in target ns
                         (find-interned-var clj-ns nom))))]
      (if (some? resolved)
        (cond (var? resolved)   (CljVar. resolved false)
              (class? resolved) (CljClass. resolved)
              :else             (throw (ex-info "Symbol resolved to an unknow type" {:symbol sym
                                                                                     :type   (type resolved)
                                                                                     :value  resolved})))
        ;; java.lang is implicit so not listed in ns form or env
        (when-some [resolved (clojure.lang.Compiler/maybeResolveIn (the-ns (:ns clj-env)) sym)]
          (CljClass. resolved))))))

(defn resolve-var
  "Given an environment and a symbol, resolve the var."
  [env sym]
  (case (peer-language env)
    ;; Critical to understand: e/defs and macros are already resolved by the cljs compiler.
    ;; If we are on the server and there is a suitable cljs var definition available, we use it.
    ;; otherwise we fallback to loading the clojure namespace and resolves into it.
    ;; This is what saves us from :require-macros everywhere.
    :clj  (let [cljs-resolved-var (resolve-cljs env sym)]
            (if (and (some? cljs-resolved-var)
                  (instance? CljVar cljs-resolved-var)  ; if the resolved var is actually a clojure var and is not a twin, it means it’s a regular macro or an e/def.
                  (not (is-twin cljs-resolved-var)))
              cljs-resolved-var
              (resolve-clj-var env sym)))
    :cljs (resolve-cljs env sym)))

(defn resolve-runtime
  "Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if:
  - it cannot be resolved,
  - it doesn't exist at runtime (is a macro),
  - sym is a special form."
  [env sym]
  (letfn [(runtime-symbol [var] (when (some? var)
                                  (when-not (or (is-macro var) (is-node var))
                                    (with-meta (var-name var) (meta sym)))))]
    (case (peer-language env)
      :clj  (runtime-symbol (resolve-var env sym))
      :cljs (if-let [v (resolve-var env sym)]
              (cond (instance? CljsVar v)  (runtime-symbol v)
                    (instance? CljClass v) (runtime-symbol v)
                    ;; GG: if sym resolves to a clojure var, look up for the cljs-specific version.
                    ;;     Why: some vars exist in two versions (e.g. cljs.core/inc)
                    ;;          - the cljs version is a function (available at runtime),
                    ;;          - the clj version is a macro expending to optimized code (compile-time only, AKA a twin var).
                    ;;            The twin doesn’t exists at runtime.
                    (instance? CljVar v)   (runtime-symbol (resolve-cljs env sym false)))
              ;; GG: corner case: there is no var for cljs.core/unquote-splicing.
              (when (= 'cljs.core/unquote-splicing (:name (cljs/resolve-var env sym)))
                'cljs.core/unquote-splicing)))))

(defn normalize-env "Given a cljs or clj compiler env, normalizes it into an electric compiler env"
  [env]
  (let [peers-config (or (::c/peers-config env) (if (:js-globals env) {::c/local :cljs, ::c/remote :cljs} {::c/local :clj, ::c/remote :clj}))]
    (if (:js-globals env)
      (assoc env ::c/peers-config peers-config)
      {:ns            (ns-name *ns*)
       :locals        (dissoc env ::c/peers-config)
       ::c/peers-config peers-config})))

(tests
  "Var resolution"
  ;; resolve on the default peer (local) with default compiler env (clj)
  (-> (normalize-env {})
    (resolve-var 'inc)
    get-var)
  :=  #'clojure.core/inc

  (-> (normalize-env {})
    (resolve-var 'java.lang.Integer)
    get-var)
  := java.lang.Integer

  (require 'cljs.core)
  ;; resolve on the local (cljs) peer
  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (-> (cljs.analyzer/empty-env)
      (assoc ::c/peers-config {::c/local :cljs ::c/remote :clj})
      (normalize-env)
      (assoc ::c/local true)            ; set current peer to local
      (resolve-var 'inc)
      get-var))
  := (resolve 'cljs.core/inc)


  ;; resolve on the remote (clj) peer
  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (-> (cljs.analyzer/empty-env)
      (assoc-in [:ns :name] (ns-name *ns*))  ; default cljs ns is cljs.user, which is not a thing.
      (assoc ::c/peers-config {::c/local :cljs ::c/remote :clj})
      (normalize-env)
      (assoc ::c/local false)               ; set current peer to remote
      (resolve-var 'inc)
      get-var))
  := #'clojure.core/inc
  )
