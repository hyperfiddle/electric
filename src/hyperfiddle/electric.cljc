(ns hyperfiddle.electric
  (:refer-clojure :exclude [eval def defn fn for empty? partial apply])
  (:require [clojure.core :as cc]
            #?(:clj [clojure.tools.logging :as log])
            contrib.data
            [contrib.cljs-target :refer [do-browser]]
            [contrib.missionary-contrib :as mx]
            [contrib.assert :as ca]
            [hyperfiddle.electric.impl.compiler :as c]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.for :refer [map-by]]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])     ; todo remove
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.electric-client])
            [hyperfiddle.electric.impl.io :as io]
            [hyperfiddle.electric.debug :as dbg]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.electric :refer [def defn fn boot for for-by local run debounce wrap on-unmount]]))
  (:import #?(:clj (clojure.lang IDeref))
           (hyperfiddle.electric Pending Failure FailureInfo)
           (missionary Cancelled)))

;; Equality semantics for Failure and Pending
;; For JVM, defined in java class.
#?(:cljs
   (extend-type Pending
     IEquiv
     (-equiv [this other]
       (instance? Pending other))))

#?(:cljs
   (extend-type Failure
     IEquiv
     (-equiv [this other]
       (and (instance? Failure other)
            (= (.-error this) (.-error other))))))

#?(:cljs (set! (.. FailureInfo -prototype -__proto__) cljs.core/ExceptionInfo.prototype))
#?(:cljs
   (extend-type FailureInfo
     IEquiv
     (-equiv [this other]
       (and (instance? FailureInfo other)
            (= (.-cause this) (.-cause other))))))

#?(:clj
   (do
     ;; Optionally, tell RCF not to rewrite Electric programs.
     (defmethod ana/macroexpand-hook `hyperfiddle.electric/run [the-var form env args] (reduced `(hyperfiddle.electric/run ~@args))) ; optional
     ;;(defmethod ana/macroexpand-hook `hyperfiddle.electric/run2 [_the-var _form _env args] `(hyperfiddle.electric/run2 ~@args))

     ;; Don't expand cc/binding (prevent infinite loop). Explicit implicit do
     (defmethod ana/macroexpand-hook 'clojure.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))
     (defmethod ana/macroexpand-hook 'cljs.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))))

(def eval "Takes a resolve map and a program, returns a booting function.
  The booting function takes
  * as first argument a function Any->Task[Unit] returned task writes the value on the wire.
  * as second argument a flow producing the values read on the wire.
  and returning a task that runs the local reactor."
  r/eval)

(def hook r/hook)
(def bind r/bind) ; for when you want to spawn a e/fn without a new
(def with r/with)

(cc/defn pair [c s]
  (m/sp
    (let [s->c (m/dfv)
          c->s (m/dfv)]
      (m/?
        (m/join {}
          (s (cc/fn [x] (m/sp ((m/? s->c) x)))
            (cc/fn [!] (c->s !) #()))
          (c (cc/fn [x] (m/sp ((m/? c->s) x)))
            (cc/fn [!] (s->c !) #())
            #(throw %)))))))

(defmacro local
  "Single peer loopback system without whitelist. Returns boot task."
  {:style/indent 0}
  [& body]
                                        ; use compiler (client) because no need for exports
  (let [[client server] (c/analyze &env `(do ~@body))]
    `(pair ~(r/emit (gensym) client) ~(r/emit (gensym) server))))

(defmacro run "test entrypoint without whitelist."
  {:style/indent 0}
  [& body]
  `((local ~@body) (cc/fn [_#]) (cc/fn [_#])))

(cc/defn failure? [x] (instance? Failure x))

#?(:clj
   (cc/defn -offload-task [thunk executor]
     (->> (m/ap (m/? (m/via executor (thunk)))) ; run once
       (m/reductions {} (Failure. (Pending.)))
       (m/relieve {}))))

(defmacro offload-task ; speculative
  ([f! executor] `(new (-offload-task ~f! ~executor))) ; rebuild flow and cancel old thread
  ; no varadic arity, user should explicitly state unit of work, so no ambiguity about concurrent tasks
  ([f!] `(new (-offload-task ~f! m/blk))))

#?(:clj (cc/defn -offload [tsk executor]
          (m/reductions {} r/pending
            (m/ap (try (m/? (m/via-call executor (m/?< (mx/poll-task tsk))))
                       (catch Cancelled _ (m/amb)))))))

(defmacro offload
  "run a blocking function (i.e. query) on threadpool specified by `executor` (i.e. m/blk or m/cpu).
IO-bound fns should use m/blk, which is the default. Compute-bound fns should pass m/cpu. Custom
executors are allowed (i.e. to control max concurrency, timeouts etc). Currently JVM only."
  ([f! executor]
   `(let [mbx# (m/mbx)]
      (mbx# ~f!)
      (new (-offload mbx# ~executor))))
  ([f!] `(offload ~f! m/blk)))

(defmacro ^:deprecated wrap "Deprecated. Use `offload` instead." [& body] `(offload #(do ~@body)))

; Should these be in missionary?
;(def chan-read! contrib.missionary-contrib/chan-read!)
;(def chan->ap contrib.missionary-contrib/chan->ap)
;(def chan->task contrib.missionary-contrib/chan->task)
;(def chan->cp contrib.missionary-contrib/chan->cp)

(cc/defn task->cp ; leo to review
  ([!x] (task->cp !x (Failure. (Pending.)))) ; note Electric dependency
  ([!x pending] (->> (m/ap (m/? !x)) (m/reductions {} pending))))

; Moved to contrib.missionary-contrib
;(defmacro use-channel ;; TODO rename
;  ([chan] `(use-channel nil ~chan))
;  ([init chan] `(new (m/reductions {} ~init (chan->ap ~chan)))))

#?(:cljs
   (deftype Clock [^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn                                                    ; cancel
     (-invoke [_]
       (if (zero? raf)
         (set! callback nil)
         (do (.cancelAnimationFrame js/window raf)
             (terminator))))
     IDeref                                                 ; sample
     (-deref [_]
       ; lazy clock, only resets once sampled
       (if (nil? callback)
         (terminator)
         (set! raf (.requestAnimationFrame js/window callback))) ; RAF not called until first sampling
       ::tick)))

; cc def, must be above defmacro def
(def ^:no-doc <clock "lazy & efficient logical clock that schedules no work unless sampled"
  #?(:cljs (cc/fn [n t]
             (let [cancel (->Clock 0 nil t)]
               (set! (.-callback cancel)
                 (cc/fn [_] (set! (.-raf cancel) 0) (n)))
               (n) cancel))
     
     ; 120 hz server, careful this impacts bandwidth in demo-two-clocks
     ; typical UI animation rate is 60 or 120hz, no point in going higher 
     :clj (m/ap (loop [] (m/amb nil (do (m/? (m/sleep (/ 1000 120))) (recur)))))
     #_(m/ap (m/? (m/sleep 1 (m/?> (m/seed (repeat nil))))))))

;; --------------------------------------

(defmacro def
  ([symbol] `(hyperfiddle.electric/def ~symbol ::c/unbound))
  ([symbol docstring init]
   (assert (string? docstring))
   (#'def &form &env (vary-meta symbol assoc :doc docstring) init))
  ([symbol init]
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta symbol assoc ::c/node (if (:js-globals &env) init `(quote ~init))))))

(cc/defn -get-system-time-ms [& [_]] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

; DOM event utilities promoted due to visibility-state being critical

#?(:cljs (cc/defn dom-listener [node typ f opts] 
           (.addEventListener node typ f (clj->js opts))
           #(.removeEventListener node typ f)))

#?(:cljs (cc/defn listen> ; we intend to replace this in UI5 workstream
           ([node event-type] (listen> node event-type identity {}))
           ([node event-type keep-fn!] (listen> node event-type keep-fn! {}))
           ([node event-type keep-fn! opts]
            (m/relieve {}
              (m/observe (cc/fn [!]
                           (dom-listener node event-type #(when-some [v (keep-fn! %)]
                                                            (! v)) opts)))))))

#?(:cljs (def <dom-visibility-state 
           (do-browser 
             (->> (listen> js/document "visibilitychange")
               (m/reductions {} (.-visibilityState js/document))
               (m/latest (cc/fn [_] (.-visibilityState js/document)))))))

(hyperfiddle.electric/def dom-visibility-state (client (new (identity <dom-visibility-state)))) ; starts Pending on server

(hyperfiddle.electric/def dom-mousemove "mousemove events, Pending if unknown"
  (client (new (m/reductions {} r/pending (e/listen> js/document "mousemove")))))

(hyperfiddle.electric/def system-time-ms "ms since 1970 Jan 1" 
  (if (= "visible" dom-visibility-state)
    (new (m/sample -get-system-time-ms <clock))
    (throw (Pending.)))) ; tab is hidden, no clock. (This guards NPEs in userland)

(hyperfiddle.electric/def system-time-secs "seconds since 1970 Jan 1" (/ system-time-ms 1000.0))

(cc/defn -check-recur-arity [provided actual fname]
  (when (not= provided actual)
    (throw (ex-info (str "You `recur`d in " (or fname "<unnamed-efn>") " with " provided
                      " argument" (when-not (= 1 provided) "s") " but it has " actual
                      " positional argument" (when-not (= 1 actual) "s"))
             {}))))

#?(:clj (cc/defn- varargs? [args] (boolean (and (seq args) (= '& (-> args pop peek))))))

#?(:clj (cc/defn- ?bind-self [code ?name] (cond->> code ?name (list 'let [?name `c/%closure]))))

#?(:clj (cc/defn- -build-fn-arity [?name args body]
          [(count args)
           `(binding [c/rec (::c/closure
                             (case (-check-recur-arity c/%arity ~(count args) '~?name)
                               (let [~@(interleave args c/arg-sym)] ~@body)))]
              (new c/rec ~@(take (count args) c/arg-sym)))]))

#?(:clj (cc/defn- -build-vararg-arities [?name args body]
          (let [npos (-> args count (- 2)), unvarargd (-> args pop pop (conj (peek args)))]
            (into [] (map (cc/fn [n]
                            [n `(binding [c/rec (::c/closure (case (-check-recur-arity c/%arity ~(inc npos) '~?name)
                                                               (let [~@(interleave unvarargd c/arg-sym)] ~@body)))]
                                  (new c/rec ~@(take npos c/arg-sym)
                                    ~(let [rst (into [] (comp (drop npos) (take (- n npos))) c/arg-sym)]
                                       (when (seq rst) ; varargs value is `nil` when no args provided
                                         (if (map? (peek args))
                                           (if (even? (count rst))
                                             (list* `hash-map rst) ; (MapVararg. :x 1)
                                             `(merge (hash-map ~@(pop rst)) ~(peek rst))) ; (MapVararg. :x 1 {:y 2})
                                           (list* `vector rst))))))]))
              (range npos 21)))))

#?(:clj (cc/defn ->narity-vec [arities] (into (sorted-set) (comp (map (cc/partial remove #{'&})) (map count)) arities)))

(cc/defn -throw-arity [?name nargs arities]
  (throw (ex-info (str "You called " (or ?name "<unnamed-efn>") " with " nargs
                    " argument" (when (not= nargs 1) "s") " but it only supports " arities)
           {})))

#?(:clj (cc/defn- throw-arity-conflict! [?name group]
          (throw (ex-info (str "Conflicting arity definitions" (when ?name (str " in " ?name)) ": "
                            (str/join " and " group))
                   {:name ?name}))))

#?(:clj (cc/defn- check-only-one-vararg! [?name varargs]
          (when (> (count varargs) 1)
            (throw-arity-conflict! ?name varargs))))

#?(:clj (cc/defn- check-arity-conflicts! [?name positionals vararg]
          (let [grouped (group-by count positionals)]
            (doseq [[_ group] grouped]
              (when (> (count group) 1)
                (throw-arity-conflict! ?name group)))
            (when-some [same (get grouped (-> vararg count dec))]
              (throw-arity-conflict! ?name (conj same vararg))))))

(defmacro fn [& args]
  (let [[?name args2] (if (symbol? (first args)) [(first args) (rest args)] [nil args])
        arities (cond-> args2 (vector? (first args2)) list)
        {positionals false, varargs true} (group-by (comp varargs? first) arities)
        _ (check-only-one-vararg! ?name (mapv first varargs))
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))
        positional-branches (into [] (map (cc/fn [[args & body]] (-build-fn-arity ?name args body))) positionals)
        vararg-branches (when (seq varargs) (-build-vararg-arities ?name (ffirst varargs) (nfirst varargs)))]
    (if (bound? #'c/*env*)
      (list ::c/closure
        (-> `(case c/%arity
               ~@(into [] (comp cat cat) [positional-branches vararg-branches])
               (-throw-arity '~?name c/%arity ~(->> arities (eduction (map first)) ->narity-vec (str/join ", "))))
          (?bind-self ?name))
        {::dbg/name ?name, ::dbg/type (or (::dbg/type (meta ?name)) :reactive-fn)
         ::dbg/meta (merge (select-keys (meta &form) [:file :line])
                      (select-keys (meta ?name) [:file :line]))})
      `(throw (ex-info "Invalid e/fn in Clojure code block (use from Electric code only)" ~(into {} (meta &form)))))))

(defmacro fn*
  "Low-level construct. Use `hyperfiddle.electric/fn` instead.
   Bare-bone reactive anonymous function. Single arity, no arity check, no variadic args support, no self-recur."
  ;; G: `e/fn*` produces a smaller program than `e/fn`. Experts can use `e/fn*` in internals and libraries.
  ;;    Users should default to `e/fn`.
  [args & body]
  (let [debug-info {::dbg/type :reactive-fn
                    ::dbg/meta (select-keys (meta &form) [:file :line])}]
    (if (seq args)
      `(::c/closure (let [~@(interleave args c/arg-sym)] ~@body) ~debug-info)
      `(::c/closure (do ~@body) ~debug-info))))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & fdecl]
  (let [[_defn sym' & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: docstring support
    `(hyperfiddle.electric/def ~sym' (hyperfiddle.electric/fn ~(vary-meta sym' merge {::dbg/type :reactive-defn}
                                                                          (meta &form)
                                                                          (meta sym'))
                                                              ~@(if (string? (first fdecl)) ; GG: skip docstring
                                         (rest fdecl)
                                         fdecl)))))

(defmacro ^:no-doc defn* [sym & fdecl]
  (let [[_defn sym' & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: docstring support
    `(hyperfiddle.electric/def ~sym' (hyperfiddle.electric/fn*
                                       ~@(if (string? (first fdecl)) ; GG: skip docstring
                                           (rest fdecl)
                                           fdecl)))))

(defmacro for-by
  {:style/indent 2}
  [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(let [xs# ~v]
       (new (r/bind map-by ~kf
              ~(->> body
                 (list* `for-by kf bindings)
                 (list `let [s (first c/arg-sym)])
                 (list `hyperfiddle.electric/fn* [])
                 (list `cc/partial (list ::c/inject (first c/arg-sym))))
              (::c/lift xs#))))
    (cons `do body))) ; todo, buggy: (e/for [x []] (println 42)) should not print

(defmacro for
  {:style/indent 1}
  [bindings & body]
  `(hyperfiddle.electric/for-by identity ~bindings ~@body))

(cc/defn ^:no-doc watchable? [x]
  #?(:clj (instance? clojure.lang.IRef x)
     :cljs (satisfies? IWatchable x)))

(cc/defn ^:no-doc checked-watch [!x]
  (->> !x (ca/check watchable?) m/watch))

(def -invalid-watch-usage-message "Invalid e/watch (use from Electric code only, maybe you forgot a e/def?)")

(defmacro watch "Derive a reactive value from a Clojure atom or reference."
  [!x]
  (if (bound? #'c/*env*)
    `(new (checked-watch ~!x))
    `(throw (ex-info -invalid-watch-usage-message ~(into {} (meta &form))))))

(cc/defn debounce-discreet
  ([delay flow] (debounce-discreet delay nil flow))
  ([delay init flow] (m/reductions {} init (m/ap (let [x (m/?< flow)]
                                                   (try (m/? (m/sleep delay x))
                                                        (catch Cancelled _ (m/amb))))))) )

(defmacro ^:deprecated debounce ; immoral? introduces avoidable delays
  "Debounce a continous flow by `delay` milliseconds."
  [delay flow]
  `(new (->> (fn [] ~flow)
             (debounce-discreet ~delay)
             (m/relieve {}))))

(cc/defn throttle [dur >in] ; in CLJ, wrong number of args (1) passed to: hyperfiddle.electric-ui4/long --- ?????
  (m/ap
    (let [x (m/?> (m/relieve {} >in))]
      (m/amb x (do (m/? (m/sleep dur)) (m/amb))))))

(defmacro ^:deprecated remote [& body]
  (if (= 1 (count body))
    `(unquote-splicing ~@body)
    `(unquote-splicing (do ~@body))))

(defmacro client
  {:style/indent 0}
  [& body]
  (if (bound? #'c/*env*)
    `(::c/client (do ~@body) ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::client))
    `(throw (ex-info "Invalid e/client in Clojure code block (use from Electric code only)" ~(into {} (meta &form))))))

(defmacro server
  {:style/indent 0}
  [& body]
  (if (bound? #'c/*env*)
    `(::c/server (do ~@body) ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::server))
    `(throw (ex-info "Invalid e/server in Clojure code block (use from Electric code only)" ~(into {} (meta &form))))))

(defmacro flow
  "Transform an Electric value into a Missionary flow by \"quoting\" it with e/fn.
Quoting it directly is idiomatic as well."
  {:style/indent 0}
  [x] `(hyperfiddle.electric/fn [] ~x))

(defmacro discard
  "Silence \"Unserializable reference transfer\"; inlining `(do ... nil)` is idiomatic as well"
  {:style/indent 0}
  [& body] `(do ~@body nil))

(hyperfiddle.electric/def trace "In a `catch` block, bound by the runtime to the current stacktrace. An Electric stacktrace is an ExceptionInfo. Use `hyperfiddle.electric.debug/stack-trace` to get a string representation." nil)

(defmacro lexical-env "Return a map containing lexical bindings" []
  (let [env (if (:js-globals &env) (:locals &env) &env)]
    (with-meta
      `{:env ~(reduce-kv (cc/fn [r k v] (assoc r (list 'quote k) k)) (empty env) env)}
      (meta &form))))

(defmacro with-cycle
  "evaluates body with symbol s bound to the previous result of the body evaluation.
  the first evaluation binds s to i."
  {:style/indent 1}
  [[s i] & body]
  `(let [a# (atom ~i) ~s (hyperfiddle.electric/watch a#)]
     (reset! a# (do ~@body))))

(defmacro partial-dynamic
  "Return a function calling given function `f` with given dynamic environment."
  [bindings f]
  `(cc/fn [& args#] (binding ~bindings (cc/apply ~f args#))))

(defmacro partial
  "Like `cc/partial` for reactive functions. Requires the target function
  arity (`argc`) until reactive function supports variadic arguments.

  e.g. (new (partial 2 (e/fn [a b] [a b]) :a) :b) ;; => [:a :b]"
  [argc F & args]
  (if (= 0 argc)
    F
    (let [rest-args (map #(symbol (str "arg_" %)) (range (- argc (count args))))]
      `(let [F# ~F]
         (hyperfiddle.electric/fn ~@(when (symbol? F) [F]) [~@rest-args]
                                  (new F# ~@args ~@rest-args))))))

(cc/defn on-unmount* [f] (m/observe (cc/fn [!] (! nil) f)))

(defmacro on-unmount "Run clojure(script) thunk `f` during unmount.

  Standard electric code runs on mount, therefore there is no `on-mount`."
  [f] `(new (on-unmount* ~f))) ; experimental

(cc/defn log-root-error [exception]
  #?(:clj (log/error exception)
     :cljs (println exception)))

(defn ?PrintServerException [id]
  (try (server
         (when-some [ex (io/get-original-ex id)]
           (log-root-error ex)
           (try (client (js/console.log "server logged the root exception"))
                (catch Pending _))))
       (catch Pending _)))

(defmacro with-zero-config-entrypoint
  {:style/indent 0}
  [& body]
  `(try
     (do ~@body)
     (catch Pending _#)                 ; silently ignore
     (catch Cancelled e# (throw e#))    ; bypass catchall, app is shutting down
     (catch :default err#               ; note client bias
       (let [reactive-stacktrace (str (ex-message err#) "\n\n" (dbg/stack-trace hyperfiddle.electric/trace))]
         (if-some [client-exception (io/get-original-ex (dbg/ex-id hyperfiddle.electric/trace))]
           (js/console.error reactive-stacktrace "\n\n" client-exception)
           (js/console.error reactive-stacktrace)))
       (new ?PrintServerException (dbg/ex-id hyperfiddle.electric/trace)))))

(defmacro boot-with-options
  "Like `boot`, but takes a map of options as first parameter.

   Supported options:
   - `:query-params`: A map of string to string, function, or promise. Functions should resolve
                      to strings or promises. Promises should resolve to strings. Each map entry
                      is added as a query parameter to the connection URL."
  {:style/indent 0}
  [{:keys [query-params]} & body]
  (assert (:js-globals &env))
  (let [[client server] (c/analyze
                          (assoc &env ::c/peers-config {::c/local :cljs ::c/remote :clj})
                          `(with-zero-config-entrypoint ~@body))]
    `(hyperfiddle.electric-client/reload-when-stale
       (hyperfiddle.electric-client/boot-with-retry
         ~(r/emit (gensym) client)
         (hyperfiddle.electric-client/connector ~query-params (quote ~server))))))

(defmacro boot "
Takes an Electric program and returns a task setting up the full system with client part running locally and server part
running on a remote host.
"
  {:style/indent 0}
  [& body]
  `(boot-with-options {} ~@body))

;; WIP: user space socket reconnection

#_
(hyperfiddle.electric/def ^{:doc "
`true` if the main process was cancelled, `false` otherwise.
"} cancelled (new r/cancelled))

#_
(hyperfiddle.electric/def ^{:doc "
`true` if the link to the remote peer is up, `false` otherwise. May throw Pending during connection.
"} connected (new r/connected))

#_
(hyperfiddle.electric/defn Entrypoint [App]
  (or cancelled
    (not= :waiting
      (with-cycle [s {:status :connecting :delay 1000}]
        (try (App.) (catch :default e (.error js/console e)))
        (case (:status s)
          :waiting (let [{:keys [since delay]} s
                         remaining (-> since (+ delay) (- time))]
                     (if (pos? remaining)
                       (do (println (str "Retrying in " (int (/ remaining 1000)) "s.")) s)
                       (do (println "Connecting...")
                           (-> s
                             (dissoc :since)
                             (assoc :status :connecting)))))
          :connecting (try (if connected
                             (do (println "Connected.")
                                 (-> s
                                   (dissoc :delay)
                                   (assoc :status :connected)))
                             (do (println "Failed to reconnect.")
                                 (-> s
                                   (update :delay * 2)
                                   (assoc :status :waiting
                                          :since time))))
                           (catch Pending _ s))
          :connected (if connected
                       s (do (println "Connection reset.")
                             {:status :waiting
                              :since  time
                              :delay  1000})))))))

(def ^:dynamic *http-request* "Bound to the HTTP request of the page in which the current Electric program is running." nil)

(defmacro check-electric [fn form]
  (if (bound? #'c/*env*)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(cc/defn -snapshot [flow] (->> flow (m/eduction (contrib.data/take-upto (complement #{r/pending})))))

(defmacro snapshot 
  "Snapshots the first non-Pending value of reactive value `x` and freezes it, 
inhibiting all further reactive updates." 
  [x] `(check-electric snapshot (new (-snapshot (hyperfiddle.electric/fn* [] ~x)))))

(defmacro apply [F & args]
  `(let [F# ~F, as# [~@args], args# (object-array (concat (pop as#) (peek as#)))]
     (case (alength args#)
       0 (new F#)
       1 (new F# (aget args# 0))
       2 (new F# (aget args# 0) (aget args# 1))
       3 (new F# (aget args# 0) (aget args# 1) (aget args# 2))
       4 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3))
       5 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4))
       6 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5))
       7 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6))
       8 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7))
       9 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8))
       10 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9))
       11 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10))
       12 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11))
       13 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12))
       14 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13))
       15 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14))
       16 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14) (aget args# 15))
       17 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14) (aget args# 15) (aget args# 16))
       18 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14) (aget args# 15) (aget args# 16) (aget args# 17))
       19 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14) (aget args# 15) (aget args# 16) (aget args# 17) (aget args# 18))
       20 (new F# (aget args# 0) (aget args# 1) (aget args# 2) (aget args# 3) (aget args# 4) (aget args# 5) (aget args# 6) (aget args# 7) (aget args# 8) (aget args# 9) (aget args# 10) (aget args# 11) (aget args# 12) (aget args# 13) (aget args# 14) (aget args# 15) (aget args# 16) (aget args# 17) (aget args# 18) (aget args# 19)))))

(cc/defn ->Object [] #?(:clj (Object.) :cljs (js/Object.))) ; private

;; low-level, most powerful, hardest to use
(defmacro for-event
  ; Progress in UI is a continuous flow (succession of values) that eventually completes with a final result.
  "For each value of >flow, spawn concurrent `body` branches which individually 
progress towards completion. Keeps each branch alive to progress in isolation 
until it signals completion by returning a reduced value, at which point the branch
is unmounted. Returns active progress values as a vector. Exceptions bubble out."
  {:style/indent 1}
  [[sym >flow] & body]
  `(let [mbx# (m/mbx)]
     (new (m/reductions {} nil (m/eduction (map #(mbx# [::mx/add (->Object) %])) ~>flow)))
     (for-by first [[progressing-event# ~sym] (new (mx/document (mx/poll-task mbx#)))]
       (let [v# (do ~@body)]
         (if (reduced? v#)
           (do (mbx# [::mx/retract progressing-event#]) nil)
           v#)))))

(def pending (Pending.))

;; high-level wrapper of above, returns a union type, waits for non-Pending value
(defmacro for-event-pending
  "Like for-event, but each branch completes as soon as a non-Pending value or 
exception is available. Returns [::pending e/pending] if there are one or more
uncompleted branches, otherwise returns a 4-colored result corresponding to the
progress of the most recently completed branch."
  {:style/indent 1}
  [bind & body]
  `(let [!state# (atom [::init]), state# (hyperfiddle.electric/watch !state#)]
     (if (seq (for-event ~bind
                (try (reduced (reset! !state# [::ok (do ~@body)]))
                     (catch Pending ex#)
                     (catch Cancelled ex# (reduced nil))
                     (catch ~(if (:ns &env) :default `Throwable) ex# 
                       (reduced (reset! !state# [::failed ex#]))))))
       [::pending pending] ; save Pending exception for easy re-throw
       state#)))

;; e/for-event-pending-switch is similar to m/?<. 
;; When a new event comes in a new branch is immediately spawned and the previous cancelled
;; ok, so these operators are basically the equivalent of m/?< m/?> and m/?= extended into continuous time. 
;; one runs events concurrently, one runs events sequentially, and one interrupts
;; for-event-sequence, for-event-concurrent, for-event-interrupt
(defmacro for-event-pending-switch ; for-event?<
  "Like for-event, but each new event supersedes the prior event, canceling and 
discarding previous it even if in progress. The single active branch completes 
on the first non-Pending value. Returns a single four-colored result 
corresponding to the progress of the most recent event."
  {:style/indent 1}
  [[sym >flow :as bind] & body]
  `(let [!i# (atom 0), i# (hyperfiddle.electric/watch !i#)
         !state# (atom [::init]), state# (hyperfiddle.electric/watch !state#)]
     (if (seq (for-event ~bind
                (try (when (<= i# (inc @!i#)) 
                       (reset! !state# [::ok (do ~@body)])) 
                  (reduced (swap! !i# inc))
                  (catch Pending ex#)
                  (catch Cancelled ex# (reduced nil))
                  (catch ~(if (:ns &env) :default `Throwable) 
                    ex# (reduced (reset! !state# [::failed ex#]))))))
       [::pending pending] state#)))

(defmacro do-event ; for-one-event
  "Run `body` continuation in response to next event (silently discarding subsequent 
events) until it completes by evaluating to a `reduced` value. On completion, 
unmounts the body, returning nil while waiting for a fresh event."
  ; Useful for button case because it discards. Not useful for create-new, because it discards.
  ; Does this blink-and-clear on completion?
  {:style/indent 1}
  [[sym >flow] & body]
  `(let [!e# (atom nil)]
     (new (m/reductions #(swap! !e# (cc/fn [cur#] ; latch first non-nil event
                                      (if (nil? cur#) %2 cur#))) ; discarding events until event processing completes
            nil ~>flow))
     (when-some [~sym (hyperfiddle.electric/watch !e#)] ; wait for non-nil event (rising edge), bind in scope for body
       (let [v# (do ~@body)] ; nil is insignificant here
         (if (reduced? v#) ; never seen
           (reset! !e# nil) ; reset, unmount body, wait for fresh event
           v#)))))

(defmacro do-event-pending
  "Run `body` continuation in response to next event (silently discarding subsequent 
events) until it completes by evaluating to a non-Pending result. On completion, 
unmounts the body, latches and returns the 4-colored result while waiting for a 
fresh event."
  {:style/indent 1}
  [[sym >flow :as bind] & body]
  `(let [!state# (atom [::init])]
     (do-event ~bind
       ; D: Why not leave the body alive, is the signal state the same as this explicit latch?
       (try (reduced (reset! !state# [::ok (do ~@body)])) ; latch result
            (catch Pending ex# (reset! !state# [::pending pending])) ; wait longer
            (catch Cancelled ex# (reduced nil))
            (catch ~(if (:ns &env) :default `Throwable) ex#
              (reduced (reset! !state# [::failed ex#]))))) ; latch result
     (hyperfiddle.electric/watch !state#)))

(cc/defn -inhibit [>x] (m/reductions nil nil (m/ap (m/?< >x) (m/amb))))

(defmacro inhibit "Run body for effects, ignore result. Returns an invariable nil."
  {:style/indent 1}
  [& body] `(new (-inhibit (hyperfiddle.electric/fn [] ~@body))))
