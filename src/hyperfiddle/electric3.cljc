(ns hyperfiddle.electric3
  (:refer-clojure :exclude [fn defn apply letfn for declare #?(:cljs Range)])
  (:require [hyperfiddle.electric.impl.missionary-util :as mu]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.runtime3 :as r]
            #?(:clj [hyperfiddle.electric.impl.entrypoint]) ; TODO rename server-entrypoint
            #?@(:cljs [hyperfiddle.electric-client3], :node nil) ; auto-load electric-client in browser builds. Before users had to remember to require it for side effect at the app entrypoint.
            [hyperfiddle.electric-dom3-events :as dom-events]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.electric.impl.mount-point :as mp]
            [clojure.core :as cc]
            [clojure.string :as str]
            [contrib.data]
            [contrib.measure :as cm]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [fipp.edn])
            [missionary.core :as m]
            [contrib.missionary-contrib :as mx]
            [clojure.math :as math]
            [contrib.debug :as dbg]
            [contrib.assert :as ca])
  (:import [missionary Cancelled])
  #?(:cljs (:require-macros hyperfiddle.electric3)))

(def web-config {::lang/peers {:client :cljs, :server :clj}})

#?(:clj (cc/defn dget [v] `(::lang/lookup ~v)))
#?(:clj (cc/defn ->pos-args [n] (eduction (take n) (map dget) (range))))

(defmacro check-electric [fn form]
  (if (::lang/electric &env)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro call [F & args] `(check-electric $ (lang/$ ~F ~@args)))
(defmacro $ [F & args] `(call ~F ~@args)) ; compat

(defmacro frame [] `(::lang/frame))

(defmacro pure "
Syntax :
```clojure
(pure table)
```
Returns the incremental sequence describing `table`.
" [expr] `((::lang/static-vars r/incseq) (frame) (::lang/pure ~expr)))

(defmacro join "
Syntax :
```clojure
(join incseq)
```
Returns the successive states of items described by `incseq`.
" [flow] `(::lang/join ~flow))

#?(:clj (cc/defn- varargs? [args] (boolean (and (seq args) (= '& (-> args pop peek))))))

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

(defmacro -fn [& args]
  (let [[?name args2] (if (symbol? (first args)) [(first args) (rest args)] [nil args])
        arities (cond-> args2 (vector? (first args2)) list)
        {positionals false, varargs true} (group-by (comp varargs? first) arities)
        _ (check-only-one-vararg! ?name (mapv first varargs))
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))]
    (into (if-some [[args & body] (first varargs)]
            (let [fixed (-> args pop pop)]
              {-1 [(count fixed)
                   (map? (peek args))
                   `(::lang/ctor
                      (let [~@(interleave fixed (map dget (range)))
                            ~(peek args) ~(dget (count fixed))] ~@body))]}) {})
      (map (cc/fn [[args & body]]
             [(count args)
              `(::lang/ctor
                (let [~@(interleave args (map dget (range)))]
                  ~@body))])) positionals)))

;; mklocal = declare lexical slot
;; bindlocal = bind lexical slot to value by name
;; See compiler walkthrough: electric/impl/lang_3_walkthrough.md
(defmacro fn [& args]
  (let [?nm (first args)]
    `(check-electric fn
       ~(if (symbol? ?nm) `(::lang/mklocal ~?nm (::lang/bindlocal ~?nm (-fn ~@args) ~?nm)) `(-fn ~@args)))))

(cc/defn ns-qualify [sym] (if (namespace sym) sym (symbol (str *ns*) (str sym))))

#?(:clj (tests
          (ns-qualify 'foo) := `foo
          (ns-qualify 'a/b) := 'a/b))

(defmacro defn [nm & fdecl]
  (let [[_defn sym] (macroexpand `(cc/defn ~nm ~@fdecl))
        env (merge (meta nm) (lang/normalize-env &env) web-config {::lang/def nm})
        nm2 (vary-meta nm merge (meta sym) {::lang/node true})
        source (lang/->source env (-> nm ns-qualify keyword)
                 (with-meta `(-fn ~nm2 ~@(cond-> fdecl (string? (first fdecl)) next))
                   (meta &form)))]
    (when-not (::lang/has-edef? (meta *ns*)) (alter-meta! *ns* assoc ::lang/has-edef? true))
    `(def ~nm2 ~source)))

(defmacro declare [& syms]
  (ca/is syms (partial every? symbol?) (str "declare takes one or more symbols to declare, got " (pr-str &form)))
  (let [syms (mapv #(vary-meta % assoc ::lang/node true) syms)]
    `(cc/declare ~@syms)))

(defmacro amb "
Syntax :
```clojure
(amb table1 table2 ,,, tableN)
```
Returns the concatenation of `table1 table2 ,,, tableN`.
"
  ([] `(join r/void))
  ([& exprs] `(join (join (i/fixed ~@(map (cc/fn [expr] `(r/invariant (pure ~expr))) exprs))))))

(defmacro input "
Syntax :
```clojure
(input cf)
```
Returns the current state of current continuous flow `cf`.
" [& flows] `(join (r/fixed-signals ~@flows)))

(cc/defn watchable! [ref]
  (ca/is ref #?(:clj #(instance? clojure.lang.IRef %) :cljs #(satisfies? IWatchable %)) (str "Not watchable: " ref)))

(defmacro watch "
Syntax :
```clojure
(watch !ref)
```
Returns the current state of current reference `!ref`.
" [ref] `(check-electric watch (input (m/watch (watchable! ~ref)))))

(defmacro diff-by "
Syntax :
```clojure
(diff-by kf xs)
```
Stabilizes successive states of collection `xs` with function `kf`. Returns each item as a table.
" [f xs] `(->> (pure ~xs) (m/reductions i/patch-vec) (m/latest (partial eduction cat)) (i/diff-by ~f) (join)))

(defmacro diff "
Syntax :
```clojure
(diff xs)
```
Stabilizes successive states of collection `xs` with identity. Returns each item as a table.
" ;; TODO optimize
  [xs] `(diff-by identity ~xs))

(hyperfiddle.electric3/defn Range "
Syntax :
```clojure
(for [i (Range end)] (F i))
(for [i (Range start end)] (F i))
(for [i (Range start end step)] (F i))
```
Returns numbers between `start` (inclusive, default 0) and `end` (exclusive) by `step` increments (default 1) as a table.
" ;; TODO optimize
  ([end] (diff (range end)))
  ([start end] (diff (range start end)))
  ([start end step] (diff (range start end step))))

(defmacro drain "
Syntax :
```clojure
(drain expr)
```
Samples and discards `expr` synchronously with changes. Returns nothing.
" ([] `(join r/void))
  ([expr] `(join (r/drain (pure ~expr))))
  ([expr & exprs] `(r/do! ~@(clojure.core/for [arg (cons expr exprs)] `(drain ~arg)))))

(defmacro client [& body] `(check-electric client (::lang/site :client ~@body)))
(defmacro server [& body] `(check-electric server (::lang/site :server ~@body)))

(defmacro for "
Syntax :
```clojure
(for [sym1 table1
      sym2 table2
      ,,,  ,,,
      symN tableN]
  & body)
```
For each tuple in the cartesian product of `table1 table2 ,,, tableN`, calls body in an implicit `do` with symbols
`sym1 sym2 ,,, symN` bound to the singleton tables for this tuple. Returns the concatenation of all body results.
" [bindings & body]
  (case bindings
    [] `(do ~@body)
    (let [[args exprs] (cc/apply map vector (partition-all 2 bindings))]
      `(::lang/call
        (r/bind-args (ctor (let [~@(interleave args (->pos-args (count args)))] ~@body))
          ~@(map (clojure.core/fn [expr]
                   `(r/effect (r/fixed-signals (join (i/items (pure ~expr))))))
              exprs))))))

(defmacro cursor [bindings & body] `(for ~bindings ~@body)) ; compat

(defmacro as-vec "
Syntax :
```clojure
(as-vec table)
```
Returns a single vector containing elements of `table`.
" [expr] `(input (m/reductions i/patch-vec (pure ~expr))))

(defmacro for-by "
Syntax :
```clojure
(for-by kf [sym1 coll1
            sym2 coll2
            ,,,  ,,,
            symN collN]
  & body)
```
Stabilizes successives states of `coll1 coll2 ,,, collN` with function `kf`. For each tuple in the cartesian product of
resulting tables, calls body in an implicit `do` with symbols `sym1 sym2 ,,, symN` bound to the singleton tables for
this tuple. Returns the concatenation of all body results as a single vector.
" [kf bindings & body]
  `(as-vec
     ~((clojure.core/fn rec [bindings]
         (if-some [[sym expr & bindings] bindings]
           `(cursor [~sym (diff-by ~kf ~expr)]
              ~(rec bindings)) `(do ~@body)))
       (seq bindings))))

(defmacro with-cycle
  "evaluates body with symbol s bound to the previous result of the body evaluation.
  the first evaluation binds s to i."
  {:style/indent 1}
  [[s i] & body]
  `(let [a# (atom ~i) ~s (watch a#)]
     (reset! a# (do ~@body))))

(defmacro with-cycle* [kf [s s0] & body] ; hacked support for amb cycles, fixme
  `(let [kf# ~kf]
     (diff-by kf#
       (with-cycle [~s (as-vec ~s0)]
         (let [~s (diff-by kf# ~s)]
           (as-vec (do ~@body)))))))

(hyperfiddle.electric3/defn Count [xs] (-> xs pure i/count input))
(hyperfiddle.electric3/defn Filter [p? xs] (for [x xs] (if (p? x) x (amb)))) ; todo write as product
(defmacro When [test & body] `(if ~test (do ~@body) (amb)))
(hyperfiddle.electric3/defn Some? [xs] (pos? (Count xs)))

(hyperfiddle.electric3/defn Tap-diffs
  ([f! x] (f! (input (pure x))) x)
  ([x] (Tap-diffs prn x)))

;; mklocal = declare lexical slot
;; bindlocal = bind lexical slot to value by name
;; See compiler walkthrough: electric/impl/lang_3_walkthrough.md
(defmacro letfn [bs & body]
  (let [sb (reverse bs)]
    (reduce (cc/fn [ac [nm]] `(::lang/mklocal ~nm ~ac))
      (reduce (cc/fn [ac [nm & fargs]] `(::lang/bindlocal ~nm (hyperfiddle.electric3/fn ~@fargs) ~ac)) (cons 'do body) sb)
      sb)))

(defmacro tag "
Returns a new tag instance.
" [] `(::lang/tag))

(defmacro mount-point "
Returns a new mount point instance.
A mount point can be :
* mutated as a key-value store via the KVS protocol. Keys must be tags generated by the same electric application.
* watched as an incremental sequence. Values will be sorted according to the relative ordering of tags.
  " [] `(mp/create (r/frame-peer (frame))))

(hyperfiddle.electric3/defn Dispatch [eF static args]
  (let [offset (count static)
        arity (+ offset (count args))] ; final count of all args
    (if-some [ctor (eF arity)] ; EFns implement IFn and return a constructor given a arg count
      (loop [args args ; if we find the constructor for the current arity, just call it
             static static]
        (if (< (count static) arity)
          (recur (next args) (conj static (::lang/pure (first args))))
          (cc/apply r/bind-args (r/bind-self ctor) static)))
      ;; search for variadic version
      (let [[fixed map? ctor] (r/get-variadic "apply" eF arity)]
        (if (< fixed offset) ; if variadic arity has more positional args than provided: pop from rest args
          (loop [args args
                 static static]
            (let [args (cons (::lang/join (r/incseq (frame) (peek static))) args)
                  static (pop static)]
              (if (< fixed (count static))
                (recur args static)
                (cc/apply r/bind-args (r/bind (r/bind-self ctor) fixed (::lang/pure (cc/apply (r/as-varargs map?) args))) static))))
          (loop [args args ; if variadic arity has less positional args than provided: push to rest args
                 static static]
            (if (< (count static) fixed)
              (recur (next args) (conj static (::lang/pure (first args))))
              (cc/apply r/bind-args (r/bind (r/bind-self ctor) fixed (::lang/pure (cc/apply (r/as-varargs map?) args))) static))))))))

(hyperfiddle.electric3/defn Apply
  ([F a]
   (::lang/call ($ Dispatch F [] a)))
  ([F a b]
   (::lang/call ($ Dispatch F [(::lang/pure a)] b)))
  ([F a b c]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b)] c)))
  ([F a b c d]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b) (::lang/pure c)] d)))
  ([F a b c d & es]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b) (::lang/pure c) (::lang/pure d)] (concat (butlast es) (last es))))))

(defmacro apply [& args] `($ Apply ~@args))

(hyperfiddle.electric3/defn ; ^:hyperfiddle.electric.impl.lang3/print-clj-source
  Partial
  "Takes an Electric function F and fewer than the normal arguments to F, and
  returns a e/fn that takes a variable number of additional args. When
  called, the returned function calls F with args + additional args."
  ;; Impl is a mechanical 1 to 1 transaltion of clojure partial.
  ;; generated code is quite large but redundant, so it gzip to 903 bytes.
  ;; we could prune this impl to reduce code size (no clear benefit)
  ;; We keep this impl as a proof that our lambda abstraction is correct
  ;; We might optimise it later if there are perf issues.
  ([F] F)
  ([F arg1]
   (hyperfiddle.electric3/fn
     ([] ($ F arg1))
     ([x] ($ F arg1 x))
     ([x y] ($ F arg1 x y))
     ([x y z] ($ F arg1 x y z))
     ([x y z & args] (hyperfiddle.electric3/apply F arg1 x y z args))))
  ([F arg1 arg2]
   (hyperfiddle.electric3/fn
     ([] ($ F arg1 arg2))
     ([x] ($ F arg1 arg2 x))
     ([x y] ($ F arg1 arg2 x y))
     ([x y z] ($ F arg1 arg2 x y z))
     ([x y z & args] (hyperfiddle.electric3/apply F arg1 arg2 x y z args))))
  ([F arg1 arg2 arg3]
   (hyperfiddle.electric3/fn
     ([] ($ F arg1 arg2 arg3))
     ([x] ($ F arg1 arg2 arg3 x))
     ([x y] ($ F arg1 arg2 arg3 x y))
     ([x y z] ($ F arg1 arg2 arg3 x y z))
     ([x y z & args] (hyperfiddle.electric3/apply F arg1 arg2 arg3 x y z args))))
  ([F arg1 arg2 arg3 & more]
   (hyperfiddle.electric3/fn [& args]
     (hyperfiddle.electric3/apply F arg1 arg2 arg3 (concat more args)))))

(cc/defn on-unmount* [f] (m/observe (cc/fn [!] (! nil) f)))

(defmacro on-unmount [f] `(input (on-unmount* ~f))) ; experimental

(hyperfiddle.electric3/defn On-unmount ; legacy
  "Run clojure/script thunk `f` during unmount. Standard electric code runs on
mount, therefore there is no `on-mount`. (Todo: we intend to rework this API in
v3 to expose the full differential diff lifecycle)"
  [f] (input (on-unmount* f)))

(defmacro boot-server [opts Main & args]
  `(hyperfiddle.electric.impl.entrypoint/boot-server ~opts ~Main ~@args))

(defn- build-properly-configured? [compiler-state]
  (let [shadow-build-state (:shadow.build.cljs-bridge/state compiler-state)]
    (or (not= :dev (:shadow.build/mode shadow-build-state)) ; only check config in dev mode
      (contains? (:hyperfiddle.electric.shadow-cljs/hooks3 shadow-build-state) 'hyperfiddle.electric.shadow-cljs.hooks3/reload-clj))))

(defmacro boot-client [opts Main & args]
  (if (build-properly-configured? (deref cljs.env/*compiler*))
    (let [env (merge (lang/normalize-env &env) web-config opts )
          source (lang/->source env ::Main `(fn [] ($ ~Main ~@args)))]
      `(cc/apply ~(if (::hot-swap opts) `r/boot-client-hot `r/boot-client-cold) (r/->defs {::Main ~source}) ::Main
         ~(select-keys opts [:cognitect.transit/read-handlers :cognitect.transit/write-handlers])
         (hyperfiddle.electric-client3/connector hyperfiddle.electric-client3/*ws-server-url*)))
    (let [message "Electric compilation error: a required shadow build hook is missing. Please add `hyperfiddle.electric.shadow-cljs.hooks3/reload-clj under :build-hooks in shadow-cljs.edn"]
      (println message)
      `(cc/fn [] (println ~message)))))

(defmacro boot-single [opts Main & args]
  (let [env (merge (lang/normalize-env &env) web-config opts)
        source (lang/->source env ::Main `(fn [] ($ ~Main ~@args)))]
    `(r/peer-sink (r/make-peer :client {} nil (r/->defs {::Main ~source}) ::Main nil))))

;; (cc/defn -snapshot [flow] (->> flow (m/eduction (contrib.data/take-upto (complement #{r/pending})))))
(cc/defn -snapshot [flow] (->> flow (m/eduction (contrib.data/take-upto (comp pos-int? :degree)))))

(defmacro snapshot
  "Snapshots the first non-Pending value of reactive value `x` and freezes it,
inhibiting all further reactive updates."
  [x] `(check-electric snapshot (join (-snapshot (pure ~x)))))

(hyperfiddle.electric3/defn Snapshot [v] (join (-snapshot (pure v))))

(hyperfiddle.electric3/defn Reconcile
  "collapse :grow/:shrink diffs into :change diffs based on position. For example,
(if x a b) on transition will grow one branch and shrink the other in one atomic
diff, but (e/Reconcile (if x a b)) will emit a single :change diff. Use case:
inhibit undesired duplicate effects with code like (if x a a) or (or a1 a2)."
  [xs]
  #_(second (e/diff-by first (map-indexed vector (e/as-vec xs)))) ; same ?
  (diff-by {} (as-vec xs))) ; force all items to collide such that the diff algorithm fallbacks on position

;; ------ Tokens ------
(cc/defn token-call [children & args]
  (doseq [f children]
    (try (when f (cc/apply f args))
         (catch #?(:clj Throwable, :cljs :default) err
           (prn "Token call failed" {:name name, :child f, :args args} err)
           (throw err)))))

(cc/defn token->string [t] (str "#<" `Token (some->> t :name pr-str (str " ")) ">"))

#?(:clj
   (defrecord TokenImpl [name children]
     clojure.lang.IFn
     (invoke [this] (token-call children))
     (invoke [this err] (token-call children err))
     Object
     (toString [this] (token->string this)))
   :cljs
   (defrecord TokenImpl [name children]
     cljs.core/IFn
     (-invoke [this] (token-call children))
     (-invoke [this err] (token-call children err))
     cljs.core/IPrintWithWriter
     (-pr-writer [this writer opts]
       (-write writer (str this)))
     Object
     (toString [this] (token->string this))))

#?(:clj (defmethod print-method TokenImpl [t ^java.io.Writer writer] (.write writer (token->string t))))

(cc/defn ->Token
  ([] (->Token `empty-token))
  ([name & children]
   (let [t (->TokenImpl name children)]
     (with-meta t
       {::children children
        ::name name
        ::hash (hash t)                 ; (hash (with-meta f …)) ≠ (hash f)
        `clojure.core.protocols/datafy meta}))))

(cc/defn token? [x] (instance? TokenImpl x))

(cc/defn compare-equal-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is = to oldval. Returns true if
  set happened, else false."
  [!ref oldval newval]
  (let [[old new] (swap-vals! !ref (cc/fn [old new] (if (= old oldval) new old)) newval)]
    (not (= old new))))

(let [->token (cc/fn [!t]
                (->Token nil
                  (cc/fn token
                    ([] (reset! !t [nil nil]) nil) ; success, burn token
                    ([err] (reset! !t [nil err]) nil)))) ; failed attempt burns token, user must interact again
      step (cc/fn [!x v on?]
             (when (on? v)
               (let [[t err] @!x]
                 (cond ; reuse outstanding token but clear error for new attempt
                   (some? err) (compare-equal-and-set! !x [nil err] [(->token !x) nil])
                   () (compare-equal-and-set! !x [nil nil] [(->token !x) nil])))))]
  (hyperfiddle.electric3/defn Token
    ([v] (Token v some?))
    ([v on?] (let [!x (atom [nil nil])] ; single watch for consistency
               (step !x v on?)
               (watch !x))))) ; route error to call site
;; --------------------

(cc/defn capture-fn
  "Captures variability of a function under a stable identity.
  Return a proxy to the captured function.
  Use case: prevent unmount and remount when a cc/fn argument updates due to an inner variable dependency."
  []
  (let [!state (object-array 1)
        ret (cc/fn [& args] (cc/apply (aget !state 0) args))]
    (cc/fn [x]
      (aset !state 0 x)
      ret)))

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

;; TODO add back dom visibility check
(cc/defn -get-system-time-ms [& [_]] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

(def system-time-ms (m/signal (m/sample -get-system-time-ms <clock)))

(hyperfiddle.electric3/defn DOMVisibilityState []
  ;; "visible" | "hidden", historically also "prerender" and "unloaded"
  ;; nowadays functionally equivalent to document.hidden true|false
  ;; not clear if we should use ".visibilityState" or ".hidden" https://github.com/w3c/page-visibility/pull/64
  ;; we stick with the least restricting impl
  (client (input (m/reductions {} (.-visibilityState js/document) (dom-events/listen js/document "visibilitychange" (cc/fn [_event] (.-visibilityState js/document)))))))

(hyperfiddle.electric3/defn DOMVisible? [] (client (= "visible" (DOMVisibilityState)))) ; prevents "visible" to cross network, a boolean is enough

(hyperfiddle.electric3/defn System-time-ms [] (if (DOMVisible?) (input system-time-ms) (amb)))
(hyperfiddle.electric3/defn System-time-secs [] (math/floor-div (System-time-ms) 1000))

(cc/defn flow->incseq [uf]
  (m/ap (m/amb (i/empty-diff 0)
          (let [!first (atom true) v (m/?> uf)]
            (assoc (i/empty-diff 1) :grow (if @!first (do (swap! !first not) 1) 0), :change {0 v})))))

(cc/letfn [(task->incseq [t] (flow->incseq (m/ap (m/? t))))
           (initialized [t init-v] (m/relieve {} (m/ap (m/amb= init-v (m/? t)))))]
  (hyperfiddle.electric3/defn Task
    ([t] (join (task->incseq t)))
    ([t init-v] (input (initialized t init-v)))))

(cc/defn task-status "
Task -> continuous flow. State is [] before task completion, [result] after.
" [t] (m/reductions conj (m/ap (m/? t))))

;; Continuous flow of thunks -> incseq of 1 item containing result of the latest thunk executed via m/blk, or the empty incseq if it's still pending.
(cc/defn offload-clear-stale
  ([<f] (offload-clear-stale <f m/blk))
  ([<f executor] (i/diff-by {} (m/cp (m/?< (task-status (mu/wrap-task `offload-clear (m/via-call executor (m/?< <f)))))))))

(cc/defn offload-latch-stale
  ([<f] (offload-latch-stale <f m/blk))
  ([<f executor]
   (i/diff-by {}
     (m/reductions {} []
       (m/ap
         (try [(m/? (mu/wrap-task `offload-latch (m/via-call executor (m/?< <f))))]
              (catch #?(:clj Throwable :cljs :default) e
                ;; Swallow all exceptions if current Thread is already interrupted - rethrow otherwise.
                ;; Catching InterruptedException is not enough, because Datomic (at least) will throw a
                ;; domain-specific exception on thread interruption.
                (try (m/!) (catch Cancelled _ (m/amb)))
                (throw e))))))))

(hyperfiddle.electric3/defn Offload-reset "
Run thunk f on a thread, returning (e/amb) while awaiting and then the result. Switch back to
(e/amb) again when f changes while awaiting the next result."
  ([f!]          (join (offload-clear-stale (join (i/items (pure f!))))))
  ([f! executor] (join (offload-clear-stale (join (i/items (pure f!))) executor))))

(cc/defn- throwing-cancel-on-interrupted! [x] (m/!) x)

(hyperfiddle.electric3/defn Offload-latch "
Run thunk f on a thread, returning (e/amb) while awaiting and then the result. Buffer the latest
result while awaiting subsequent values of f, such that intermediate pending states are not seen."
  ([f!]          (join (offload-latch-stale (join (i/items (pure (comp throwing-cancel-on-interrupted! f!)))))))
  ([f! executor] (join (offload-latch-stale (join (i/items (pure (comp throwing-cancel-on-interrupted! f!)))) executor))))

(hyperfiddle.electric3/defn Offload
  "Run thunk f on a thread, returning (e/amb) while awaiting and then the result. Buffer the latest
result while awaiting subsequent values of f, such that intermediate pending states are not seen."
  ;; G: I chose e/Offload-latch as default impl because:
  ;;  - e/Offload behaved like e/Offload-latch (no intermediate amb)
  ;;  - only know use case for e/Offload-reset is "show spinner while task is pending".
  ;;    - Pending model for v3 is still WIP.
  ;;    - today a naive UI will blink - confusing to newcomers
  ([f!]          (Offload-latch f!))
  ([f! executor] (Offload-latch f! executor)))

(hyperfiddle.electric3/declare ^{:doc "Bound to the HTTP request of the page in which the current Electric program is running."}
  http-request)

(cc/defn measure< [nm v] (cm/measure nm v))
(defmacro Measure [nm v] `(->> ~v pure (measure< ~nm) join))

(cc/defn dbg< [nm v] (dbg/instrument* nm v))
(hyperfiddle.electric3/defn DBG [nm x] (->> x pure (dbg< nm) join))

;; low level unbundled conditional switch
;; for taking a branch on one peer and joining it on another
;; using low-level primitives
(defmacro case_ [test & branches] `(::lang/case_ ~test ~@branches))
(defmacro call_ [ctor] `(::lang/call ~ctor))

(cc/defn pp-ex [] (#?(:clj prn :cljs js/console.error) r/*e))

(comment
  (def _!x (atom (m/observe (cc/fn [!] (! nil) #(throw (ex-info "boom" {}))))))
  (def _ps ((m/cp (m/?< (m/?< (m/watch _!x)))) #(prn :step) #(prn :done)))
  @_ps
  (reset! _!x (m/observe (cc/fn [!] (! 42) #())))
  @_ps
  )

(hyperfiddle.electric3/declare
  ^{:dynamic true
    :doc "
A map of {`e-defn-qualified-sym e-defn-value}. To resolve e/defns at runtime.
"}
  *exports*)

(defmacro exports "Experimental. Return a map {sym efn} of all e/defn tagged with ::e/export in the current ns" []
  ;; We don't want to export all e/defns from all nss.
  ;; Instead users can (def exports (e/exports)) and (def exports (merge ns1/exports ns2/exports))
  (->> (r/exported-evars)
    (eduction (filter #(= *ns* (.-ns %))))
    (eduction (map symbol)
      (map (cc/fn [sym] [`'~sym sym])))
    (into {})))

(hyperfiddle.electric3/declare
  ^{:dynamic true
    :doc "
A place to store var bindings *intended* to be passed to host interop.
Electric doesn't automatically convey those bindings.

(e/defn With-electric-bindings [f]
  (let [bindings e/*bindings*] ; capture dynamic scope
    (e/capture-fn ; prevent reruns on any binding value change.
      (fn [& args]
        (with-bindings bindings
          (apply f args))))))"}
  *bindings*)
