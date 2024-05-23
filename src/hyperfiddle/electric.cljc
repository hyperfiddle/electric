(ns hyperfiddle.electric
  (:refer-clojure :exclude [eval def defn fn for partial apply])
  (:require [clojure.core :as cc]
            #?(:clj [clojure.tools.logging :as log])
            [clojure.spec.alpha :as s]
            contrib.data
            [contrib.cljs-target :refer [do-browser]]
            [contrib.missionary-contrib :as mx]
            [contrib.assert :as ca]
            #?(:clj [hyperfiddle.electric.impl.expand :as expand])
            [hyperfiddle.electric.impl.lang :as lang]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.for :as for]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana]) ; todo remove
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.electric-client])
            [hyperfiddle.electric.impl.io :as io]
            [hyperfiddle.electric.debug :as dbg]
            [clojure.string :as str]
            [contrib.str])
  #?(:cljs (:require-macros
            [hyperfiddle.electric :refer [offload-task offload def check-electric
                                             client server fn fn* defn for-by for watch discard with-cycle
                                             partial-dynamic partial on-unmount with-zero-config-entrypoint
                                             snapshot apply for-event for-event-pending
                                             for-event-pending-switch do-event do-event-pending]]))
  (:import (hyperfiddle.electric Failure Pending FailureInfo)
           (missionary Cancelled)))

(s/def ::user-version string?)

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
     (defmethod ana/macroexpand-hook `hyperfiddle.electric/local [the-var form env args] (reduced `(hyperfiddle.electric/local ~@args)))

     ;; Don't expand cc/binding (prevent infinite loop). Explicit implicit do
     (defmethod ana/macroexpand-hook 'clojure.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))
     (defmethod ana/macroexpand-hook 'cljs.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))))

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

(cc/defn normalize-env [env] (if (:js-globals env) env {:locals env, :ns (ns-name *ns*)}))

#?(:clj (cc/defn read-web-config [conf-str]
          (let [args (mapv keyword (str/split conf-str #","))]
            {::lang/peers (cc/apply hash-map args), ::lang/current (first args)})))

#?(:clj (def web-config
          (or (some-> (System/getProperty "hyperfiddle.electric.web-config-peers") read-web-config)
            {::lang/peers {:client :cljs :server :clj}
             ::lang/current :server})))

(defmacro local
  "Single peer loopback system without whitelist. Returns boot task."
  {:style/indent 0}
  [& body]
  (let [env (normalize-env &env)
        client (lang/analyze (merge env web-config {::lang/me :client}) `(do ~@body))
        client-info (r/compile "clocal" client env)
        server (lang/analyze (merge env web-config {::lang/me :server}) `(do ~@body))
        server-info (r/compile "slocal" server env)]
    `(pair
       (r/main ~client-info)
       (r/main ~server-info))))

(defmacro run "test entrypoint without whitelist." {:style/indent 0} [& body] `((local ~@body) {} {}))

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
  ([symbol] `(hyperfiddle.electric/def ~symbol [::lang/unbound '~(cc/symbol (str *ns*) (str symbol))]))
  ([symbol docstring init]
   (assert (string? docstring))
   (#'def &form &env (vary-meta symbol assoc :doc docstring) init))
  ([symbol init] (lang/-def (merge (normalize-env &env) web-config) symbol init)))

(defmacro check-electric [fn form]
  (if expand/*electric*
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(defmacro client {:style/indent 0} [& body]
  `(check-electric client
     (::lang/toggle :client ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::client) ~@body)))
(defmacro server {:style/indent 0} [& body]
  `(check-electric server
     (::lang/toggle :server ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::server) ~@body)))

(cc/defn -get-system-time-ms [& [_]] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))


;; DOM event utilities promoted due to visibility-state being critical
;; TODO find a way to move out of electric core
#?(:cljs (cc/defn dom-listener [node typ f opts]
           (.addEventListener node typ f (clj->js opts))
           #(.removeEventListener node typ f)))

;; TODO find a way to move out of electric core
#?(:cljs (cc/defn listen> ; we intend to replace this in UI5 workstream
           ([node event-type] (listen> node event-type identity {}))
           ([node event-type keep-fn!] (listen> node event-type keep-fn! {}))
           ([node event-type keep-fn! opts]
            (m/relieve {}
              (m/observe (cc/fn [!]
                           (dom-listener node event-type #(when-some [v (keep-fn! %)]
                                                            (! v)) opts)))))))

;; TODO find a way to move out of electric core
#?(:cljs (def <dom-visibility-state
           (do-browser
             (->> (listen> js/document "visibilitychange")
               (m/reductions {} (.-visibilityState js/document))
               (m/latest (cc/fn [_] (.-visibilityState js/document)))))))

;; TODO find a way to move out of electric core
(hyperfiddle.electric/def dom-visibility-state (client (new (identity <dom-visibility-state))))

;; TODO find a way to move out of electric core
#?(:cljs (def <dom-mousemove (do-browser (m/reductions {} r/pending (listen> js/document "mousemove")))))
(hyperfiddle.electric/def dom-mousemove "mousemove events, Pending if unknown"
  (client (new (identity <dom-mousemove))))

;; TODO find a way to depend on dom stuff without having to promote them to electric core.
(hyperfiddle.electric/def system-time-ms "ms since 1970 Jan 1"
  (client
    (if (= "visible" dom-visibility-state)
      (new (m/sample -get-system-time-ms <clock))
      (throw (Pending.))))) ; tab is hidden, no clock. (This guards NPEs in userland)

(hyperfiddle.electric/def system-time-secs "seconds since 1970 Jan 1" (/ system-time-ms 1000.0))

(defmacro fn*
  "Low-level construct. Use `hyperfiddle.electric/fn` instead.
   Bare-bone reactive anonymous function. Single arity, no arity check, no variadic args support, no self-recur."
  ;; G: `e/fn*` produces a smaller program than `e/fn`. Experts can use `e/fn*` in internals and libraries.
  ;;    Users should default to `e/fn`.
  [args & body]
  (let [debug-info {::dbg/type :reactive-fn
                    ::dbg/meta (select-keys (meta &form) [:file :line])}]
    (if (seq args)
      `(::lang/closure (let [~@(interleave args lang/arg-sym)] ~@body) ~debug-info)
      `(::lang/closure (do ~@body) ~debug-info))))

(cc/defn -check-recur-arity [provided actual fname]
  (when (not= provided actual)
    (throw (ex-info (str "You `recur`d in " (or fname "<unnamed-efn>") " with " provided
                      " argument" (when-not (= 1 provided) "s") " but it has " actual
                      " positional argument" (when-not (= 1 actual) "s"))
             {}))))

#?(:clj (cc/defn- varargs? [args] (boolean (and (seq args) (= '& (-> args pop peek))))))

#?(:clj (cc/defn- ?bind-self [code ?name] (cond->> code ?name (list 'let [?name `lang/%closure]))))

#?(:clj (cc/defn- -build-fn-arity [?name args body]
          [(count args)
           `(binding [lang/rec (::lang/closure
                             (case (-check-recur-arity lang/%arity ~(count args) '~?name)
                               (let [~@(interleave args lang/arg-sym)] ~@body)))]
              (new lang/rec ~@(take (count args) lang/arg-sym)))]))

#?(:clj (cc/defn- -build-vararg-arity [?name args body]
          (let [npos (-> args count (- 2)), unvarargd (-> args pop pop (conj (peek args))), v (gensym "varargs")]
            `(binding [lang/rec (::lang/closure (case (-check-recur-arity lang/%arity ~(inc npos) '~?name)
                                                  (let [~@(interleave unvarargd lang/arg-sym)] ~@body)))]
               (new lang/rec ~@(take npos lang/arg-sym)
                 (let [~v (into [] (drop ~npos) lang/%args)]
                   (when (seq ~v) ; varargs value is `nil` when no args provided
                     ~(if (map? (peek args))
                        `(if (even? (count ~v))
                           (cc/apply hash-map ~v) ; (MapVararg. :x 1)
                           (merge (cc/apply hash-map (pop ~v)) (peek ~v))) ; (MapVararg. :x 1 {:y 2})
                        v))))))))

#?(:clj (cc/defn ->narity-set [arities]
          (into (sorted-set) (comp (map #(take-while (complement #{'&}) %)) (map count)) arities)))
#?(:clj (cc/defn arity-holes [arity-set]
          (remove arity-set (range (reduce max arity-set)))))

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
        arity-set (->narity-set (map first arities))
        {positionals false, varargs true} (group-by (comp varargs? first) arities)
        _ (check-only-one-vararg! ?name (mapv first varargs))
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))
        positional-branches (into [] (map (cc/fn [[args & body]] (-build-fn-arity ?name args body))) positionals)]
    (list `check-electric `fn
      (list ::lang/closure
        (-> `(case lang/%arity
               ~@(into [] (comp cat cat) [positional-branches])
               ~@(if (seq varargs)
                   (conj [(arity-holes arity-set) `(-throw-arity '~?name lang/%arity ~(str/join ", " arity-set))]
                     (-build-vararg-arity ?name (ffirst varargs) (nfirst varargs)))
                   [`(-throw-arity '~?name lang/%arity ~(str/join ", " arity-set))])
               #_(-throw-arity '~?name lang/%arity ~(->> arities (eduction (map first)) ->narity-set (str/join ", "))))
          (?bind-self ?name))
        {::dbg/name ?name, ::dbg/type (or (::dbg/type (meta ?name)) :reactive-fn)
         ::dbg/meta (merge (select-keys (meta &form) [:file :line])
                      (select-keys (meta ?name) [:file :line])
                      {::dbg/ns (name (.getName *ns*))})}))))

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

(cc/defn- -splicev [args] (if (empty? args) args (into [] cat [(pop args) (peek args)])))

(hyperfiddle.electric/defn* Apply* [F args] ; we use `defn*` instead of e/def e/fn* for better stacktraces
  (let [spliced (-splicev args)]
    (case (count spliced)
      0 (new F)
      1 (new F (nth spliced 0))
      2 (new F (nth spliced 0) (nth spliced 1))
      3 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2))
      4 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3))
      5 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4))
      6 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5))
      7 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6))
      8 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7))
      9 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8))
      10 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9))
      11 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10))
      12 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11))
      13 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12))
      14 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13))
      15 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14))
      16 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15))
      17 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16))
      18 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17))
      19 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17) (nth spliced 18))
      20 (new F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17) (nth spliced 18) (nth spliced 19)))))

(defmacro apply [F & args]
  (assert (not (empty? args)) (str `apply " takes and Electric function and at least one argument. Given 0.")) ; matches clojure behavior
  `(new Apply* ~F [~@args]))


(defmacro for-by
  {:style/indent 2}
  [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(let [xs# ~v]
       (new (r/bind for/map-by ~kf
              (cc/partial (::lang/inject lang/%0)
                (::lang/closure
                  (let [~s lang/%0]
                    (for-by ~kf ~bindings ~@body))))
              (::lang/lift xs#))))
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

(defmacro watch "Derive a reactive value from a Clojure atom or reference."
  [!x]
  `(check-electric watch (new (checked-watch ~!x))))

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

(defmacro flow
  "Transform an Electric value into a Missionary flow by \"quoting\" it with e/fn.
Quoting it directly is idiomatic as well."
  {:style/indent 0}
  [x] `(hyperfiddle.electric/fn [] ~x))

(defmacro discard
  "Silence \"Unserializable reference transfer\"; inlining `(do ... nil)` is idiomatic as well"
  {:style/indent 0}
  [& body] `(do ~@body nil))

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

(cc/defn log-root-error [exception async-stack-trace]
  #?(:clj (let [ex (dbg/empty-client-exception exception)
                ex (dbg/clean-jvm-stack-trace! (dbg/remove-async-stack-trace ex))
                ex (dbg/add-async-frames! ex async-stack-trace)]
            (if-some [data (not-empty (dissoc (ex-data ex) ::type))]
              (log/error ex "Uncaugh exception:" (ex-message ex) "\n" data)
              (log/error ex "Uncaugh exception")))
     :cljs (js/console.error exception)))

#?(:cljs
   (cc/defn- client-log-server-error [message async-trace]
     (let [err (js/Error. message)]
       (set! (.-stack err) (first (str/split-lines (.-stack err))))
       (js/console.error err) ; We'd like to bundle these two messages into one, but chrome refuses to render "\n" after an exception.
                              ; We would need browser-custom formatting. Not worth it today.
       (js/console.log (->> (dbg/render-async-stack-trace async-trace)
                            (contrib.str/align-regexp #" at ")
                            (dbg/left-pad-stack-trace 4))
         "\n" "This is a server-side exception. The full exception was printed on the server."))))

#?(:cljs
   (cc/defn- client-log-client-error [ex async-trace]
     (set! (.-stack ex) (dbg/cleanup-js-stack-trace (.-stack ex)))
     (js/console.error ex) ; We'd like to bundle these two messages into one, but chrome refuses to render "\n" after an exception.
                           ; We would need browser-custom formatting. Not worth it today.
     (js/console.log (->> (dbg/render-async-stack-trace async-trace)
                        (contrib.str/align-regexp #" at ")
                        (dbg/left-pad-stack-trace 4)))))

(cc/defn- log-on-server-that-error-happened-on-client []
  ;; FIXME should be inlinable, but Electric fails to resolve log/info var in a `server` block.
  #?(:clj (log/info "This is a client-side exception. The full exception was printed on the client.")))

(hyperfiddle.electric/defn ?PrintClientException [msg id]
  (server
    (let [async-trace (::dbg/trace (ex-data lang/trace))]
      (try
        (client
          (if-some [ex (io/get-original-ex id)]
            (do
              (client-log-client-error ex async-trace)
              (try (server (log-on-server-that-error-happened-on-client))
                   (catch Pending _)))
            (client-log-server-error msg async-trace)))
        (catch Pending _)))))

(defmacro with-zero-config-entrypoint
  {:style/indent 0}
  [& body]
  `(try
     (do ~@body)
     (catch Pending _#)                 ; silently ignore
     (catch Cancelled e# (throw e#))    ; bypass catchall, app is shutting down
     (catch Throwable err#
       (log-root-error (or (io/get-original-ex (dbg/ex-id lang/trace)) err#) (dbg/get-async-trace lang/trace))
       (new ?PrintClientException (ex-message err#) (dbg/ex-id lang/trace)))))

(defmacro boot-server [opts Main & args]
  (let [env (merge (normalize-env &env) web-config {::lang/me :server} opts)
        ir (lang/analyze env `(with-zero-config-entrypoint (new ~Main ~@args)))
        info (r/compile (gensym) ir env)]
    `(r/main ~info)))

(defmacro boot-client [opts Main & args]
  (let [env (merge (normalize-env &env) web-config {::lang/me :client} opts)
        ir (lang/analyze env `(with-zero-config-entrypoint (new ~Main ~@args)))
        info (r/compile (gensym) ir env)]
    `(hyperfiddle.electric-client/reload-when-stale
       (hyperfiddle.electric-client/boot-with-retry
         (r/main ~info)
         hyperfiddle.electric-client/connector))))

(hyperfiddle.electric/def http-request "Bound to the HTTP request of the page in which the current Electric program is running." nil)

(cc/defn -snapshot [flow] (->> flow (m/eduction (contrib.data/take-upto (complement #{r/pending})))))

(defmacro snapshot
  "Snapshots the first non-Pending value of reactive value `x` and freezes it,
inhibiting all further reactive updates."
  [x] `(check-electric snapshot (new (-snapshot (hyperfiddle.electric/fn* [] ~x)))))

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
