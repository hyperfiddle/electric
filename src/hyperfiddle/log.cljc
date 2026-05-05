(ns hyperfiddle.log
  "Thin facade over SLF4J (JVM) and goog.log (CLJS).
  See electric/docs/_design/hyperfiddle-agent-aot.md."
  #?(:clj  (:import [org.slf4j Logger LoggerFactory])
     :cljs (:require [goog.log :as glog]
                     [goog.debug.TextFormatter])))

(defn get-logger*
  "Returns the platform Logger for `name` (string). Internal — prefer the
  `get-logger` macro at call sites."
  [name]
  #?(:clj  (LoggerFactory/getLogger ^String name)
     :cljs (glog/getLogger name)))

#?(:clj
   (defmacro get-logger
     "Returns the platform Logger.
     Zero-arg form resolves the current namespace name at compile time —
     `(def logger (log/get-logger))` is the idiomatic call site.
     One-arg form takes an explicit string name."
     ([]
      `(get-logger* ~(str (or (some-> &env :ns :name) *ns*))))
     ([name]
      `(get-logger* ~name))))

#?(:clj
   (defn- ^String coerce-msg [msg-or-fn]
     (if (string? msg-or-fn) msg-or-fn (msg-or-fn))))

(defn trace
  ([logger msg-or-fn] (trace logger msg-or-fn nil))
  ([logger msg-or-fn ex]
   #?(:clj  (when (.isTraceEnabled ^Logger logger)
              (let [msg (coerce-msg msg-or-fn)]
                (if ex (.trace ^Logger logger msg ^Throwable ex) (.trace ^Logger logger msg))))
      :cljs (let [lvl (.-FINEST ^js glog/Level)]
              (if ex (glog/log logger lvl msg-or-fn ex) (glog/log logger lvl msg-or-fn))))))

(defn debug
  ([logger msg-or-fn] (debug logger msg-or-fn nil))
  ([logger msg-or-fn ex]
   #?(:clj  (when (.isDebugEnabled ^Logger logger)
              (let [msg (coerce-msg msg-or-fn)]
                (if ex (.debug ^Logger logger msg ^Throwable ex) (.debug ^Logger logger msg))))
      :cljs (if ex (glog/fine logger msg-or-fn ex) (glog/fine logger msg-or-fn)))))

(defn info
  ([logger msg-or-fn] (info logger msg-or-fn nil))
  ([logger msg-or-fn ex]
   #?(:clj  (when (.isInfoEnabled ^Logger logger)
              (let [msg (coerce-msg msg-or-fn)]
                (if ex (.info ^Logger logger msg ^Throwable ex) (.info ^Logger logger msg))))
      :cljs (if ex (glog/info logger msg-or-fn ex) (glog/info logger msg-or-fn)))))

(defn warn
  ([logger msg-or-fn] (warn logger msg-or-fn nil))
  ([logger msg-or-fn ex]
   #?(:clj  (when (.isWarnEnabled ^Logger logger)
              (let [msg (coerce-msg msg-or-fn)]
                (if ex (.warn ^Logger logger msg ^Throwable ex) (.warn ^Logger logger msg))))
      :cljs (if ex (glog/warning logger msg-or-fn ex) (glog/warning logger msg-or-fn)))))

(defn error
  ([logger msg-or-fn] (error logger msg-or-fn nil))
  ([logger msg-or-fn ex]
   #?(:clj  (when (.isErrorEnabled ^Logger logger)
              (let [msg (coerce-msg msg-or-fn)]
                (if ex (.error ^Logger logger msg ^Throwable ex) (.error ^Logger logger msg))))
      :cljs (if ex (glog/error logger msg-or-fn ex) (glog/error logger msg-or-fn)))))

#?(:cljs (defonce ^:private console-handler-installed? (atom false)))

;; automatically called just below. Matches our current use case. In case it diverges in the future
;; we'll need to install for each entrypoint / each environment.
(defn install-console-handler!
  "CLJS only. Installs a goog.log root handler that mirrors `goog.debug.Console`:
  formats records via `goog.debug.TextFormatter` and routes them by level to
  `console.error/warn/log/debug` (CONFIG and above → `console.log`, matching
  goog's mapping that avoids `console.info`'s cross-browser visibility quirks).
  Differs from goog.debug.Console only in passing the exception as a 2nd arg
  *only when present*, sidestepping the trailing `<empty string>` token in
  browser devtools. Idempotent. JVM no-op."
  []
  #?(:clj nil
     :cljs (when (compare-and-set! console-handler-installed? false true)
             (let [severe-val  (.-value (.-SEVERE  ^js glog/Level))
                   warning-val (.-value (.-WARNING ^js glog/Level))
                   config-val  (.-value (.-CONFIG  ^js glog/Level))
                   ^js fmt     (goog.debug.TextFormatter.)]
               (set! (.-showAbsoluteTime  fmt) false)
               (set! (.-showExceptionText fmt) false)
               (set! (.-appendNewline     fmt) false)
               (glog/addHandler (glog/getLogger "")
                 (fn [^js record]
                   (let [level-val (.-value (.getLevel record))
                         fn-name   (cond
                                     (>= level-val severe-val)  "error"
                                     (>= level-val warning-val) "warn"
                                     (>= level-val config-val)  "log"
                                     :else                      "debug")
                         msg       (.formatRecord fmt record)
                         ex        (.getException record)]
                     (if ex
                       ((aget js/console fn-name) msg ex)
                       ((aget js/console fn-name) msg))))))
             true)))

#?(:cljs (install-console-handler!))

(def ^:private valid-levels #{:trace :debug :info :warn :error :off :all})

(defn set-level
  "Sets the minimum log level on `logger`. `level` ∈ #{:trace :debug :info :warn :error :off :all}.
  JVM: requires `ch.qos.logback.classic.Logger` (binding-specific — the SLF4J `Logger` interface
  has no `setLevel`). Routed through `java.lang.reflect.Method` so /electric carries no compile-time
  logback dep and the call site emits no Clojure reflection warning.
  CLJS: forwards to `goog.log/setLevel`."
  [logger level]
  (when-not (valid-levels level)
    (throw (ex-info "Unknown log level" {:level level :valid valid-levels})))
  #?(:clj  (let [^Class level-class                  (Class/forName "ch.qos.logback.classic.Level")
                 field-name                          (case level
                                                       :trace "TRACE", :debug "DEBUG", :info "INFO",
                                                       :warn "WARN",   :error "ERROR",
                                                       :off "OFF",     :all "ALL")
                 level-val                           (.get (.getField level-class field-name) nil)
                 ^Class logger-class                 (.getClass ^Object logger)
                 ^java.lang.reflect.Method set-level (.getMethod logger-class "setLevel"
                                                       (into-array Class [level-class]))]
             (.invoke set-level logger (into-array Object [level-val])))
     :cljs (glog/setLevel logger
             (case level
               :trace (.-FINEST  ^js glog/Level)
               :debug (.-FINE    ^js glog/Level)
               :info  (.-INFO    ^js glog/Level)
               :warn  (.-WARNING ^js glog/Level)
               :error (.-SEVERE  ^js glog/Level)
               :off   (.-OFF     ^js glog/Level)
               :all   (.-ALL     ^js glog/Level)))))
