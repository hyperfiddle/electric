(ns ^:no-doc hyperfiddle.logger
  "A Photon compatible logger. Not public API. 
  Thin uniform macro wrapper on top of clojure.tools.logging and js/console.log|warn|error.
  `Timbre` rely on `cc/fn`, which is not yet supported by photon. It will be at some point.
  `cc/print` flushes to `*out*` immediatly, concurent writes produces gibberish. A proper logger
  sequences writes.
  To be removed when timbre (or an equivalent) is supported by photon.
  Backed by clojure.tools.logging > backed by SLF4J > backed by logback. See logback.xml."
  (:refer-clojure :exclude [time])
  #?(:clj (:require [clojure.tools.logging :as log]
                    [clojure.tools.logging.impl :as impl])
     :cljs (:require-macros [hyperfiddle.logger :refer [log debug info trace warn error]])))

(def levels [:trace :debug :info :warn :error])

#?(:cljs (goog-define LEVEL "trace")) ; Set log level as cljs compile time constant.

(def ^:dynamic *LEVEL* #?(:clj :trace, :cljs (keyword LEVEL)))

(defn ^:export set-level! "Set runtime log level. See `hyperfiddle.logger/levels`."
  [level]
  (let [level (if (string? level) (keyword level) level)]
    (assert ((set levels) level) "Invalid log level.")
    #?(:clj (alter-var-root #'*LEVEL* (constantly level))
       :cljs (set! *LEVEL* level))))

(let [rank (zipmap levels (range (count levels)))]
  (defn active? [level]
    (>= (rank level) (rank *LEVEL*))))

(def js-prefix {:trace "⚪ TRACE"
                :debug "🐛 DEBUG"
                :info  "ℹ️ INFO"
                :warn  "  WARN"
                :error "  ERROR"})

(defn log*
  "When photon compiles to clojurescript, the server peer still don't know about the js/ namespace."
  [level ns & args]
  (when (active? level)
    #?(:clj  (let [logger (impl/get-logger log/*logger-factory* ns)]
               (if (instance? Exception (first args))
                 (log/log* logger level (first args) (apply print-str (rest args)))
                 (log/log* logger level nil (apply print-str args))))
       :cljs (let [logger (case level
                            (:trace :debug) js/console.debug
                            :info           js/console.info
                            :warn           js/console.warn
                            :error          js/console.error
                            js/console.log)]
               (apply logger (get js-prefix level "") ns args)))))

(defn- gen-log [level env args]
  (if (:js-globals env)
    `(log* ~level ~(str (:name (:ns env))) ~@args)
    `(when (active? ~level)
       (log/logp ~level ~@args))))

(defmacro log   [level & args] (gen-log level &env args))
(defmacro trace [& args] (gen-log :trace &env args))
(defmacro debug [& args] (gen-log :debug &env args))
(defmacro info  [& args] (gen-log :info  &env args))
(defmacro warn  [& args] (gen-log :warn  &env args))
(defmacro error [& args] (gen-log :error &env args))

;; `FATAL` level not implemented because:
;; - it’s not a thing in `js/console`
;; - `clojure.tools.logging` rewrites it silently to `ERROR`. Highly confusing.
;;    reason: SLF4J don’t support it by default http://www.slf4j.org/faq.html#fatal .

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  [message expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str ~message " - elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))
