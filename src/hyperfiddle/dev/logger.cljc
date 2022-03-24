(ns hyperfiddle.dev.logger
  #?(:clj (:require [clojure.tools.logging :as log]
                    [clojure.tools.logging.impl :as impl])
     :cljs (:require-macros [hyperfiddle.dev.logger :refer [log debug info trace warn error]])))

(def levels [:trace :debug :info :warn :error])

#?(:cljs (goog-define LEVEL "trace"))

(def ^:dynamic *LEVEL* #?(:clj :trace, :cljs (keyword LEVEL)))

(defn ^:export set-level! [level]
  (let [level (if (string? level) (keyword level) level)]
    (assert ((set levels) level) "Invalid log level.")
    #?(:clj (alter-var-root #'*LEVEL* (constantly level))
       :cljs (set! *LEVEL* level))))

(defn active? [level]
  (let [rank (zipmap levels (range (count levels)))]
    (>= (rank level) (rank *LEVEL*))))

(def js-level {:trace "⚪ TRACE"
               :debug "🐛 DEBUG"
               :info  "ℹ️ INFO"
               :warn  "  WARN"
               :error "  ERROR"})

#?(:clj (defn clj-log [level ns & args]
          (let [logger (impl/get-logger log/*logger-factory* ns)]
            (if (instance? Exception (first args))
              (log/log* logger level (first args) (apply print-str (rest args)))
              (log/log* logger level nil (apply print-str args))))))

;; FIXME rename
(defn js-log* [level ns & args]
  (when (active? level)
    #?(:clj  (apply clj-log level ns args)
       :cljs (let [f (case level
                       (:trace :debug) js/console.debug
                       :info           js/console.info
                       :warn           js/console.warn
                       :error          js/console.error
                       js/console.log)]
               (apply f (get js-level level "") ns args)
               (last args)))))

(defn- js-log [level env args]
  (let [ns (str (:name (:ns env)))]
    `(js-log* ~level ~ns ~@args)))

(def logger {nil    `log/log
             :trace `log/trace
             :debug `log/debug
             :info  `log/info
             :warn  `log/warn
             :error `log/error})

(defn- cljs? [env] (some? (:js-globals env)))

#?(:clj (def ^:private log*-impl log/log*))
#?(:clj (def ^:private print-str-impl clojure.core/print-str))

#?(:clj
   (defn log* [logger level throwable messages]
     (log*-impl logger level throwable (apply print-str-impl messages))
     (last messages)))

(defn gen-eval-once-call [level env args]
  (if (cljs? env)
    (js-log level env args)
    `(when (active? ~level)
       (~(logger level) ~@args))
    #_`(with-redefs [log/log*               log*
                   clojure.core/print-str vector]
       (~(logger level) ~@args))))

(defmacro log   [level & args] (gen-eval-once-call level &env args))
(defmacro trace [& args] (gen-eval-once-call :trace &env args))
(defmacro debug [& args] (gen-eval-once-call :debug &env args))
(defmacro info  [& args] (gen-eval-once-call :info  &env args))
(defmacro warn  [& args] (gen-eval-once-call :warn  &env args))
(defmacro error [& args] (gen-eval-once-call :error &env args))

;; `FATAL` level not implemented because:
;; - it’s not a thing in `js/console`
;; - `clojure.tools.logging` rewrites it silently to `ERROR`. Highly confusing.
;;    reason: SLF4J don’t support it by default http://www.slf4j.org/faq.html#fatal .

