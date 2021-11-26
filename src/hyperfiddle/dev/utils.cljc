(ns hyperfiddle.dev.utils
  #?(:clj (:require [clojure.tools.logging :as log])
     :cljs (:require-macros [hyperfiddle.dev.utils :refer [log debug info trace warn error]])))

(defn- cljs? [env] (:js-globals env))

(def js-level {:trace "⚪ TRACE"
               :debug "🐛 DEBUG"
               :info  "ℹ️ INFO"
               :warn  "  WARN"
               :error "  ERROR"})

(defn- js-log* [level ns & args]
  #?(:cljs (let [f (case level
                     (:trace :debug) js/console.debug
                     :info           js/console.info
                     :warn           js/console.warn
                     :error          js/console.error
                     js/console.log)]
             (apply f (get js-level level "") ns args))))

(defn- js-log [level env args]
  (let [ns (str (:name (:ns env)))]
    `(js-log* ~level ~ns ~@args)))

(defmacro log [& args]
  (if (cljs? &env)
    (js-log nil &env args)
    `(log/log ~@args)))

(defmacro trace [& args]
  (if (cljs? &env)
    (js-log :trace &env args)
    `(log/trace ~@args)))

(defmacro debug [& args]
  (if (cljs? &env)
    (js-log :debug &env args)
    `(log/debug ~@args)))

(defmacro info [& args]
  (if (cljs? &env)
    (js-log :info &env args)
    `(log/info ~@args)))

(defmacro warn [& args]
  (if (cljs? &env)
    (js-log :warn &env args)
    `(log/warn ~@args)))

(defmacro error [& args]
  (if (cljs? &env)
    (js-log :error &env args)
    `(log/error ~@args)))

;; `FATAL` level not implemented because:
;; - it’s not a thing in `js/console`
;; - `clojure.tools.logging` rewrites it silently to `ERROR`. Highly confusing.
;;    reason: SLF4J don’t support it by default http://www.slf4j.org/faq.html#fatal .

