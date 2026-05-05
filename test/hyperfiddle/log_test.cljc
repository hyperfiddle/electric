(ns hyperfiddle.log-test
  (:require [hyperfiddle.log :as log]
            #?(:clj [hyperfiddle.rcf :as rcf :refer [tests]]))
  #?(:clj (:import [org.slf4j Logger]
                   [ch.qos.logback.classic Level]
                   [ch.qos.logback.core.read ListAppender])))

#?(:clj
   (defn fresh-logger
     ^ch.qos.logback.classic.Logger [name level]
     (let [logger ^ch.qos.logback.classic.Logger (log/get-logger name)
           appender (doto (ListAppender.) (.setName "test") (.start))]
       (.detachAndStopAllAppenders logger)
       (.setAdditive logger false)
       (.setLevel logger level)
       (.addAppender logger appender)
       logger)))

#?(:clj
   (defn events [^ch.qos.logback.classic.Logger logger]
     (.list ^ListAppender (.getAppender logger "test"))))

#?(:clj
   (tests
     "get-logger returns an SLF4J Logger"
     (instance? Logger (log/get-logger "hyperfiddle.log-test.basic")) := true))

#?(:clj
   (tests
     "info with a plain string emits one INFO event"
     (let [logger (fresh-logger "hyperfiddle.log-test.info-string" Level/INFO)]
       (log/info logger "hello")
       (count (events logger)) := 1
       (.getMessage ^ch.qos.logback.classic.spi.ILoggingEvent (first (events logger))) := "hello"
       (str (.getLevel ^ch.qos.logback.classic.spi.ILoggingEvent (first (events logger)))) := "INFO")))

#?(:clj
   (tests
     "info with a thunk: thunk invoked once and result logged when level enabled"
     (let [logger (fresh-logger "hyperfiddle.log-test.info-thunk-on" Level/INFO)
           called (atom 0)]
       (log/info logger #(do (swap! called inc) "from-thunk"))
       @called := 1
       (count (events logger)) := 1
       (.getMessage ^ch.qos.logback.classic.spi.ILoggingEvent (first (events logger))) := "from-thunk")))

#?(:clj
   (tests
     "info with a thunk: thunk NOT invoked when level disabled"
     (let [logger (fresh-logger "hyperfiddle.log-test.info-thunk-off" Level/OFF)
           called (atom 0)]
       (log/info logger #(do (swap! called inc) (throw (ex-info "boom" {}))))
       @called := 0
       (count (events logger)) := 0)))

#?(:clj
   (tests
     "info with a string + exception: exception attached"
     (let [logger (fresh-logger "hyperfiddle.log-test.info-ex" Level/INFO)
           ex (Exception. "the cause")]
       (log/info logger "broken" ex)
       (let [^ch.qos.logback.classic.spi.ILoggingEvent ev (first (events logger))]
         (.getMessage ev) := "broken"
         (.. ev getThrowableProxy getMessage) := "the cause"))))

#?(:clj
   (tests
     "trace/debug/info/warn/error all reach the appender at TRACE threshold"
     (let [logger (fresh-logger "hyperfiddle.log-test.all-levels" Level/TRACE)]
       (log/trace logger "t")
       (log/debug logger "d")
       (log/info  logger "i")
       (log/warn  logger "w")
       (log/error logger "e")
       (mapv #(str (.getLevel ^ch.qos.logback.classic.spi.ILoggingEvent %)) (events logger))
       := ["TRACE" "DEBUG" "INFO" "WARN" "ERROR"])))

#?(:clj
   (tests
     "WARN threshold suppresses TRACE/DEBUG/INFO"
     (let [logger (fresh-logger "hyperfiddle.log-test.threshold-warn" Level/WARN)]
       (log/trace logger "t")
       (log/debug logger "d")
       (log/info  logger "i")
       (log/warn  logger "w")
       (log/error logger "e")
       (mapv #(str (.getLevel ^ch.qos.logback.classic.spi.ILoggingEvent %)) (events logger))
       := ["WARN" "ERROR"])))

#?(:clj
   (tests
     "error with exception attaches the throwable"
     (let [logger (fresh-logger "hyperfiddle.log-test.error-ex" Level/ERROR)
           ex (RuntimeException. "kaboom")]
       (log/error logger "fatal" ex)
       (let [^ch.qos.logback.classic.spi.ILoggingEvent ev (first (events logger))]
         (.getMessage ev) := "fatal"
         (.. ev getThrowableProxy getMessage) := "kaboom"))))
