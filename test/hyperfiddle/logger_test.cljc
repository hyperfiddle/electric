(ns hyperfiddle.logger-test
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.logger :as log]
            [clojure.tools.logging.test :as log-test]))

(tests
  "Log levels are totally ordered"
  (log-test/with-log

    (log/set-level! :error)
    (log/log :error "error")
    (log/log :warn "error")
    (log-test/logged? 'hyperfiddle.logger-test :error #"error") := true
    (log-test/logged? 'hyperfiddle.logger-test :warn #"error") := false

    (log/set-level! :warn)
    (log/log :error "warn")
    (log/log :warn "warn")
    (log/log :info "warn")
    (log-test/logged? 'hyperfiddle.logger-test :error #"warn") := true
    (log-test/logged? 'hyperfiddle.logger-test :warn #"warn") := true
    (log-test/logged? 'hyperfiddle.logger-test :info #"warn") := false

    (log/set-level! :info)
    (log/log :error "info")
    (log/log :warn "info")
    (log/log :info "info")
    (log/log :debug "info")
    (log-test/logged? 'hyperfiddle.logger-test :error #"info") := true
    (log-test/logged? 'hyperfiddle.logger-test :warn #"info") := true
    (log-test/logged? 'hyperfiddle.logger-test :info #"info") := true
    (log-test/logged? 'hyperfiddle.logger-test :debug #"info") := false

    (log/set-level! :debug)
    (log/log :error "debug")
    (log/log :warn "debug")
    (log/log :info "debug")
    (log/log :debug "debug")
    (log/log :trace "debug")
    (log-test/logged? 'hyperfiddle.logger-test :error #"debug") := true
    (log-test/logged? 'hyperfiddle.logger-test :warn #"debug") := true
    (log-test/logged? 'hyperfiddle.logger-test :info #"debug") := true
    (log-test/logged? 'hyperfiddle.logger-test :debug #"debug") := true
    (log-test/logged? 'hyperfiddle.logger-test :trace #"debug") := false

    (log/set-level! :trace)
    (log/log :error "trace")
    (log/log :warn "trace")
    (log/log :info "trace")
    (log/log :debug "trace")
    (log/log :trace "trace")
    (log-test/logged? 'hyperfiddle.logger-test :error #"trace") := true
    (log-test/logged? 'hyperfiddle.logger-test :warn #"trace") := true
    (log-test/logged? 'hyperfiddle.logger-test :info #"trace") := true
    (log-test/logged? 'hyperfiddle.logger-test :debug #"trace") := true
    (log-test/logged? 'hyperfiddle.logger-test :trace #"trace") := true))

