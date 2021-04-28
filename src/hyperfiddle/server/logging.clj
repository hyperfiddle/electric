(ns hyperfiddle.server.logging
  (:require clojure.stacktrace
            [clojure.pprint :refer [pprint]]
            [taoensso.encore :as encore]
            [taoensso.timbre :as timbre]))

;; Adapted from https://github.com/ptaoussanis/timbre/issues/208#issuecomment-269008644
(defn ns-patterns-mw [ns-patterns]
  (let [filters           (->> (keys ns-patterns)
                     (filter string?)
                     (sort-by count) reverse                ; longest ns pattern wins
                     (map (juxt identity encore/compile-ns-filter))
                     doall)
        ns-str->log-level (encore/memoize_
                           (fn [?ns-str]
                             (let [namesp (-> (some (fn [[namesp f]] (when (f ?ns-str) namesp)) filters)
                                              (or :all))]
                               (get ns-patterns namesp))))]
    (fn [{:keys [?ns-str config level] :as opts}]
      (let [loglevel (or (ns-str->log-level ?ns-str) (get config :level))]
        (when (timbre/level>= level loglevel)
          opts)))))

(timbre/merge-config!
 {:level          :debug
  :timestamp-opts {:pattern "HH:mm"}
  :output-fn      (fn output-fn
                    ([data] (output-fn nil data))
                    ([opts data]
                     (let [{:keys [level ?err #_vargs msg_ ?ns-str ?file _hostname
                                   _timestamp ?line]} data]
                       (str
                        (timbre/color-str :blue (name level)) " "
                        "[" #_(contrib.string/str-last-n 25) (or ?ns-str ?file "?") ":" (or ?line "?") "]"
                        #_"\n  "
                        (force msg_)
                        (when-let [err ?err]
                          (str "\n" (timbre/stacktrace err opts)))
                        ))))
  :ns-blacklist   ["org.apache.http.wire"
                   "org.eclipse.jetty.*"]
  :middleware     [(ns-patterns-mw
                    {
                     "hypercrud.*"           :warn
                     "com.amazonaws.*"       :info
                     "datomic.*"             :warn
                     "hyperfiddle.state"     :info
                     "io.netty.*"            :info
                     "io.pedestal.*"         :info
                     "io.pedestal.http.cors" :warn
                     "org.apache.activemq"   :error ; prevent connection death spam from sleeping dev computer
                     "org.apache.http.*"     :info
                     ;; "org.eclipse.jetty.*" :info
                     "org.xnio.*"            :warn
                     "org.xnio"              :warn
                     "org.jboss.*"           :warn
                     })
                   (fn [data]
                     (cond
                       (:?msg-fmt data)        data
                       (and (:?err data)
                            (= (.getMessage (clojure.stacktrace/root-cause (:?err data)))
                               "Broken pipe")) nil
                       :else
                       (assoc data :msg_
                              (binding [*print-level* 4]
                                (str (some-> 'contrib.do/*scope resolve deref
                                             (interleave (repeat '>)) vec)
                                     "\n  "
                                     (reduce
                                      (fn [s val]
                                        (as-> val s'
                                          (if (string? s') s' (with-out-str (pprint s' #_144)))
                                          (cond (= s "")                               s'
                                                (.endsWith s "\n")                     (str s s')
                                                (or (string? val) (< (.length s') 80)) (str s " " s')
                                                :else                                  (str s "\n" s'))))
                                      "" (:vargs data)))))))
                   ]})
