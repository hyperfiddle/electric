(ns contrib.network)

(defn port-available?
  "Check if a `port` is available for `ip` interface."
  ([^String ip port]
   (let [socket-addr (try (java.net.InetSocketAddress. (java.net.InetAddress/getByName ip) port)
                          (catch java.io.IOException t (throw (ex-info "Invalid ip address" {:ip ip} t))))
         server-socket (java.net.ServerSocket.)]
     (try
       (doto server-socket
         (.setReuseAddress true)
         (.bind socket-addr))
       true
       (catch java.io.IOException _ false)
       (finally (.close server-socket))))))

(comment
  (port-available? "127.0.0.1" 8080) := true
  )

(defn find-open-port
  "Return the first open `port` for `ip` in the range [from .. to["
  [ip from to]
  (some (fn [port] (and (port-available? ip port) port)) (range from to)))

(comment
  (find-open-port "127.0.0.1" 8080 8083) := 8080
  )