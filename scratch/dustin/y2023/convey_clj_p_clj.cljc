(ns user
  (:require [datalevin.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-jetty-adapter :as photon-jetty-adapter]
            [mount.core :refer [defstate]]
            [ring.adapter.jetty9 :as ring.jetty9]))

(def ^:dynamic *user* nil)
(p/def db)

(comment
  (defstate server
            :start
            (ring.jetty9/run-jetty
              handler
              {:port (get *command-line-args* :port default-port)
               :join? false
               :websockets
               {"/"
                (wrap-yam-session
                  (fn [ring-req]
                    (when-let [token (auth/token ring-req)]
                      (photon-jetty-adapter/photon-ws-adapter
                        (fn [write-msg read-msg]
                          (when-let [user* [:yam.user/github-token token]]
                            (binding [*user-ent* user*]
                              (photon-jetty-adapter/photon-ws-message-handler ring-req write-msg read-msg))))))))}})
            :stop (ring.jetty9/stop-server server)))

#?(:clj (defn with-user-ent-fn [user thunk]
          (binding [*user* user] ; rebind in Clojure
            (thunk))))

(defmacro binding-clj-user "convey user binding visible from photon, to clojure call"
  [& body]
  `(with-user-ent-fn
     *user* ; capture photon env, pass to clojure
     (fn [] ~@body)))

(defn foo-query [db] (d/q ... *user*)) ; clojure can see *user-ent*

(p/defn Page []
  (binding-clj-user
    (foo-query db)))

(p/defn App []
  (p/server
    (binding [db (p/watch (p/watch (d/create-conn ...)))]
      (Page.))))
