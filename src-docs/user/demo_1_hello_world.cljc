(ns user.demo-1-hello-world
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros user.demo-1-hello-world)))

(p/defn App []
  (p/client
    (dom/h1 "Hello World")
    (dom/div "Hello from server, where JVM number type is: "
      (dom/code (p/server (pr-str (type 1)))))
    (dom/div "Hello from client, where JS number type is: "
      (dom/code (p/client (pr-str (type 1)))))))
