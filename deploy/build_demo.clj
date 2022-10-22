(ns build-demo
  (:require clojure.edn
            [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            shadow.cljs.devtools.api
            uberdeps.api))

(defn compile-js [& [opts]]
  (shadow.cljs.devtools.api/compile :devkit))

(defn uberjar [& [opts]]
  (uberdeps.api/package
    (clojure.edn/read-string (slurp "deps.edn"))
    "target/photon.jar"
    {}))

(defn build [& [opts]]
  (do (compile-js)
      (uberjar)))