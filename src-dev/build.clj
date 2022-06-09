(ns build
  (:require [clojure.tools.build.api :as b]))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis (b/create-basis {:project "deps.edn"})
            :javac-opts ["-source" "8" "-target" "8"]}))
