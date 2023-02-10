(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis (b/create-basis {:project "deps.edn"})
            :javac-opts ["-source" "8" "-target" "8"]}))

(def defaults {:src-pom "pom.xml" :lib 'com.hyperfiddle/electric})

(defn clean [opts]
  (bb/clean opts))

(defn jar [opts]
  (bb/jar (merge defaults opts)))

(defn install [opts]
  (bb/install (merge defaults opts)))

(defn deploy [opts]
  (bb/deploy (merge defaults opts)))
