(ns build
  "build electric.jar library artifact"
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))

(def defaults {:src-pom "pom.xml" :lib lib})

(defn clean [opts]
  (bb/clean opts))

(defn jar [opts]
  (bb/jar (merge defaults opts)))

(defn install [opts]
  (bb/install (merge defaults opts)))

(defn deploy [opts]
  (bb/deploy (merge defaults opts)))

