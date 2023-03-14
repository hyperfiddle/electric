(ns build
  "build electric.jar library artifact"
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            [clojure.java.shell :as sh]))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))

(def defaults {:src-pom "pom.xml" :lib lib})

(defn clean-client [_] (b/delete {:path "resources/public/js"}))
(defn clean-server [_] (b/delete {:path "resources/private/electric/server_programs"}))

(defn clean [opts]
  (clean-client opts)
  (clean-server opts)
  (bb/clean opts))

(defn jar [opts]
  (bb/jar (merge defaults opts)))

(defn install [opts]
  (bb/install (merge defaults opts)))

(defn deploy [opts]
  (bb/deploy (merge defaults opts)))

;; Uberjar

(def class-dir "target/classes")
(defn default-uberjar-name [{:keys [lib version] :or {version version}}]
  (format "target/%s-%s-standalone.jar" "electric-demos" version))

(defn noop [_]) ; to preload mvn deps

(defn build-client [{:keys [optimize debug verbose version]
                     :or   {optimize true, debug false, verbose false, version version}}]
  (println "Building client. Version:" version)
  (let [command (->> ["clj" "-M:prod:shadow-cljs" "release" "prod"
                      (when debug "--debug")
                      (when verbose "--verbose")
                      "--config-merge"
                      (pr-str {:compiler-options {:optimizations (if optimize :advanced :simple)}
                               :closure-defines  {'hyperfiddle.electric-client/VERSION version}})]
                  (remove nil?))]
    (apply println "Running:" command)
    (let [{:keys [exit out err]} (apply sh/sh command)]
      (when-not (zero? exit)  (println "Exit code" exit))
      (when err (println err))
      (when out (println out)))))


(defn uberjar [{:keys [jar-name version optimize debug verbose]
                :or   {version version, optimize true, debug false, verbose false}}]
  (println "Cleaning up before build")
  (clean nil)

  (build-client {:optimize optimize, :debug debug, :verbose verbose, :version version})

  (println "Bundling sources")
  (b/copy-dir {:src-dirs   ["src" "src-prod" "src-docs" "resources"]
               :target-dir class-dir})

  (println "Building uberjar")
  (b/uber {:class-dir class-dir
           :uber-file (str (or jar-name (default-uberjar-name {:version version})))
           :basis     (b/create-basis {:project "deps.edn"
                                       :aliases [:prod]})}))
