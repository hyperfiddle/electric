(ns uberjar
  "Build an uberjar for demos"
  (:require [clojure.tools.build.api :as b]
            [clojure.java.shell :as sh]))

(def lib 'com.hyperfiddle/electric-demos)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(def class-dir "target/classes")
(defn default-jar-name [{:keys [version] :or {version version}}]
  (format "target/%s-%s-standalone.jar" (name lib) version))

(defn noop [_]) ; to preload mvn deps
(defn clean [_] (b/delete {:path "target"}))
(defn clean-cljs [_] (b/delete {:path "resources/public/js"}))

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

  (println "Cleaning cljs compiler output")
  ;; (clean-cljs nil)

  ;; (build-client {:optimize optimize, :debug debug, :verbose verbose, :version version})

  (println "Bundling sources")
  (b/copy-dir {:src-dirs   ["src" "src-prod" "src-dev" "src-docs" "resources"]
               :target-dir class-dir})

  (println "Building uberjar")
  (b/uber {:class-dir class-dir
           :uber-file (str (or jar-name (default-jar-name {:version version})))
           :basis     basis}))
