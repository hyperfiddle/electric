(ns build
  "build electric.jar library artifact and demos"
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            [shadow.cljs.devtools.api :as shadow-api] ; so as not to shell out to NPM for shadow
            [shadow.cljs.devtools.server :as shadow-server]
            ))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

;;; Library

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

;;; Uberjar

(def class-dir "target/classes")
(defn default-jar-name [{:keys [version] :or {version version}}]
  (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean-cljs [_]
  (b/delete {:path "resources/public/js"}))

(defn build-client [{:keys [optimize debug verbose version]
                     :or {optimize true, debug false, verbose false, version version}}]
  (println "Building client. Version:" version)
  (shadow-server/start!)
  (shadow-api/release :prod {:debug debug,
                             :verbose verbose,
                             :config-merge [{:compiler-options {:optimizations (if optimize :advanced :simple)}
                                             :closure-defines {'hyperfiddle.electric-client/VERSION version}}]})
  (shadow-server/stop!))

(defn uberjar [{:keys [jar-name version optimize debug verbose]
                :or   {version version, optimize true, debug false, verbose false}}]
  (println "Cleaning up before build")
  (clean nil)

  (println "Cleaning cljs compiler output")
  (clean-cljs nil)

  (build-client {:optimize optimize, :debug debug, :verbose verbose, :version version})

  (println "Bundling sources")
  (b/copy-dir {:src-dirs   ["src" "src-dev" "src-docs" "resources"]
               :target-dir class-dir})

  (println "Compiling server. Version:" version)
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src" "src-dev" "src-docs"]
                  :ns-compile '[user]
                  :class-dir  class-dir})

  (println "Building uberjar")
  (b/uber {:class-dir class-dir
           :uber-file (str (or jar-name (default-jar-name {:version version})))
           :basis     basis
           :main      'user}))


(defn noop [_])                         ; run to preload mvn deps
