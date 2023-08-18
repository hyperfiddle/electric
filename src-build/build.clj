(ns build
  "build electric.jar library artifact"
  (:require [clojure.tools.build.api :as b]
            [clojure.java.shell :as sh]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(def class-dir "target/classes")

(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))

(def defaults {:src-pom "src-build/pom-template.xml" :lib lib})

(defn clean-client [_] (b/delete {:path "resources/public/js"}))
(defn clean-server [_] (b/delete {:path "resources/private/electric/server_programs"}))

(defn clean [opts]
  (clean-client opts)
  (clean-server opts)
  (b/delete {:path "target"}))

(defn jar [{:keys [version] :or {version version}}]
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)
        opts (assoc defaults
               :version    version
               :basis      basis
               :class-dir  class-dir
               :jar-file   jar-file
               :scm        {:tag version}
               :src-dirs   ["src"])]
    (println "Writing pom.xml")
    (b/write-pom opts)
    (println "Copying resources to" class-dir)
    (b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
    (println "Building jar" jar-file)
    (b/jar opts)))

(defn install [{:keys [version] :or {version version}}]
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)]
    (b/install {:basis      basis
                :lib        lib
                :version    version
                :jar-file   jar-file
                :class-dir  class-dir})))

(defn deploy [opts]
  (let [{:keys [lib version class-dir installer jar-file] :as opts} (merge defaults opts)]
    (assert version ":version is required to deploy")
    (when (and installer (not= :remote installer))
      (println ":installer" installer "is deprecated -- use install task for local deployment"))
    (let [jar-file (or jar-file (format "target/%s-%s.jar" (name (or lib 'application)) version))]
      (dd/deploy (merge {:installer :remote :artifact (b/resolve-path jar-file)
                         :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                   opts)))))

;; Uberjar

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
  #_(doseq [[k v] args] (println (pr-str v) (pr-str (type v)))) ; clojure.main option type reader diagnostics
  ; https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L482-L516

  (println "Cleaning up before build")
  (clean nil)

  (build-client {:optimize optimize, :debug debug, :verbose verbose, :version version})

  (println "Bundling sources")
  (b/copy-dir {:src-dirs   ["src" "src-prod" "src-docs" "resources"]
               :target-dir class-dir})

  (println "Building uberjar")
  (b/uber {:class-dir class-dir
           :uber-file (or (str jar-name) ; defend against shell misquoting causing clj to read "app.jar" without quotes thus as symbol
                        (format "target/%s-%s-standalone.jar" "electric-demos" version))
           :basis     (b/create-basis {:project "deps.edn"
                                       :aliases [:prod]})}))
