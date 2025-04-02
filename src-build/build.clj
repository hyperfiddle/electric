(ns build
  "build electric.jar library artifact"
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis     (b/create-basis {:project "deps.edn", :extra "../electric-secret/deps.edn"}))
(def aot-basis (b/create-basis {:project "deps.edn", :extra "../electric-secret/deps.edn", :aliases [:build-deps]}))

(def class-dir "target/classes")

(def defaults {:src-pom "src-build/pom-template.xml" :lib lib :class-dir class-dir})

(defn clean [opts] (b/delete {:path "target"}))

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
    (b/copy-dir {:src-dirs ["src"], :target-dir class-dir})
    (b/compile-clj {:basis aot-basis
                    :class-dir class-dir
                    :ns-compile '[hyperfiddle.electric.impl.entrypoint hyperfiddle.electric.impl.auth hyperfiddle.electric.impl.jwt hyperfiddle.electric.impl.auth0 hyperfiddle.electric.shadow-cljs.hooks3]
                    :filter-nses '[hyperfiddle.electric.impl.entrypoint hyperfiddle.electric.impl.auth hyperfiddle.electric.impl.jwt hyperfiddle.electric.impl.auth0 hyperfiddle.electric.shadow-cljs.hooks3]})
    (println "Building jar" jar-file)
    (b/jar opts)))

(defn install [{:keys [version] :or {version version}}]
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)]
    (b/install {:basis      basis
                :lib        lib
                :version    version
                :jar-file   jar-file
                :class-dir  class-dir})))

(defn deploy [opts] ; clojars
  (let [{:keys [lib version class-dir installer jar-file] :as opts} (merge defaults opts)]
    (assert version ":version is required to deploy")
    (when (and installer (not= :remote installer))
      (println ":installer" installer "is deprecated -- use install task for local deployment"))
    (let [jar-file (or jar-file (format "target/%s-%s.jar" (name (or lib 'application)) version))]
      (dd/deploy (merge {:installer :remote :artifact (b/resolve-path jar-file)
                         :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                   opts)))))

;; For reference
#_
(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))
