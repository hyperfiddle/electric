(ns build
  "build electric.jar library artifact"
  (:require [clojure.data.xml :as xml]
            [clojure.tools.build.api :as b]
            [clojure.tools.build.tasks.write-pom :as wp]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.hyperfiddle/electric)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(defn alias-deps [basis alias] (get-in basis [:aliases alias :extra-deps]))

(defn extend-basis [basis deps]
  (b/create-basis (update (:basis-config basis) :extra merge {:deps deps})))

(def basis-with-cljdoc-provided-deps (extend-basis basis (alias-deps basis :cljdoc-extra-deps)))

(def class-dir "target/classes")

(def defaults {:src-pom "src-build/pom-template.xml" :lib lib :class-dir class-dir})

(defn clean [opts] (b/delete {:path "target"}))

(xml/alias-uri 'pom "http://maven.apache.org/POM/4.0.0")

(let [original @#'wp/to-dep]
  (defn to-dep ; Add <scope> support for tools.build POM <dependency> generation. For cljdoc extra deps.
    [[lib {:keys [pom/scope] :as coord}]]
    (cond-> (original [lib coord])
      scope (conj [::pom/scope "provided"]))))

(defn jar [{:keys [version] :or {version version}}]
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)
        opts (assoc defaults
               :version    version
               :basis      basis-with-cljdoc-provided-deps
               :class-dir  class-dir
               :jar-file   jar-file
               :scm        {:tag version}
               :src-dirs   ["src"])]
    (println "Writing pom.xml")
    (with-redefs [wp/to-dep to-dep]
      (b/write-pom opts))
    (println "Copying resources to" class-dir)
    (b/copy-dir {:src-dirs ["src"], :target-dir class-dir})
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

;; For reference
#_
(defn compile-java [_]
  (b/javac {:src-dirs ["src"]
            :class-dir "src"
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))

