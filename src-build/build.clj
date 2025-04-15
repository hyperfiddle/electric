(ns build
  (:require [clojure.tools.build.api :as tools.build]
            [hyperfiddle.build :as build]))

;; Expose generic tasks
(def clean #'build/clean)
(def install #'build/install)
(def deploy #'build/deploy)

(defn build [opts] ; custom build task because of AOT
  (build/clean opts)
  (let [basis (partial build/create-basis :project "deps.edn", :extra "../electric-secret/deps.edn")
        {:keys [class-dir src-dirs] :as opts} (build/defaults (basis :aliases [:release]) opts)]
    (tools.build/write-pom opts)
    (tools.build/copy-dir {:src-dirs src-dirs, :target-dir class-dir})
    (tools.build/compile-clj {:basis (basis :aliases [:release :build-deps])
                              :class-dir class-dir
                              :ns-compile '[hyperfiddle.electric.impl.entrypoint hyperfiddle.electric.impl.auth hyperfiddle.electric.impl.jwt hyperfiddle.electric.impl.auth0 hyperfiddle.electric.shadow-cljs.hooks3]
                              :filter-nses '[hyperfiddle.electric.impl.entrypoint hyperfiddle.electric.impl.auth hyperfiddle.electric.impl.jwt hyperfiddle.electric.impl.auth0 hyperfiddle.electric.shadow-cljs.hooks3]})
    (tools.build/jar opts)))

