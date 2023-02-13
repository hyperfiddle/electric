(ns repl
  (:require
    [clojure.tools.namespace.reload :as reload]
    [clojure.tools.namespace.repl :as repl]
    [clojure.tools.namespace.track :as track]))

(def ignored-nss
  [#"^scratch\..*"])

(defn ignored? [nss ns]
  (boolean (some #(re-find % (name ns)) nss)))

(declare refresh!)

(defn track-reload-one
  "Executes the next pending unload/reload operation in the dependency
  tracker. Returns the updated dependency tracker. If reloading caused
  an error, it is captured as ::error and the namespace which caused
  the error is ::error-ns."
  [tracker]
  (let [{unload ::track/unload,
         load   ::track/load} tracker]
    (cond
      (seq unload)
      (let [n (first unload)]
        (when-not (ignored? ignored-nss (name n))
          (reload/remove-lib n))
        (update-in tracker [::track/unload] rest))
      (seq load)
      (let [n (first load)]
        (try
          (when-not (ignored? ignored-nss (name n))
            (require n :reload))
          (update-in tracker [::track/load] rest)
          (catch Throwable t
            (println "Error loading" n "." (ex-message t) (ex-message (ex-cause t)))
            (when (= 'dev n)
              (intern (create-ns 'dev) 'refresh! refresh!))
            (update-in tracker [::track/load] rest))))
      :else
      tracker)))

(defn refresh! []
  (with-redefs [reload/track-reload-one track-reload-one]
    (repl/refresh)))

(defn refresh-all! []
  (with-redefs [reload/track-reload-one track-reload-one]
    (repl/refresh-all)))
