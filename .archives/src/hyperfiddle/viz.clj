(ns hyperfiddle.viz
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [dorothy.core :as dot]
            [dorothy.jvm :refer [save!]]
            [hyperfiddle.trace :as trace])
  (:import hyperfiddle.View
           java.io.File))

(defn- links
  ([ast] (into [] (links nil [] ast)))
  ([prev acc ast]
   (if-not (:on ast)
     [[(:id ast) prev]]
     (cond->> (:on ast)
       true         (mapcat (fn [node'] (links (:id ast) acc node')))
       (some? prev) (cons [(:id ast) prev])))))

(defn default-node-renderer [{:keys [id name type frame rank ok ended queued val]}]
  {:label (cond-> (str (clojure.core/name type))
            name       (str " " name "\n")
            (not name) (str " #" id "\n")
            true       (str "frame " frame ", rank " rank "\n")
            ended      (str " ended")
            queued     (str " queued")
            true       (str val))
   :color (if ok :green :red)})

(defn- graph
  ([^View >v] (graph default-node-renderer >v))
  ([rendererf, ^View >v]
   (let [ast (trace/datafy (.-node >v))]
     (dot/digraph (->> (mapv (juxt :id rendererf) (trace/nodes ast))
                       (into (links ast)))))))

(defn view->digraph
  ([file-path, ^View >v] (view->digraph default-node-renderer file-path >v))
  ([rendererf, file-path, ^View >v]
   (-> (graph rendererf >v)
       (dot/dot)
       (save! file-path {:format :png}))))

(defn- delete-files-recursively
  "https://gist.github.com/edw/5128978"
  [fname & [silently]]
  (letfn [(delete-f [file]
            (when (.isDirectory file)
              (doseq [child-file (.listFiles file)]
                (delete-f child-file)))
            (io/delete-file file silently))]
    (delete-f (io/file fname))))

(defn animation [file-path]
  (let [n       (atom 0)
        ^File f (io/file file-path)]
    (when (.exists f) (delete-files-recursively file-path)) ;; if he dies, he dies.
    (.mkdir f)
    [(fn step [^View >v]
       (view->digraph (str file-path (File/separator) @n) >v)
       (swap! n inc))
     (fn end []
       (let [end-path (str file-path ".gif")]
         (when (.exists (io/file end-path)) (io/delete-file end-path))
         (shell/sh "convert" "-dispose" "previous" "-delay" "250" "-loop" "0" "-resize" "800x800" (str file-path "/*") end-path)))]))

(defmacro capture-gif [file-path >out & puts]
  (let [step-sym (gensym "step")
        stepf    `(~step-sym ~>out)
        body     (cons stepf (interleave puts (repeat stepf)))]
    `(let [[~step-sym end#] (animation ~file-path)]
       ~@body
       (end#))))
