(ns hyperfiddle.viz
  (:require [clojure.java.shell :as io]
            [dorothy.core :as dot]
            [dorothy.jvm :refer [save!]]
            [hyperfiddle.fabric :as f])
  (:import hyperfiddle.View))

(defn- node-type [node]
  (keyword (aget hyperfiddle.NodeDef/__hx_constructs (.. node -def -index))))

(defn- node-data [node]
  {:type   (node-type node)
   :ended  (.-ended node)
   :frame  (.-frame node)
   :id     (.-id node)
   :queued (.-queued node)
   :rank   (.-rank node)
   :val    (.-val node)
   :ok     (.ok node)})

(defn- datafy
  "Given a Dataflow View node (like `(.-node >v)`), return a map description.
  Recursively walks upward (towards inputs)."
  [node]
  (when node
    (cond-> (node-data node)
      (.-on node) (assoc :on (mapv datafy (f/hx->clj (.-on node)))))))

(def ^:private nodes (partial tree-seq :on :on))

(defn- links
  ([ast] (into [] (links nil [] ast)))
  ([prev acc ast]
   (if-not (:on ast)
     [[(:id ast) prev]]
     (cond->> (:on ast)
       true         (mapcat (fn [node'] (links (:id ast) acc node')))
       (some? prev) (cons [(:id ast) prev])))))

(defn default-node-renderer [{:keys [id type frame rank ok ended queued val]}]
  {:label (cond-> (str (name type) " #" id "\n")
            true   (str "frame " frame ", rank " rank "\n")
            ended  (str " ended")
            queued (str " queued")
            true   (str val))
   :color (if ok :green :red)})

(defn- graph
  ([^View >v] (graph default-node-renderer >v))
  ([rendererf, ^View >v]
   (let [ast (datafy (.-node >v))]
     (dot/digraph (->> (mapv (juxt :id rendererf) (nodes ast))
                       (into (links ast)))))))

(defn view->digraph
  ([file-path, ^View >v] (view->digraph default-node-renderer file-path >v))
  ([rendererf, file-path, ^View >v]
   (-> (graph rendererf >v)
       (dot/dot)
       (save! file-path {:format :png}))))

(defn animation [file-path]
  (let [n (atom 0)]
    [(fn step [^View >v]
       (view->digraph (str file-path "__" @n) >v)
       (swap! n inc))
     (fn end []
       (io/sh "convert" "-delay" "200" "-loop" "0" (str file-path "*") (str file-path ".gif")))]))
