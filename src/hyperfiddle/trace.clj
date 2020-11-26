(ns hyperfiddle.trace
  (:require [hyperfiddle.hxclj :as hx])
  (:import hyperfiddle.View
           hyperfiddle.Maybe))

(defn- node-type [node]
  (keyword (aget hyperfiddle.NodeDef/__hx_constructs (.. node -def -index))))

(defn- node-data [node]
  {:type   (node-type node)
   :ended  (.-ended node)
   :frame  (.-frame node)
   :id     (.-id node)
   :name   (.-name node)
   :queued (.-queued node)
   :rank   (.-rank node)
   :val    (.-val node)
   :ok     (.ok node)})

(defn datafy
  "Given a Dataflow View node (like `(.-node >v)`), return a map description.
  Recursively walks upward (towards inputs)."
  [node]
  (when node
    (cond-> (node-data node)
      (.-on node) (assoc :on (mapv datafy (hx/hx->clj (.-on node)))))))

(def nodes (partial tree-seq :on :on))

(defn- unwrap [val]
  (condp = (type val)
    Maybe (some-> (.-params ^Maybe val) (aget 0))))

(defn trace [^View view]
  (->> (.-node view)
       (datafy)
       (nodes)
       (filter :name)
       (map (fn [{:keys [name val]}]
              [name (unwrap val)]))
       (reverse)))
