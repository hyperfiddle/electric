(ns geoffrey.2023.top-level-events)


#_(defmacro continuous-event
    ([type] `(continuous-event* dom/node ~type nil))
    ([node type] `(continuous-event* ~node ~type nil))
    ([node type options] `(continuous-event* ~node ~type ~options)))

(defn continuous-event [node type options init f]
  (->> (m/observe (fn [!]
                    (let [callback (comp ! f)]
                      (.addEventListener node type callback (clj->js options))
                      #(.removeEventListener node type callback))))
    (m/reductions {} init)
    (m/relieve {})))

(e/def MouseCoordinates ; Should mousemove always be tracked at the top level?
  (new (continuous-event dom/node "mousemove" {:passive true} [0 0]
         (fn [event] [(.-clientX event) (.-clientY event)]))))
