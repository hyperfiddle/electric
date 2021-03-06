(ns leo.todomvc.actual-dom
  (:require [leo.hfdl :refer [ifn $]]
            [missionary.core :as m]))

(defn row [])

(defn checkbox []
  (doto (.createElement js/document "input")
    (.setAttribute "type" "checkbox")))

(defn label []
  (.createElement js/document "span"))

(defn input-text []
  (doto (.createElement js/document "input")
    (.setAttribute "type" "text")))

(def button
  (ifn [parent]))

(defn mount [item parent]
  (m/observe
    (fn [_]
      (.appendChild parent item)
      #(.removeChild parent item))))

(defn events [item type]
  (m/observe
    (fn [!]
      (.addEventListener item type !)
      #(.addEventListener item type !))))

(defn set-attr [item k v]
  (->> v
    (m/latest (fn [x] (.setAttribute item k x)))
    (m/relieve {})))

(defn get-attr [item k]

  )

(def editable-text
  (ifn [parent text]
    (let [!editing? (atom false)
          e @(m/watch !editing?)
          l (label)
          i (input-text)]
      @(mount l (when-not e parent))
      @(mount i (when e parent))
      @(->> (events l "dblclick")
         (m/relieve (fn [_ _] (swap! !editing? not))))
      @(set-attr l "text" text)
      @(->> (events i "keydown")
         (m/transform (filter enter?))
         (m/integrate {} nil)
         (m/sample (fn [x _] (swap! !editing? not) x) (get-attr i "value"))
         (m/relieve {})))))

(def todo-item
  (ifn [parent {:keys [id done description]}]
    (let [r (row)]
      @(mount r parent)
      {:done?       (let [cb (checkbox)]
                      @(mount cb r)
                      @(set-attr cb "value" (extend done))
                      @(get-attr cb "value"))
       :description ($ editable-text r description)
       :removed?    ($ button r)}
      )))

(def todo-list
  (ifn [parent todos]
    (let [t (table)]
      @(mount t parent)
      (rfor [todo :db/id todos]
        ($ todo-item t todo)))))

(defn main []
  (let [!state (atom [])]
    (run-dag
      (let [todos ($ todo-list (.-body js/document) @(m/watch !state))]
        (reset! !state todos)))))