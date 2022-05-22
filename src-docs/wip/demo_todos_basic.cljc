(ns wip.demo-todos-basic
  (:require clojure.edn
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros wip.demo-todos-basic)))

(def auto-inc (partial swap! (atom 0) inc))

(defn task-create [description]
  [{:db/id            (str "dustin-" (auto-inc))
    :task/description description
    :task/status      :active}])

(defn task-status [id status]
  [{:db/id       id
    :task/status status}])

(defn task-remove [id])                                     ; todo

(def !conn #?(:clj (d/create-conn {})))

(comment
  (d/transact !conn (task-create "buy milk"))
  (d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] @!conn 1)
  := :active)

(defn clear-input! [el v] (dom/set-property! el "value" "") v)

(p/def db)                                                  ; server

(p/defn Todo-list [basis-t]
  (dom/div
    (concat
      (dom/div
        (dom/text "TodoMVC")
        (dom/input
          (dom/attribute "type" "text")
          (z/impulse basis-t
            (->> (dom/events dom/parent "keyup")
                 (m/eduction
                   (filter (comp #{dom/keycode-enter} dom/keycode))
                   (map (comp task-create dom/target-value))
                   (map (partial clear-input! dom/parent)))))))
      (dom/div
        (apply concat
               (dom/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
                 (dom/div
                   (concat
                     (dom/input
                       (dom/attribute "type" "checkbox")
                       (dom/set-checked! dom/parent (#{:done} ~@(:task/status (d/entity db id))))
                       (z/impulse basis-t
                         (->> (dom/events dom/parent dom/input-event)
                              (m/eduction
                                (map (comp {false :active true :done} dom/get-checked dom/event-target))
                                (map (partial task-status id))))))
                     (dom/span (dom/text (str id "-" ~@(:task/description (d/entity db id))))))))))
      (dom/div
        (dom/span
          (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                         :where [?e :task/status ?status]]
                                       db :active)) " items left")))))))

(defn transact [tx-data] #?(:clj (do (prn `transact tx-data) (d/transact! !conn tx-data) nil)))

(p/defn App []
  (let [!t      (atom 0)
        basis-t (p/watch !t)]
    (prn `basis-t basis-t)
    ~@(binding [db (p/watch !conn)]
        ~@(if-some [tx (seq (Todo-list. basis-t))]
            (swap! !t + (do ~@(transact tx) 1))             ; auto-transact
            (prn :idle)))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _))))))

(comment (user/browser-main! `main))
