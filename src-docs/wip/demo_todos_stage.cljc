(ns wip.demo-todos-stage
  (:require clojure.edn
            clojure.pprint
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros wip.demo-todos-stage)))

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

(defn clear-input! [el v] (dom/set-properties! el {:value ""}) v)

(p/def db)                                                  ; server

(p/defn Todo-list [basis-t]
  (dom/div
    (dom/h1 (dom/text "Todo list with staging area pattern"))
    (concat
      (dom/input {:type "text"}
        (->> (dom/events dom/parent "keyup")
             (m/eduction
               (filter (comp #{dom/keycode-enter} (dom/getter ["keyCode"])))
               (map (comp task-create dom/target-value))
               (map (partial clear-input! dom/parent)))
             (z/impulse basis-t)))
      (dom/div
        (apply concat
               (dom/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
                 (dom/div
                   (concat
                     (dom/input {:type "checkbox"
                                 :checked (#{:done} ~@(:task/status (d/entity db id)))}
                       (->> (dom/>events "input"
                                         (comp
                                          (map (comp {false :active true :done} (dom/getter ["target" "checked"])))
                                          (map (partial task-status id))))
                            (z/impulse basis-t)))
                     (dom/span (dom/text (str ~@(:task/description (d/entity db id)) " - id: " id))))))))
      (dom/p
        (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                       :where [?e :task/status ?status]]
                                     db :active)) " items left"))))))

(def auto-inc-2 (let [!x (atom 0)] (fn [stage]
                                     (println ::auto-inc stage @!x)
                                     (swap! !x inc))))

(def !stage #?(:cljs (atom nil)))                           ; we choose stage on client (not shared)

(defn write-edn [edn] (with-out-str (clojure.pprint/pprint edn))) ; no cc/fn in photon yet

(p/defn App []
  (let [stage (new (m/eduction (dedupe) (m/watch !stage)))]
    ~@(let [tx-report  (d/with (p/watch !conn) stage)
            current-tx (-> tx-report :tempids :db/current-tx)]
        (println ::tx-report tx-report)
        (binding [db (:db-after tx-report)]
          ~@(let [basis-t (auto-inc-2 stage)
                  tx      (seq (Todo-list. basis-t))]
              (when tx
                (js/console.log ::tx-data tx)
                (swap! !stage concat tx)))))

    (dom/p
      (when-some [event (dom/button {:type "button"}
                          (dom/text "transact!")
                          (->> (dom/events dom/parent "click")
                               (z/impulse z/clock)))]
        (println ::transact! event)
        ~@(do (d/transact! !conn stage) nil)                ; todo wait for server ack to clear stage
        (reset! !stage []))

      (if-some [tx (dom/input {:type "text"
                               :value (write-edn stage)}
                     (->> (dom/events dom/parent "input")
                          (m/eduction (map dom/target-value) (dedupe))
                          (z/impulse z/clock)))]
        (do
          (js/console.log ::stage tx)
          (reset! !stage (clojure.edn/read-string tx)))
        (do (js/console.log ::stage ::idle) nil)))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _))))))

(comment (user/browser-main! `main))

; button
; to transact
; clear staging area
; get new time basis
; Does d/with and d/transact return the same time basis for the same log value
; touch query with latest time basis
