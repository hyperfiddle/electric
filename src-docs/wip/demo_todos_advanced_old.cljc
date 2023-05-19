(ns wip.demo-todos-advanced-old
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled])
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [contrib.debug :as dbg]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(comment (alter-var-root #'!conn (fn [_] (d/create-conn {}))))
(e/def db) ; injected database ref; Electric defs are always dynamic

;; auto-incrementing task id, to define ordering
;; an optimistically rendered task won't jump on the screen
(defonce !order-id #?(:clj (atom 0) :cljs nil))

;; user configurable latency and tx fail rate
#?(:clj (def !latency (atom 200)))
(e/def latency (e/server (e/watch !latency)))

#?(:clj (def !fail-rate (atom 1)))
(e/def fail-rate (e/server (e/watch !fail-rate)))

;; tx with configured latency and fail rate
#?(:clj (defn tx! [tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "tx failed" {:tx tx}))
              (d/transact! !conn tx)))))

(e/def Tx!)

(e/defn Latency [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Latency: " latency "ms"))
    (ui/range latency (e/fn [v] (e/server (reset! !latency v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

(e/defn FailRate [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Fail Rate: " fail-rate " out of " max))
    (ui/range fail-rate (e/fn [v] (e/server (reset! !fail-rate v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

(defn ->task [desc] {:task/description desc, :task/status :active, :task/order (swap! !order-id inc)})

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description :task/order]) ...] :where [?e :task/status]] db)
            (sort-by :task/order #(compare %2 %1)))))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)
          server-checked? (= :done status)]
      (e/client
        (dom/div
          ;; TODO a failed tick won't revert, is that expected/good?
          (ui/checkbox server-checked?
            (e/fn [checked?]
              (e/server (new Tx! [[:db/add id :task/status (if checked? :done :active)]])))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [db (e/watch !conn), Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
      (e/client
        (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (Latency. 0 2000)
        (FailRate. 0 10)
        (dom/div (dom/props {:class "todo-list"})
          ;; we have to tuck away the input node because we'll be mounting nodes under a <ul>
          (let [in (dom/input (dom/props {:placeholder "Buy milk"}) dom/node)]
            (dom/div {:class "todo-items"}
              ;; client-side concurrent optimistically rendered list of tasks
              (e/for-event [v (e/listen> in "keydown" (partial ui/?read-line! in))]
                (dom/div
                  (let [!err (atom nil), err (e/watch !err)]
                    (if-not err              ; base case, go and try to transact
                      (try (reduced (e/server (new Tx! [(->task v)])))
                           (catch Pending _ (dom/text "⌛ " v)) ; render spinner while waiting
                           (catch Cancelled _) ; work around electric bug
                           (catch :default e (reset! !err e))) ; tx failed, trigger other branch
                      ;; tx failed, render failed value with retry button
                      (do (dom/text "💀 " v)
                          (ui/button (e/fn [] (reset! !err nil))
                            (dom/text "⟳"))
                          (dom/text " (" (ex-message err) ")"))))))
              ;; server-side query rendered commited list of tasks
              (e/server
                (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                  (TodoItem. id)))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))

;; alternative syntax for optimistic rendering
(comment
  (render-optimistically
    :ok (e/fn [v] (e/server (new Tx! [(->task v)])))
    :pending (e/fn [v] (dom/div (dom/text "⌛ " v)))
    :failed (e/fn [v ex retry!]
              (.error js/console ex)
              (dom/div
                (dom/text "💀 " v)
                (ui/button (e/fn [] (retry!))
                  (dom/text "⟳"))))))

;; gotchas
;; - in failed state moving a range picker floods more errors in the console
;; - checkbox failure doesn't revert, is that correct/good?
