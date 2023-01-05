(ns wip.todomvc_collect
  (:require clojure.edn
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros wip.todomvc_collect)))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(p/def db)                                                  ; server
(p/def next-id!)                                            ; client
(p/def basis-t)                                             ; client
(def !state #?(:cljs (atom {::filter :all})))               ; client

(defn query-todos [db filter]
  {:pre [filter]}
  (case filter
    :active (d/q '[:find [?e ...] :where [?e :task/status :active]] db)
    :done (d/q '[:find [?e ...] :where [?e :task/status :done]] db)
    :all (d/q '[:find [?e ...] :where [?e :task/status]] db)))

(defn todo-count [db filter]
  {:pre  [filter]
   :post [(number? %)]}
  (-> (case filter
        :active (d/q '[:find (count ?e) . :where [?e :task/status :active]] db)
        :done (d/q '[:find (count ?e) . :where [?e :task/status :done]] db)
        :all (d/q '[:find (count ?e) . :where [?e :task/status]] db))
      ; datascript can return nil wtf
      (or 0)))

(p/defn Filter-control [state target label]
  (dom/a (dom/class (if (= state target) "selected"))
         (dom/text label)
         (when (->> (dom/events dom/parent "click")
                    (p/impulse dom/system-time-ms))
           ; emit next filter value
           target)))

;(defmacro collect [& body] `(m/ap (m/amb=)))

(defn collect
  ([a b] (m/ap (m/amb= (m/?> a) (m/?> b))))
  ([a b c] (m/ap (m/amb= (m/?> a) (m/?> b) (m/?> c)))))

(p/defn TodoStats [state]                                   ; returns tx for cleared todos
  (let [active ~@(todo-count db :active)
        done ~@(todo-count db :done)
        #_#_[active done] (mapv (partial todo-count db) [:active :done])]
    (println `active active)
    (println `done done)
    (dom/div
      (dom/span
        (dom/class "todo-count")
        (dom/strong (dom/text active))
        (dom/span (dom/text (str " " (case active 1 "item" "items") " left"))))

      (concat                                               ; collect tx
        (when-let [filter' (dom/ul
                             (dom/class "filters")
                             (->>
                               (collect
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :all "All")))
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :active "Active")))
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :done "Completed"))))
                               (m/relieve {})
                               new))]
          ; careful to dedupe state, this can infinite loop otherwise
          (swap! !state assoc ::filter filter') nil)        ; don't accidentally emit a tx

        (when (pos? done)
          (dom/button
            (dom/class "clear-completed")
            (dom/text (str "Clear completed " done))
            (when (->> (dom/events dom/parent "click")
                       (p/impulse basis-t))                 ; bug - stays up too long if more todos complete
              (p/for [id ~@(query-todos db :done)]
                {:db/id id :task/status :done}))))))))

(p/defn TodoItem [id]
  ~@(let [x #_{:keys [task/status
                      task/description]} (d/entity db id)
          status                         (:task/status x)
          description                    (:task/description x)]
      ~@(dom/li
          (dom/class (case status :done "completed" :active "editing"))
          (concat
            (dom/input
              (dom/attribute "type" "checkbox")
              (dom/set-checked! dom/parent (#{:done} status))
              (when-let [status' (->> (dom/events dom/parent dom/input-event)
                                      (m/eduction (map (comp {false :active true :done}
                                                             dom/get-checked
                                                             dom/event-target)))
                                      (p/impulse basis-t))]
                [{:db/id id :task/status status'}]))
            (dom/span (dom/text (str description)))))))

(p/defn TodoList [state]
  (let [active ~@(todo-count db :active)]
    (dom/section
      (dom/class "main")
      (->>
        (collect
          (p/fn []
            (dom/input
              (dom/class "toggle-all")
              (dom/attribute "type" "checkbox")
              (dom/set-checked! dom/parent (zero? active))
              (when (->> (dom/events dom/parent dom/input-event)
                         (p/impulse basis-t))
                ; Toggle all to done, unless everything is already done, in which case toggle all to active.
                (let [status' (case (pos? active) :done :active)]
                  (p/for [id ~@(query-todos db :active)]
                    {:db/id id :task/status status'})))))
          (p/fn []
            (dom/ul
              (dom/class "todo-list")
              (apply concat                                 ; merge txes
                     (dom/for [id ~@(query-todos db (::filter state))]
                              (TodoItem. id)))))
          ) (m/relieve {}) new))))

(p/defn CreateTodo []
  (dom/input
    (dom/attribute "type" "text")
    (dom/attribute "placeholder" "What needs to be done?")
    (when-let [s (->> (dom/events dom/parent "keyup")
                      (m/eduction
                        (filter (comp #{dom/keycode-enter} dom/keycode))
                        (map dom/target-value))
                      (p/impulse basis-t))]
      (dom/set-property! dom/parent "value" "")
      [{:task/description s
        :task/status      :active
        :db/id            (str ::tempid "-" (next-id!))}])))

(p/defn TodoMVC "returns transactions" [state]
  (dom/div
    (concat
      (dom/section
        (dom/class "todoapp")
        (dom/header
          (dom/class "header")
          (dom/h1 (dom/text "TodoMVC"))
          (CreateTodo.)))

      (when ~@(pos? (todo-count db :all))
        (TodoList. state)
        (dom/footer
          (dom/class "footer")
          #_(dom/text "hi")                                 ; crashes if empty footer
          (TodoStats. state)))

      (dom/footer
        (dom/class "info")
        (dom/p (dom/text "Double-click to edit a todo")))

      (dom/div
        (dom/dl
          (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str ~@(todo-count db :all))))
          (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str ~@(query-todos db :all))))
          (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state))))))))

(defn transact [tx] #?(:clj (do (prn `transact tx) (d/transact! !conn tx) nil)))

(p/defn App []
  (let [!t    (atom 0)
        state (->> (m/watch !state) (m/eduction (dedupe)) (m/relieve {}) new)] ; prevent infinite loop on setting same filter
    (binding [basis-t  (p/watch !t)                         ; basis-t is used to ack completed transact to controls
              next-id! (partial swap! (atom 0) inc)]
      (prn `basis-t basis-t)
      ~@(binding [db (p/watch !conn)]
          ~@(if-some [tx (seq (doto (TodoMVC. state) (println `tx)))]
              (swap! !t + (do ~@(transact tx) 1))           ; auto-transact
              (prn :idle))))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _))))))

(comment
  (user/browser-main! `main)
  (todo-count @!conn :all)
  (todo-count @!conn :active)
  (todo-count @!conn :done)
  (query-todos @!conn :all)
  (query-todos @!conn :active)
  (query-todos @!conn :done)
  (d/q '[:find (count ?e) . :where [?e :task/status]] @!conn)
  )
