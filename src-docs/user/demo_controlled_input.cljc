(ns user.demo-controlled-input
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [peter.y2022.dom-pure :as dp]
   [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.demo-controlled-input)))

(def latency-ms 300)
(defn latency [v] (p/task->cp (m/sp (m/? (m/sleep latency-ms v)))))
#?(:clj (defn q [db] (:x db)))
#?(:clj (def !db (atom {:x "0"})))

(p/defn App* []
  (p/client
    (dom/div
      (p/server
        (p/with-cycle [pending false]
          (try
            (let [db (p/watch !db)]
              (reset! !db
                (new (latency (p/client
                                (let [v (ui/input (p/server (q db))
                                          {:style {:background-color (if pending "yellow" "white")}})]
                                  (dom/div (dom/text "server value: " (:x db)))
                                  {:x v}))))))
            false
            (catch Pending _ true)))
        nil))))

#?(:cljs (defn tvalue [e] (.. e -target -value)))

(p/defn Input [server-value F]
  (let [in (dom/dom-element dom/node "input")
        [user-value pending] (dp/reify-pending
                               (dp/flow-case server-value
                                 (dp/event* in "input") (let [v (tvalue dp/it)] (when F (F. v)) v)))
        focused? (dp/flow-case false (dp/event* in "focus") true (dp/event* in "blur") false)]
    (dom/with in
      (.setAttribute dom/node "type" "text")
      (set! (.-value dom/node) server-value)
      (when pending (dom/props {:style {:background-color "yellow"}}))
      (if focused? user-value server-value))))

(p/defn App []
  (p/client
    (dom/div
      (p/server
        (let [db (p/watch !db)]
          (p/client
            (let [server-value (p/server (q db))
                  ret (Input. server-value (p/fn [v] (p/server (swap! !db assoc :x (new (latency v))))))]
              (prn "retval:" ret)
              (dom/div (dom/text "server value: " server-value))))
          nil)))))

(comment
  ;; evaluate to add a concurrent user modifying the db value
  #?(:clj (def concurrent-user ((m/sp (loop []
                                        (m/? (m/sleep (+ 1000 (rand-int 4000))))
                                        (swap! !db update :x (fn [v] (if (zero? (rand-int 2))
                                                                       (str v "#")
                                                                       (apply str (butlast v)))))
                                        (recur)))
                                identity identity)))
  ;; evaluate to stop the user
  #?(:clj (concurrent-user))
  )
