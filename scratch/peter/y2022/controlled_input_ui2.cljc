(ns user.controlled-input-ui2
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom2 :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.controlled-input-ui2)))

(def latency-ms 2000)
(defn latency [v] (p/task->cp (m/sp (m/? (m/sleep latency-ms v)))))
#?(:clj (defn q [db] (:x db)))
#?(:clj (def !db (atom {:x "0"})))

(p/defn App []
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
