(ns dustin.y2022.loop.photon-cycle0-beginning
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-cycle0-beginning)))

(defmacro with-cycle [[s i] & body]
  `(let [a# (atom ~i) ~s (p/watch a#)]
     (reset! a# (do ~@body))))

(defmacro loop! [[anaphor seed] & body]
  `(let [a# (atom ~seed) ~anaphor (p/watch a#)]
     ; rewrite recur to reset
     (reset! a# (do ~@body))))

(:command
  (p/with-cycle [{:keys [status] :as state} {:status :idle}]
    (let [stage
          (p/server
            (p/with-cycle [stage []]
              (binding [db (:db-after (d/with db stage))]
                (p/client (Body. false))))) ;; TODO validation
          _ (dom/hr)
          commit (when (Button. "commit!" (not= :idle status)) stage) ;; TODO disable when invalid
          discard (when (Button. "discard" (not= :idle status)) [])]
      (case status
        :idle (if-some [command (or commit discard)]
                {:status :request
                 :command command}
                state)
        :request (assoc state :status :pending)
        :pending (if busy state {:status :idle})))))