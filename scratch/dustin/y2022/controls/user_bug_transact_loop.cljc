(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(p/defn Triangle [{:as task :keys [db/id] ui-state :task.ui/state}]
  (p/client
    (let [ui-state ui-state]
      (dom/div {:class if (:expanded? ui-state) "expanded"})
      (ui/element
        dom/div
        {::ui/click-event (p/fn [_]
                            (p/server
                              (db/transact! [{:db/id id :task.ui/state (update ui-state :expanded? not)}])
                              nil))}))))

(p/server (Triangle. (d/entity db (:db/id task-id))))
