(ns dustin.y2022.popover1
  (:require #?(:clj [datomic.api :as d])
            #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.popover1)))

(p/def db)

(p/defn Popover [BtnLabel Body busy]
  (:request
    (p/with-cycle [{:keys [status]} {:status :closed}]
      (let [; popover anchor
            toggle (Button. (BtnLabel. (not= :closed status)) (= :pending status))
            ; popover body
            request (case status
                      :closed nil
                      (dom/div {:style {:position "fixed"}}
                        (dom/div {:style {:border "1px pink solid" :padding "5px"
                                          :position "relative" :left "3em" :top "2em" :z-index "1"
                                          :width "50em" :height "40em"
                                          :background-color "rgb(248 250 252)"}}
                          (p/server
                            (let [!stage (atom []), stage (p/watch !stage)] ; fork
                              (binding [db (:db-after (d/with db stage))]
                                (p/client
                                  (p/with-cycle [busy false]
                                    (let [tx (Body. busy)]
                                      (try (p/server (reset! !stage tx) false)
                                           (catch Pending _ true))))
                                  (dom/hr)
                                  (:command
                                    (p/with-cycle [{:keys [status] :as state} {:status :idle}]
                                      (let [stage (try stage (catch Pending []))
                                            commit (when (Button. "commit!" (not= :idle status)) stage)
                                            discard (when (Button. "discard" (not= :idle status)) [])]
                                        (case status
                                          :idle (if-some [command (or commit discard)]
                                                  {:status :request
                                                   :command command}
                                                  state)
                                          :request (assoc state :status :pending)
                                          :pending (if busy state {:status :idle}))))))))))))]
        {:status (case status
                   :closed (if toggle :open :closed)
                   :open (case request nil (if toggle :closed :open) :pending)
                   :pending (if busy :pending :closed))
         :request request}))))