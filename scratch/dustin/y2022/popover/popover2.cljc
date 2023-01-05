(ns dustin.y2022.popover2
  (:require #?(:clj [datomic.api :as d])
            #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :refer [node]]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.popover2)))

(p/def db)

(p/defn Button [label busy]
  (dom/with (dom/dom-element node "button")
            (dom/set-text-content! node label)
            (Event. "click" busy)))

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
                                  :pending (if busy state {:status :idle}))))))))]
        {:status (case status
                   :closed (if toggle :open :closed)
                   :open (case request nil (if toggle :closed :open) :pending)
                   :pending (if busy :pending :closed))
         :request request}))))


