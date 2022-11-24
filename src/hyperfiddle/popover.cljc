(ns hyperfiddle.popover
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros hyperfiddle.popover)))

; data PopoverState = Closed | Open request | Pending request
; data BodyState = Idle | Request command | Pending command
; data Command = Commit tx | Discard

(p/defn PopoverBody [Body busy]
  (dom/div {:style {:position "fixed"}}
    (dom/div {:style {:border           "1px pink solid" :padding "5px"
                      :position         "relative" :left "3em" :top "2em" :z-index "1"
                      :width            "50em" :height "40em"
                      :background-color "rgb(248 250 252)"}}
      (:command
        (p/with-cycle [{:keys [status] :as state} {:status :idle}]

          (let [!stage (atom ::unknown) stage (p/watch !stage)]

            (p/with-cycle [loading ::hf/idle]
              (dom/div (pr-str loading))
              (try

                (let [stage (p/server
                              (p/with-cycle [stage []]
                                (binding [hf/db (:db-after (hf/with hf/db stage))]
                                  (p/client (Body.)))))] ;; TODO validation
                  (reset! !stage stage))

                ::hf/idle
                (catch Pending e ::hf/loading)))

            (dom/hr)
            (let [commit (when (ui/Button. "commit!" (not= :idle status)) stage) ;; TODO disable when invalid
                  discard (when (ui/Button. "discard" (not= :idle status)) [])]
              (ui/edn-editor stage {::dom/disabled true})
              (case status
                :idle (if-some [command (or commit discard)]
                        {:status :request
                         :command command}
                        state)
                :request (assoc state :status :pending)
                :pending (if busy state {:status :idle})))))))))

(p/defn Popover [label Body]
  (:request
    (p/with-cycle [{:keys [status]} {:status :closed}]
      (let [busy false
            toggle (ui/Button. label (= :pending status)) ; popover anchor
            request (case status
                      :closed nil
                      (:open :pending) (PopoverBody. Body busy))]
        {:status  (case status
                    :closed (if toggle :open :closed)
                    :open (case request nil (if toggle :closed :open) :pending)
                    :pending (if busy :pending :closed))
         :request request}))))