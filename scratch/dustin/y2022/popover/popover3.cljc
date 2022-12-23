(ns dustin.y2022.popover3
  (:require contrib.str
            #?(:clj [datascript.core :as d])
            #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-dom2 :refer [Event]]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.popover3)))

(def !conn #?(:cljs nil
              :clj (doto (d/create-conn {:order/email {}})
                     (d/transact! [{:order/email "alice@example.com" :order/gender :order/female}]))))


(p/def db)

(p/defn StagingArea [stage !stage]
  (p/client
    (dom/element "style" ".stage {width: 30em; height: 5em;}")
    (ui/textarea {::dom/class "stage"
                  ::ui/value (some-> stage vec contrib.str/pprint-str)
                  ::ui/input-event (p/fn [e]
                                     (let [tx (try (clojure.edn/read-string (.. e -target -value))
                                                   (catch :default _ nil))]
                                       (p/server (reset! !stage tx))))})))

(p/defn Button [label busy]
  (dom/with (dom/dom-element dom/node "button")
            (dom/set-text-content! dom/node label)
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

(p/defn Input [controlled-value]
  ; data State = Editing local-value | NotEditing controlled-value
  (:value ; local or controlled
    (dom/with (dom/dom-element dom/node "input")
              (.setAttribute dom/node "type" "text")
              (p/with-cycle [state {:edit? false}]
                (if (:edit? state)
                  (merge state
                         {:edit? (not (some? (Event. "blur" false)))}
                         (when-some [e (Event. "input" false)]
                           {:value (.-value (.-target e))})) ; use local value
                  (do (.setAttribute dom/node "value" controlled-value) ; throw away local value
                      {:edit? (some? (Event. "focus" false)) ; never busy - process synchronously
                       :value controlled-value})))))) ; throw away local value

(p/defn Form [busy]
  (let [v (p/server (:order/email (d/entity db 1)))
        v' (Input. (or v ""))]
    (when-not (= v v')
      (println 'v' v')
      [[:db/add "-1" :person/name v']])))

(p/defn Demo []
  (p/client
    (let [!stage (atom []) stage (p/watch !stage)]
      (p/with-cycle [busy false]
        (p/server
          (binding [db (:db-after (d/with (p/watch !conn) stage))]
            (p/client
              (when-let [tx (Popover. (p/fn [open?] (str (if open? "close" "open") " popover"))
                                 Form
                                 busy)]
                (println 'tx tx)
                (reset! !stage (concat stage tx))
                false)))))
      (StagingArea. stage !stage))))