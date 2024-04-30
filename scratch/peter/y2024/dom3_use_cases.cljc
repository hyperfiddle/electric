(ns peter.y2024.dom3-use-cases
  (:require
   [missionary.core :as m]
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.electric-dom3-efns :as dom]
   [hyperfiddle.incseq :as i]))

;;;;;;;;;;;;;;;;;;
;;; PAY BUTTON ;;;
;;;;;;;;;;;;;;;;;;

(defn ->state [events]
  (m/ap (let [!state (atom [::idle])
              [state v] (m/?> (m/watch !state))
              e (m/?> events)]
          ))
  (->> (m/ap
         (let [!take? (atom true), done! #(reset! !take? true), e (m/?> events)]
           (if @!take?
             (do (reset! !take? false) [e done!])
             (m/amb))))
    (m/reductions {} nil)))

(defn listen> [node typ]
  (m/observe (fn [!] (.addEventListener node typ !) #(.removeEventListener node typ !))))

(let [[state v !state] (events->state 'node "click")]
  (case state
    ::idle (dom/style {:background-color "gray"})
    ::busy (do (dom/props {:aria-busy true}) (if-let [v ($ ProcessPayment)] (!state ::failed v) (!state ::success)))
    ::success (do (dom/style {:background-color "green"}) (after 5000 (!state ::idle)))
    ::failed (do (dom/style {:background-color "red"}) ($ ShowError v))))




(dom/style {:background-color
            (if-some [done! (tx-events "click")]
              (case ($ SuccessMonitor ($ Transact))
                ::success (do (done!) "green")
                ::failure (do (done!) "red")
                nil "yellow")
              "gray")})

(dom/style {:background-color
            (let [[_foo done! busy?] (tx-events "click" (fn [e] (.stopPropagation e) "foo"))]
              (case ($ CheckResult ($ Transact conn {:foo :bar}) busy?)
                ::success (do (done!) "green")
                ::failure (do (done!) "red")
                #_else (if busy? "yellow" "purple")))})

(dom/style {:background-color
            (let [[_foo done! busy?] (tx-events "click" (fn [e] (.stopPropagation e) "foo"))
                  true? (fetch db user :checkbox)]
              (case ($ CheckResult ($ Transact conn {:checked (not true?)}) busy?)
                ::success (do (done!) "green")
                ::failure (do (done!) "red")
                #_else (if busy? "yellow" "purple")))})

#?(:cljs (defn listen* [node typ f opts]
           (m/observe (fn [!]
                        (let [! #(! (f %)), opts (clj->js opts)]
                          (.addEventListener node typ ! opts)
                          #(.removeEventListener node typ ! opts))))))

(defn event->task [flow]
  (->> (m/ap
         (let [!busy? (atom false)
               v (m/?> (m/eduction (remove (fn [_] @!busy?)) flow))
               dfv (m/dfv)]
           (m/amb [v #(dfv false) (reset! !busy? true)]  [v #() (reset! !busy? (m/? dfv))])))
    (m/reductions {} [nil #() false])))

(comment
  (def !! (atom nil))
  (def ps ((event->task (m/observe (fn [!] (reset! !! !) #()))) #(prn :step) #(prn :done)))
  @ps
  (@!! 5)
  (def d @ps)
  d
  ((second d))
  (ps)
  )

(dom/on "click" (e/fn [e]
                  (let [true? (fetch db user :checkbox)]
                    (Transact conn {:checked (not true?)}))))

(hf-ui/button)

(dom/button
  (let [[v busy? done!] (tx-events "keydown" (fn [e] (when (= "Enter" (.-key e)) (-> e .-target .-value))))]
    (when ($ Transact {}) (done!))
    )
  )

(dom/style {:background-color
            (or (tx-events "click"
                  (e/fn [busy?]
                    (when busy? (dom/props {:disabled true}))
                    (case ($ SuccessMonitor ($ Transact) busy?)
                      ::success "green"
                      ::failure "red"
                      #_else nil)))
              "purple")})

(if-some [busy! (tx-events "click")]
  (case ($ SuccessMonitor ($ Transact))
    ::success "green"
    ::failure "red"
    nil "yellow")
  "gray")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPTIMISTIC UPDATE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; [ buy milk... ]       <- input
;;
;; [ ] go to store       <- open task
;; [X] call car shop     <- completed task
;;
;; - tasks are stored on server
;; - we want to render new tasks as user enters them
;; - user is decopuled from server/network, i.e. can enter N tasks concurrently
;; - tasks have to render immediately
;; - as server acks/rejects them they should update

;; impl
;; - project server values into local spine
;; - CRUD updates go to local spine

(defn pop-value [nd] (let [v (.-value nd)] (set! (.-value nd) "") v))
(defn enter [e] (when (= "Enter" (.-key e)) (pop-value (.-target e))))
(defn add-task! [S e]
  (when-some [v (enter e)]
    (let [id (random-uuid)]
      (S id {} {:db/id id, :task/description v, ::state ::local}))))

;; fill client spine with data from server query
(let [S ($ Project (i/spine) (e/server #(query-tasks db)))]
  (dom/input
    ;; optimistically add local items to the spine
    (e/join (dom/uf->is (dom/listen* dom/node "keydown" (partial add-task! S)))))
  (dom/ul
    (e/cursor [t (e/join S)]
      (dom/li
        (dom/text (:task/description t))
        (dom/props {:border-color
                    (case (::state t)
                      ;; transact local items
                      ;; hoist transaction up to survive page navigating away
                      ::local (do ($ Transact S t) "blue")
                      ::ok "green"
                      ::failed "red")})
        ;; mount retry button when task failed to transact
        (when (= ::failed (::state t))
          (dom/button (dom/text "Retry")
            (let [[v done! busy?] (e/join (dom/event->task (dom/listen* dom/node "click" identity)))]
              (when ($ Transact S t) (done!)))))))))
