(ns hyperfiddle.input-zoo0
  (:require clojure.string
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

;; Crude uncontrolled inputs, rarely useful

(e/defn Input* [& {:keys [maxlength type] :as props
                   :or {maxlength 100 type "text"}}]
  (e/client ; explicit site on all controls for compat with neutral callers
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (dom/On "input" #(-> % .-target .-value (subs 0 maxlength)) "")))) ; no token

(e/defn Checkbox* [& {:keys [id label] :as props
                      :or {id (random-uuid)}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
        (dom/On "change" #(-> % .-target .-checked) false))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

;; Simple controlled inputs (dataflow circuits)

(e/defn Input [v & {:keys [maxlength type] :as props
                    :or {maxlength 100 type "text"}}]
  (e/client
    (e/with-cycle [v (str v)] ; emits signal of current state
      (dom/input (dom/props (assoc props :maxLength maxlength :type type))
        (when-not (dom/Focused?) (set! (.-value dom/node) v))
        (dom/On "input" #(-> % .-target .-value (subs 0 maxlength)) v))))) ; emit on boot, rebuild on reset

(e/defn Checkbox [checked & {:keys [id label] :as props
                             :or {id (random-uuid)}}]
  (e/client
    (e/amb
      (e/with-cycle [checked checked]
        (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
          (when-not (dom/Focused?) (set! (.-checked dom/node) checked))
          (dom/On "change" #(-> % .-target .-checked) checked)))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

;; Transactional inputs
; Errors are forwarded in via token callback
; Errors are never sent out via signal, because consumers already saw it when they forwarded it in

(e/defn Input! [v & {:keys [maxlength type] :as props
                     :or {maxlength 100 type "text"}}]
  (e/client
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (let [e (dom/On "input" identity nil) [t err] (e/RetryToken e) ; reuse token until commit
            editing? (dom/Focused?)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)]
        (when-not dirty? (set! (.-value dom/node) v))
        (when error? (dom/props {:aria-invalid true}))
        (when waiting? (dom/props {:aria-busy true}))
        (if waiting? [t ((fn [] (-> e .-target .-value (subs 0 maxlength))))] (e/amb))))))

; include the error for the pending monitor - unclear if right
; encapsulate errors, the form already saw it when forwarding here

(e/defn Checkbox! [checked & {:keys [id label] :as props
                              :or {id (random-uuid)}}]
  ; todo esc?
  (e/client
    (e/amb
      (dom/div ; checkboxes don't have background so style wrapper div
        (dom/props {:style {:display "inline-block" :width "fit-content"}})
        (let [[e t err input-node]
              (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
                (let [e (dom/On "change" identity) [t err] (e/RetryToken e)] ; single txn, no concurrency
                  [e t err dom/node]))
              editing? (dom/Focused? input-node)
              waiting? (some? t)
              error? (some? err)
              dirty? (or editing? waiting? error?)]
          (when-not dirty? (set! (.-checked input-node) checked))
          (when error? (dom/props {:aria-invalid true}))
          (when waiting? (dom/props {:aria-busy true}))
          (if waiting? [t ((fn [] (-> e .-target .-checked)))] (e/amb))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn Button! [directive & {:keys [label disabled id token] :as props
                              :or {id (random-uuid)}}]
  (dom/button (dom/text label) ; (if err "retry" label)
    (dom/props (-> props (dissoc :label :disabled) (assoc :id id)))
    (let [[t err] (e/RetryToken (dom/On "click" identity nil))]
      (prn 'Button! directive t err)
      (dom/props {:disabled (or disabled (some? t))}) ; todo compile kw args
      #_(when waiting? (dom/props {:aria-busy true}))
      (when (some? err) (dom/props {:aria-invalid true})) ; bug - error not removed (glitch)
      (if t [t directive] (e/amb))))) ; injected tokens do not resubmit until user interacts again

(e/defn InputSubmitCreate!
  "optimistic, cancel & retry are forwarded to optimistic list item's InputSubmit!
buffers (dirty), commit, discard bundled as enter/esc"
  [& {:keys [maxlength type] :as props
      :or {maxlength 100 type "text"}}]
  (e/client
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (letfn [(read! [node] (not-empty (subs (.-value node) 0 maxlength)))
              (read-clear! [node] (when-some [v (read! node)] (set! (.-value node) "") v))
              (submit! [e] (let [k (.-key e)]
                             (cond
                               (= "Enter" k) (read-clear! (.-target e))
                               (= "Escape" k) (do (set! (.-value dom/node) "") nil)
                               () nil)))]
        #_(PendingMonitor) ; the optimistic list item is responsible for pending/retry affordances
        (dom/On-all "keydown" submit!)))))
