(ns hyperfiddle.input-zoo0
  (:require clojure.string
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

(e/defn PendingMonitor [edits] ; todo DirtyMonitor
  ; todo red error state
  (when (pos? (e/Count edits)) (dom/props {:aria-busy true}))
  edits)

;; Crude uncontrolled inputs, rarely useful

(e/defn Input* [& {:keys [maxlength type] :as props
                   :or {maxlength 100 type "text"}}]
  (e/client ; explicit site on all controls for compat with neutral callers
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (dom/On "input" #(-> % .-target .-value) "")))) ; no token

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
        (dom/On "input" #(-> % .-target .-value) v))))) ; emit on boot, rebuild on reset

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
      (prn 'Button! t (some? err))
      (dom/props {:disabled (or disabled (some? t))}) ; todo compile kw args
      (when (some? err) (dom/props {:aria-invalid true})) ; bug - error not removed (glitch)
      (if t [t directive] (e/amb))))) ; injected tokens do not resubmit until user interacts again

;; Transactional inputs that auto-submit (i.e., builtin submit and cancel)
; for forms that don't have an explicit submit button - like TodoMVC and iphone settings.
; in collection case, they can attach to inflight submit from a create-new.

(e/defn InputSubmit! [v & {:keys [maxlength type token] :as props
                           :or {maxlength 100 type "text"}}]
  ; like Input! but with commit/discard affordance at value level
  ; dirty when you dirty, you can blur and it stays ditry
  ; submit with enter, tab, or commit button
  ; discard with esc or discard

  ; event strategy: use "input" because it works on numeric, and also keydown reads
  ; the value before the node target value is set.
  ; separately, grab esc/cancel somehow (and it must be checkbox compatible)
  (e/client
    (let [!commit-err (atom nil) commit-err (e/watch !commit-err)
          [t v] (dom/input (dom/props (assoc props :maxLength maxlength :type type))
                  (PendingMonitor ; injected RetryToken activates this state
                    ; todo also listen for meta keys
                    (let [e (dom/On "input" identity nil) [t err] (e/RetryToken e)]
                      (when-not (or (dom/Focused?) (some? t)) (set! (.-value dom/node) v))
                      #_(if t [t ((fn [] (some-> e .-target .-value (subs 0 maxlength))))] (e/amb))
                      (if-let [t' (or t token)] ; the pending value may have come from create-new !
                        [t' ((fn [] (cond
                                      t (some-> e .-target .-value (subs 0 maxlength)) ; local
                                      token v)))] ; injected
                        (e/amb))))) ; tricky amb

          ; [t v e] - t can be burned but e & v remains = retry ?

          external-submit? (some? token)
          dirty? (or (e/Some? t) token)
          locally-dirty? (and (e/Some? t) (not= t token)) ; cancel inflight txn in this case - todo
          can-commit? locally-dirty?

          [us _ :as btns]
          (e/amb ; todo wire to input esc/enter
            (Button! ::commit :label "commit" :error commit-err :disabled (not can-commit?)) ; todo progress
            (Button! ::discard :label "discard" :disabled (not (e/Some? t))))]

      ;(prn 'edit (e/Some? t) (e/as-vec v)) (prn 'btns (e/as-vec btns))
      (e/for [[u cmd] btns]
        (case cmd
          ::discard (case ((fn [] (us) (t) (when token (token)))) ; clear any in-flight commit yet outstanding
                      (e/amb)) ; clear edits, controlled form will reset
          ::commit [(fn token
                      ; double burn is harmless when (= t token)
                      ([] (u) (t) (token)) ; success, burn both commit token and field token
                      ([err] (reset! !commit-err err) (u #_err))) ; keep uncommited field, present retry
                    v]))))) ; commit latest value from field

(e/defn CheckboxSubmit! [checked & {:keys [id label #_token] :as props
                                    :or {id (random-uuid)}}]
  ; checkbox - cannot discard, submit on toggle interaction.
  ; failure will discard and highlight red
  ; todo attach to in-flight submit
  (e/client
    (let [[t v] (e/amb
                  (dom/div ; for yellow background
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
                  (e/When label (dom/label (dom/props {:for id}) (dom/text label))))
          [us _ :as btns]
          (e/amb ; todo wire to input esc/enter
            (Button! ::commit :label "commit" :disabled (not (e/Some? t))) ; todo progress
            (Button! ::discard :label "discard" :disabled (not (e/Some? t))))]

      ;(prn 'edit t v) (prn 'btns (e/as-vec btns))
      (e/for [[u cmd] btns]
        (case cmd
          ::discard (case ((fn [] (us) (t))) ; clear any in-flight commit yet outstanding
                      (e/amb)) ; clear edits, controlled form will reset
          ::commit [(fn token
                      ([] (u) (t)) ; success, burn both commit token and field token
                      ([err] (u err))) ; keep uncommited field, present retry
                    v]))))) ; commit latest value from field

;; Submit and clear inputs - chat, create-new, etc

(e/defn InputSubmitClear! [& {:keys [maxlength type] :as props
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
        (PendingMonitor
          (dom/OnAll "keydown" submit!))))))


;; Graveyard

; these are inline submit inputs but uncancellable and cannot retry on failure
; because each sequential submit is independent, due to On-all allocating new
; tokens for each submission

(e/defn InputSubmit!-nocancel-noretry
  [v & {:keys [maxlength type] :as props
        :or {maxlength 100 type "text"}}]
  (e/client
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (PendingMonitor
        (letfn [(read! [node] (not-empty (subs (.-value node) 0 maxlength)))
                (submit! [e] (let [k (.-key e)]
                               (cond
                                 (= "Enter" k) (read! (.-target e)) ; no clear
                                 (= "Escape" k) (do (set! (.-value dom/node) "") nil)
                                 () nil)))]
          (dom/OnAll "keydown" submit!)))))) ; eagerly submit individual edits

(e/defn CheckboxSubmit!-nocancel-noretry
  [checked & {:keys [id label] :as props
              :or {id (random-uuid)}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
        (let [edits (dom/OnAll "change" #(-> % .-target .-checked))] ; eagerly submit individual edits
          (when-not (or (dom/Focused?) (pos? (e/Count edits)))
            (set! (.-checked dom/node) checked))
          edits))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))