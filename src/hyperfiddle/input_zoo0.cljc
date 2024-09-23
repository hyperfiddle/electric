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
      (prn 'Button! directive t err)
      (dom/props {:disabled (or disabled (some? t))}) ; todo compile kw args
      #_(when waiting? (dom/props {:aria-busy true}))
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
  ; todo also listen for meta keys

  ; event strategy: use "input" because it works on numeric, and also keydown reads
  ; the value before the node target value is set.
  ; separately, grab esc/cancel somehow (and it must be checkbox compatible)
  (e/client
    (let [[t v] (dom/input (dom/props (-> props (dissoc :token) (assoc :maxLength maxlength :type type)))
                  (let [e (dom/On "input" identity nil) [t err] (e/RetryToken e)
                        editing? (dom/Focused?)
                        waiting-mine? (some? t)
                        waiting? (or (some? t) (some? token))
                        error? (some? err) ; todo route error from original token
                        dirty? (or editing? waiting-mine? error?)]
                    (prn 'InputSubmit! editing? waiting-mine? waiting? error? dirty?) ; prove glitch
                    (when-not dirty? (set! (.-value dom/node) v))
                    (when error? (dom/props {:aria-invalid true}))
                    (when waiting? (dom/props {:aria-busy true})) ; glitch
                    (if waiting?
                      [(or t token) ; absorb foreign token
                       ((fn [] (cond
                                 t (some-> e .-target .-value (subs 0 maxlength)) ; local override
                                 token v)))]
                      (e/amb)))) ; tricky amb

          ; [t v e] - t can be burned but e & v remains = retry ?

          external-submit? (some? token) ; inlining this under when causes a glitch NPE in commit
          ; q (and external-submit? (not= t token))
          dirty? (e/Some? t)
          locally-dirty? (and (e/Some? t) (not= t token)) ; cancel inflight txn in this case - todo
          can-commit? locally-dirty?

          [us _ :as btns]
          (e/amb ; todo wire to input esc/enter
            (Button! ::commit :label "commit" :disabled (not can-commit?)) ; todo progress
            (Button! ::discard :label "discard" :disabled (not dirty?)))]

      external-submit? ; workaround crash on discard in todos

      ;(prn 'edit (e/Some? t) (e/as-vec v)) (prn 'btns (e/as-vec btns))
      (e/for [[u cmd] btns]
        (case cmd
          ::discard (case ((fn [] (us) (t) (when external-submit? (token)))) ; clear any in-flight commit yet outstanding
                      (e/amb)) ; clear edits, controlled form will reset
          ::commit [(fn token ; double burn is harmless when (= t token)
                      ([] (u) (t) (when external-submit? (token))) ; success, burn both commit token and field token
                      ([err] (u err))) ; keep uncommited field, present retry
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

;; Dubious controls due to absence of failure handling, nonetheless they are
;; simple and useful in the meantime while the rigorous controls are still WIP.

(e/defn InputSubmit?!
  "Dubious: nocancel, noretry (Assumes happy path with no failure recovery?!),
noforeign (cannot attach to foreign in-flight edits). Controlled, buffers dirty
edits, discard on Esc, submit on Enter. Eagerly submits concurrent isolated
edits which race. Sets [aria-busy=true] until all edits are accepted."
  [v & {:keys [maxlength type autofocus ::discard ::commit] :as props
        :or {maxlength 100 type "text"}}]
  (e/client
    (dom/input (dom/props (-> props (dissoc ::discard ::commit)
                            (assoc :maxLength maxlength :type type)))
      (when-not (dom/Focused?) (set! (.-value dom/node) v))
      #_(when autofocus (case v (.focus dom/node))) ; focus after v loads
      (PendingMonitor
        (letfn [(read! [node] (not-empty (subs (.-value node) 0 maxlength)))
                (submit! [e] (let [k (.-key e)]
                               (cond
                                 ; if commit directive provided, use it, otherwise emit v
                                 (= "Enter" k) (let [v (read! (.-target e))] ; no clear
                                                 (if commit (commit v) v))
                                 ; if discard directive provided, emit, otherwise swallow
                                 (= "Escape" k) (do (set! (.-value dom/node) v) discard)
                                 () nil)))]
          ; nocancel, noretry, nocreate. To attach to foreign in-flight edits
          ; (i.e. from InputSubmitCreate?!), we'd need to hook an On-all token
          ; accordingly, which defeats the purpose of this "dubious" implementation.
          (let [edits (dom/On-all "keydown" submit!)] ; eagerly submit individual edits
            (when-not (or (dom/Focused?) (pos? (e/Count edits))) (set! (.-value dom/node) v))
            edits))))))

(e/defn CheckboxSubmit?!
  "Dubious: nocancel, noretry."
  [checked & {:keys [id label] :as props
              :or {id (random-uuid)}}]
  (e/client
    (e/amb
      (dom/div ; for pending monitor
        (dom/props {:style {:display "inline-block" :width "fit-content"}})
        (PendingMonitor
          (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
            (let [edits (dom/On-all "change" #(-> % .-target .-checked))] ; eagerly submit individual edits
              (when-not (or (dom/Focused?) (pos? (e/Count edits))) (set! (.-checked dom/node) checked))
              edits))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn InputSubmitCreate?!
  "Dubious: nocancel, noretry."
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
        (PendingMonitor
          (dom/On-all "keydown" submit!))))))