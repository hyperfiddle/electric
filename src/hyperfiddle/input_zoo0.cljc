(ns hyperfiddle.input-zoo0
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

(e/defn PendingMonitor [edits] ; todo DirtyMonitor
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

(e/defn Input! [v & {:keys [maxlength type] :as props
                     :or {maxlength 100 type "text"}}]
  (e/client
    (dom/input (dom/props (assoc props :maxLength maxlength :type type))
      (PendingMonitor
        (let [e (dom/On "input" identity nil) t (e/Token e)] ; reuse token until commit
          (when-not (or (dom/Focused?) (some? t)) (set! (.-value dom/node) v))
          (if t [t ((fn [] (-> e .-target .-value (subs 0 maxlength))))] (e/amb)))))))

(e/defn Checkbox! [checked & {:keys [id label] :as props
                              :or {id (random-uuid)}}]
  ; todo esc?
  (e/client
    (e/amb
      (dom/div ; for yellow background
        (dom/props {:style {:display "inline-block" :width "fit-content"}})
        (PendingMonitor ; checkboxes don't have background so style wrapper div
          (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
            (let [e (dom/On "change" identity) t (e/Token e)] ; single txn, no concurrency
              (when-not (or (dom/Focused?) (some? t)) (set! (.-checked dom/node) checked))
              (if t [t ((fn [] (-> e .-target .-checked)))] (e/amb))))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn Button! [directive & {:keys [label disabled id error] :as props
                              :or {id (random-uuid)}}]
  (dom/button (dom/text label)
    (dom/props (-> props (dissoc :label :disabled) (assoc :id id)))
    (let [t (e/Token (dom/On "click"))]
      (dom/props {:disabled (or disabled (some? t)) ; todo compile kw args
                  :aria-invalid (some? error)})
      (when (some? error)
        (dom/span (dom/text error) (dom/props {:class "hyperfiddle-error"})))
      (e/When (some? t) [t directive]))))

;; Transactional inputs that auto-submit (i.e., builtin submit and cancel)
; for forms that don't have an explicit submit button - like settings page.

(e/defn InputSubmit! [v & {:keys [maxlength type] :as props
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
                  (PendingMonitor
                    ; todo also listen for meta keys
                    (let [e (dom/On "input" identity nil) t (e/Token e)]
                      (when-not (or (dom/Focused?) (some? t)) (set! (.-value dom/node) v))
                      (if t [t ((fn [] (-> e .-target .-value (subs 0 maxlength))))] (e/amb))))) ;tricky amb
          [us _ :as btns]
          (e/amb ; todo wire to input esc/enter
            (Button! ::commit :label "commit" :disabled (not (e/Some? t)) :error commit-err) ; todo progress
            (Button! ::discard :label "discard" :disabled (not (e/Some? t))))]

      (prn 'edit t v) (prn 'btns (e/as-vec btns))
      (e/for [[u cmd] btns]
        (case cmd
          ::discard (case (us (t)) ; clear any in-flight commit yet outstanding
                      (e/amb)) ; clear edits, controlled form will reset
          ::commit [(fn token
                      ([] (u (t))) ; success, burn both commit token and field token
                      ([err] (reset! !commit-err err) (u))) ; keep uncommited field, present retry
                    v]))))) ; commit latest value from field

(e/defn CheckboxSubmit! [checked & {:keys [id label] :as props
                                    :or {id (random-uuid)}}]
  (e/client
    (let [!commit-err (atom nil) commit-err (e/watch !commit-err)
          [t v] (dom/div ; for yellow background
                  (dom/props {:style {:display "inline-block" :width "fit-content"}})
                  (e/amb
                    (PendingMonitor
                      (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
                        (let [e (dom/On "change" identity) t (e/Token e)] ; single txn, no concurrency
                          (when-not (or (dom/Focused?) (some? t)) (set! (.-checked dom/node) checked))
                          (if t [t ((fn [] (-> e .-target .-checked)))] (e/amb)))))
                    (e/When label (dom/label (dom/props {:for id}) (dom/text label)))))
          [us _ :as btns]
          (e/amb ; todo wire to input esc/enter
            (Button! ::commit :label "commit" :disabled (not (e/Some? t)) :error commit-err) ; todo progress
            (Button! ::discard :label "discard" :disabled (not (e/Some? t))))]

      ;(prn 'edit t v) (prn 'btns (e/as-vec btns))
      (e/for [[u cmd] btns]
        (case cmd
          ::discard (case (us (t)) ; clear any in-flight commit yet outstanding
                      (e/amb)) ; clear edits, controlled form will reset
          ::commit [(fn token
                      ([] (u (t))) ; success, burn both commit token and field token
                      ([err] (reset! !commit-err err) (u))) ; keep uncommited field, present retry
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
          (dom/OnAll "keydown" submit!)))))) ; onall is wrong concurrency for this

(e/defn CheckboxSubmit!-nocancel-noretry
  [checked & {:keys [id label] :as props
              :or {id (random-uuid)}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label))
        (let [edits (dom/OnAll "change" #(-> % .-target .-checked))] ; wrong concurrency
          (when-not (or (dom/Focused?) (pos? (e/Count edits)))
            (set! (.-checked dom/node) checked))
          edits))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))