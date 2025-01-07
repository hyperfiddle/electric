(ns hyperfiddle.electric-forms3
  #?(:cljs (:require-macros hyperfiddle.electric-forms3))
  (:require [contrib.data :refer [index-by auto-props qualify]]
            [contrib.str :refer [pprint-str]]
            [missionary.core :as m]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

;;; Simple controlled inputs (dataflow circuits)

(e/defn Input [v & {:keys [maxlength type parse] :as props
                    :or {maxlength 100 type "text" parse identity}}]
  (e/client
    (dom/input (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
      (e/Reconcile ; don't reveal :grow/:shrink when the `if` switches
        ; (Note: Reconcile is discrete, so it will not even emit :change on switch)
        (if (dom/Focused?)
          (dom/On "input" #(-> % .-target .-value (subs 0 maxlength) parse) (e/snapshot (str v)))
          (set! (.-value dom/node) (str v)))))))

(e/defn Checkbox [checked & {:keys [id label parse] :as props
                             :or {id (random-uuid) parse identity}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id})
        (dom/props (dissoc props :id :label :parse))
        (e/Reconcile
          (if (dom/Focused?) ; TODO port safari fix from forms0
            (dom/On "change" #(-> % .-target .-checked parse) (e/snapshot checked))
            (set! (.-checked dom/node) checked))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

;;; Simple uncontrolled inputs (sugar for the unvarying literal case)

(e/defn Input* "
Simple uncontrolled input, e.g.

    (parse-long (Input 42 :maxlength 100))"
  [init-v & {:as props}]
  (e/client (e/with-cycle [v init-v] (Input v props))))

(e/defn Checkbox* "
Simple uncontrolled checkbox, e.g.

    (Checkbox false :label \"debug\")"
  [init-v & {:as props}]
  (e/client (e/with-cycle [v init-v] (Checkbox v props))))

(e/defn InputValidity
  "Manage input validation state"
  ([validation-message] (InputValidity dom/node validation-message))
  ([node validation-message]
   (when (not-empty (str validation-message))
     (dom/props node {:data-errormessage (str validation-message)}))
   (.setCustomValidity node (str validation-message))
   (e/on-unmount #(.setCustomValidity node "")))) ; empty string required, not nil

;;; Transactional inputs
; Errors are forwarded in via token callback
; Errors are never sent out via signal, because consumers already saw it when they forwarded it in

(e/defn Input! [field-name ; fields are named like the DOM, <input name=...> - for coordination with form containers
                v & {:keys [maxlength type parse edit-monoid Validate] :as props
                     :or {maxlength 100 type "text" parse identity edit-monoid hash-map, Validate (e/fn [_])}}]
  (e/client
    (dom/input
      (dom/props (-> props (dissoc :parse :Validate) (assoc :maxLength maxlength :type type)))
      (let [e (dom/On "input" identity nil) [t err] (e/Token e) ; reuse token until commit
            editing? (dom/Focused?)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)]
        (when-not dirty? (set! (.-value dom/node) v)) ; todo - submit must reset input while focused
        (when error? (dom/props {:aria-invalid true})) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (when waiting? (dom/props {:aria-busy true}))
        (e/When waiting? ; return nothing, not nil - because edits are concurrent, also helps prevent spurious nils
          (let [v' ((fn [] (-> e .-target .-value (subs 0 maxlength) parse)))
                validation-message (Validate v')
                edit (edit-monoid field-name v')] ; named field edit, a KV structure
            (InputValidity validation-message)
            [t edit validation-message]) ; edit request, bubbles upward to service
          )))))

(defn unify-t ; unify-token ; WIP
  ([]  (constantly nil))
  ([t] (or t (constantly nil)))
  ([t1 t2] (comp first (juxt (unify-t t1) (unify-t t2))))
  ([t1 t2 & ts] (reduce unify-t (unify-t t1 t2) ts)))

(defn after-ack "
proxy token t such that callback f! will run once the token is ack'ed. E.g. to ack many tokens at once."
  [t1 f!]
  (unify-t t1 (fn proxy-token [& [_err]] (f!))))
;; chaining f after t or t after f is just `comp`

(e/defn Checkbox! [k checked & {:keys [id type label parse edit-monoid Validate] :as props
                                :or {id (random-uuid) type :checkbox parse identity edit-monoid hash-map
                                     Validate (e/fn [_])}}]
  ; todo esc?
  (e/client
    (e/amb
      (let [[e t err input-node]
            (dom/input (dom/props {:type type, :id id}) (dom/props (dissoc props :id :label :parse :edit-monoid :Validate))
                       (let [e (dom/On "change" identity nil) [t err] (e/Token e)] ; single txn, no concurrency
                         [e t err dom/node]))
            editing? (dom/Focused? input-node) ; TODO port safari fix from forms0
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)
            v (if waiting? ((fn [] (-> e .-target .-checked parse))) checked)
            validation-message (Validate v)]
        (when (or (not dirty?) (#{"radio" :radio} type)) ; Radio's "don't damage user input" behavior handled at radiogroup level.
          (set! (.-checked input-node) checked))
        (when error? (dom/props input-node {:aria-invalid true}))  ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (InputValidity input-node validation-message)
        (e/When waiting?
          (dom/props input-node {:aria-busy true})
          [t (edit-monoid k v) validation-message]))
      (e/When label (dom/label (dom/props {:for id}) (dom/text (label k)))))))

(e/defn LatestEdit "
Implement radio single-select behavior by retaining only the token from the most recently touched
input[type=radio], unselecting/clearing all other input[type=radio] by accepting their tokens.
The pattern is general, i.e. not specific to just radios: every time a new concurrent token arrives,
accept the previous token and retain the new one."
  [edits]
  (let [!latest (atom nil)
        latest (e/watch !latest)]
    (case (e/Count edits)
      0 (reset! !latest nil)
      (swap! !latest (fn [[old-t _old-v] new] (when old-t (old-t)) new) edits))
    (e/When latest latest)))

(defn -checked? [v x]
  (when v (= v x)))

(e/defn Radio! [k authoritative-v & {:keys [id option-label Options type Validate edit-monoid]
                       :or {type :radio, Validate (e/fn [_]), edit-monoid hash-map}
                       :as props}]
  (let [!selected (atom [nil (e/snapshot authoritative-v) nil]) ; "don't damage user input" – value will be set in absence of a token – see below
        [t v errors :as edit] (e/watch !selected)
        validation-message (not-empty (str (Validate v)))
        options (Options)]
    (reset! !selected
      (dom/dl
        (dom/props {:id id, :role "radiogroup", :data-errormessage validation-message
                    :style {:--radiogroup-items-count (e/Count options)}})
        (let [props (if (#{"radio" :radio} type) (assoc props :name k) props)]
          (LatestEdit
            (e/for [x options]
              (let [id (random-uuid)]
                (dom/dt (dom/label (dom/props {:for id}) (dom/text x)))
                (dom/dd
                  (Checkbox! x (-checked? v x) :id id :type type :label option-label
                    :edit-monoid (fn [x _checked?] x)
                    (dissoc props :id :type :option-label :Options :Validate)))))))))
    (if (some? t) ; "don't damage user input" – only track authoritative value in absence of a token
      [(after-ack t (fn after [] (swap! !selected assoc 0 nil 2 nil))) ; clear token and error, don't touch value
       (edit-monoid k v)
       errors]
      (do (swap! !selected assoc 1 authoritative-v) (e/amb)))))


;;; Buttons

(e/defn Button* [{:keys [label] :as props}]
  (dom/button (dom/text label)
              (dom/props (dissoc props :label))
              [(dom/On "click" identity nil) dom/node]))

(e/defn Button "Simple button, return latest click event or nil."
  [{:keys [label] :as props}]
  (first (Button* props)))

(e/defn Button!* [{:keys [label] :as props}]
  (let [[event node] (Button* props)
        [btn-t err] (e/Token event)]
    [btn-t err event node]))

(e/defn Button! ; no clear use case, it seems users always want TxButton! or a variant of TxButton!
  "Transactional button. Emits a token on click. See `Button!*` for customization and `TxButton!` for a non-trivial impl example."
  [{:keys [label] :as props}]
  (let [[t _err _event _node] (Button!* props)]
    (e/When t t))) ; should it return [t err]?

(e/defn TxButton! ; Regular `Button!`, with extra markup.
  "Transactional button with busy state. Disables when busy."
  [{:keys [disabled type] :or {type :submit} :as props}]
  (let [[btn-t err event node] (e/Reconcile ; HACK wtf? further props on node will unmount on first click without this
                                 (Button!* (-> props (assoc :type type) (dissoc :disabled))))]
    ;; Don't set :disabled on <input type=submit> before "submit" event has bubbled, it prevents form submission.
    ;; When "submit" event reaches <form>, native browser impl will check if the submitter node (e.g. submit button) has a "disabled=true" attr.
    ;; Instead, let the submit event propagate synchronously before setting :disabled, by queuing :disabled on the event loop.
    (dom/props node {:disabled (e/Task (m/sleep 0 (or disabled (some? btn-t))))})
    (dom/props node {:aria-busy (some? btn-t)})
    (dom/props node {:aria-invalid (#(and (some? err) (not= err ::invalid)) err)}) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
    (dom/props node {:data-tx-status (when (and (some? event) (nil? btn-t) (nil? err)) "accepted")}) ; FIXME can't distinguish between successful tx or tx canceled by clicking discard.
    (e/When btn-t btn-t))) ; forward token to track tx-status ; should it return [t err]?

(e/defn ButtonGenesis!
  "Spawns a new tempid/token for each click. You must monitor the spawned tempid
in an associated optimistic collection view!"
  [directive & {:keys [label disabled type form] :as props
                :or {type :button}}] ; default type in form is submit
  (dom/button (dom/text label) ; (if err "retry" label)
    (dom/props (-> props (dissoc :label :disabled) (assoc :type type)))
    (dom/props {:disabled (or disabled #_(some? t))})
    #_(dom/props {:aria-busy (some? t)})
    #_(dom/props {:aria-invalid (some? err)})
    (e/for [[btn-q e] (dom/On-all "click" identity)]
      (let [[form-t form-v] (e/snapshot form)] ; snapshot to detach form before any reference to form, or spending the form token would loop into this branch and cause a crash.
        (form-t) ; immediately detach form
        [(fn token ; proxy genesis
           ([] (btn-q) #_(form-t))
           ([err] '... #_(btn-q err)))

          ; abandon entity and clear form, ready for next submit -- snapshot to avoid clearing concurrent edits
         [directive form-v]]))))

#_(defn stop-err-propagation [token] (when token (fn ([] (token)) ([_err])))) ; scratch

(defn directive->command [directive edit token]
  (when token
    (let [[edit-t edit-v] edit]
      [(unify-t token edit-t)
       (if edit-t [directive edit-v] [directive nil])])))

(e/defn Directive! [directive edit token]
  (e/When token (directive->command directive edit token)))

;;; Forms

; commit/discard with staging area
; inputs - emit as you type - with a token
; stage - monitor edits and accumulate them
; button - batch all edits into single token, chained with upstream tokens

#?(:cljs (defn- active-form-input [form]
           (when-let [focused-input (.-activeElement js/document)]
             (when (.contains form focused-input)
               focused-input))))

#?(:cljs (defn- blur-active-form-input! [form] (some-> (active-form-input form) (.blur))))

#?(:cljs (defn- reset-active-form-input! [form]
           (when-let [input (active-form-input form)]
             (set! (.-value input) ""))))

(e/defn FormDiscard! ; dom/node must be a form
  [directive edits & {:keys [show-button] :as props}]
  (e/client
    ;; reset form on <ESC>
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    ;; Render an <input type=reset>, natively resetting form on click.
    (e/When show-button (Button (-> props (assoc :type :reset) (dissoc :show-button))))
    ;; Handle form reset
    (->> (dom/On "reset" #(do (.preventDefault %) (.stopPropagation %) (blur-active-form-input! (.-target %)) %) nil)
      (e/Token) ; TODO render error for failed custom :discard command, if any.
      (first) ; drop err
      (Directive! directive edits))))

#?(:cljs
   (defn event-submit-form! [^js e]
     ;; (js/console.log "submit form" {:type (.-type e), :currentTarget (.-currentTarget e), :target (.-target e), :submitter (.-submitter e)}) ; debug why form got submitted
     (some-> e .-target .-form .requestSubmit)))

(defn event-is-from-this-form?
  "State if an event intercepted by an event listener on a form (event's
  currentTarget attribute is a form) originated from the same form or from an
  input in this form. Allows detecting when a form-related event happened in a
  nested form. Undefined behavior if event's `currentTarget` is not a form."
  [e]
  (let [currentTarget (.-currentTarget e)]
    (or (= currentTarget (.-target e))        ; event happened on form itself
      (= currentTarget (.-form (.-target e))) ; event happened on an input in a form
      )))

(e/defn FormSubmit! ; dom/node must be a form
  [directive edits & {:keys [disabled show-button label auto-submit] :as props}]
  (e/client
    ;; Simulate submit by pressing Enter on <input> nodes
    ;; Don't simulate submit if there's a type=submit button. type=submit natively handles Enter.
    (when (and (not show-button) (not disabled))
      (dom/On "keypress" (fn [e] ;; (js/console.log "keypress" dom/node)
                           (when (and (event-is-from-this-form? e) ; not from a nested form
                                   (= "Enter" (.-key e))
                                   (= "INPUT" (.-nodeName (.-target e))))
                             (.preventDefault e) ; prevent native form submission
                             (event-submit-form! e) ; fire submit event
                             nil)) nil))
    (when auto-submit ; Simulate autosubmit
      ;; checkboxes only
      (dom/On "change" (fn [e] ; TODO consider commit on text input blur (as a change event when autosubmit = false)
                         (.preventDefault e)
                         #_(js/console.log "change" dom/node)
                         (when (and (event-is-from-this-form? e) ; not from a nested form
                                 (instance? js/HTMLInputElement (.-target e))
                                 (= "checkbox" (.. e -target -type)))
                           ;; (js/console.log "change" (hash e) e)
                           ;; (pause!)
                           (event-submit-form! e)) e) nil)
      ;; all inputs but checkboxes
      (dom/On "input"  (fn [e]
                         ;; (js/console.log "input" dom/node)
                         (when (and (event-is-from-this-form? e) ; not from a nested form
                                 (instance? js/HTMLInputElement (.-target e))
                                 (not= "checkbox" (.. e -target -type)))
                           ;; (js/console.log "input" e)
                           (event-submit-form! e)) e) nil))

    ;; We handle form validation manually, disable native browser submit prevention on invalid form.
    ;; Also hide native validation UI.
    (dom/On "invalid" #(.preventDefault %) nil {:capture true})

    (let [;; We forward tx-status to submit button, so we need a handle to propagate token state. Triggering "submit" is not enough.
          ;; Unlike discard which is a simple <button type=reset> natively triggering a "reset" event on form, because we don't render success/failure (could be revisited).
          btn-t (when show-button (TxButton! (-> props (assoc :type :submit) (dissoc :auto-submit :show-button))))
          ;; Only authoritative event
          ;; Native browser navigation must always be prevented
          submit-event (dom/On "submit" #(do (.preventDefault %) #_(js/console.log "submit" (hash %) (event-is-from-this-form? %) (clj->js {:node dom/node :currentTarget (.-currentTarget %) :e %}))
                                             (when (event-is-from-this-form? %) %)) nil)
          [t err] (if auto-submit (e/Token edits) (e/Token submit-event))]
      btn-t ; force let branch to force-mount button
      submit-event ; always force-mount submit event handler, even if auto-submit=true, to prevent browser hard navigation on submit.
      (e/When (and t edits) ; in principle submit button should be disabled if edits = ∅. But semantics must be valid nonetheless.
        (Directive! directive edits (unify-t t btn-t))))))

(e/defn FormSubmitGenesis!
  "Spawns a new tempid/token for each submit. You must monitor the spawned entity's
lifecycle (e.g. for errors) in an associated optimistic collection view!"
  [directive form & {:keys [disabled show-button label #_auto-submit] :as props}] ; auto-submit unsupported
  (e/amb
    ;; TODO unify ButtonGenesis! and Button!
    (e/When show-button (ButtonGenesis! directive :disabled disabled :label label :form form)) ; button will intercept submit events to act as submit!
    ; But, button may hidden, so no default submit, so we need to explicitly handle this also
    (e/for [[btn-q _e] (dom/On-all "submit" #(do (.preventDefault %) (.stopPropagation %)
                                                 (when-not disabled (doto % (js/console.log 'FormSubmitGenesis!-submit)))))]
      ;; TODO logic duplicated in ButtonGenesis!
      (let [[form-t form-v] (e/snapshot form)] ; snapshot to detach form before any reference to form, or spending the form token would loop into this branch and cause a crash.
        (form-t) ; immediately detach form
        [(fn token ; proxy genesis
           ([] (btn-q) #_(form-t))
           ([err] '... #_(btn-q err)))
         ;; abandon entity and clear form, ready for next submit -- snapshot to avoid clearing concurrent edits
         [directive form-v]]))))

(defn invert-fields-to-form [edit-merge edits]
  (when (seq edits)
    (let [ts (map first edits)
          kvs (map second edits)
          validations (not-empty (remove nil? (map #(nth % 2 nil) edits)))
          dirty-form (not-empty (apply edit-merge kvs)) ; collect fields into form, retain until commit/discard
          #_#_dirty-form-guess (apply merge (e/as-vec guess)) ; todo collisions
          form-t (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                   ([] (doseq [t ts] (t)))
                   ([err] #_(doseq [t ts] (t err ::keep)))) ; we could route errors to dirty fields, but it clears dirty state
          ]
      [form-t dirty-form validations])))

(comment

  (invert-fields-to-form merge [])
  (invert-fields-to-form merge [[#() {:a "a"}] [#() {:b "b"}]])
  := [_ {:a "a", :b "b"}]

  (invert-fields-to-form merge [[#() {:a "a"} "invalid a"] [#() {:b "b"} "invalid b"]])
  := [_ {:a "a", :b "b"} ["invalid a" "invalid b"]]
  )

(defn debug-cleanup-form-edit [[_cmd & _args :as form-edit]]
  (when form-edit
    (update form-edit 0 (fn [cmd]
                          (try (contrib.data/unqualify cmd)
                               (catch #?(:clj Throwable, :cljs :default) _
                                 cmd))))))

(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    edits ; concurrent edits are what give us dirty tracking
    & {:as props}]
   (e/client
     (let [{::keys [debug commit ; :commit fn must be side-effect free, :debug true will call :commit on every edit and present the result to the user
                    discard show-buttons auto-submit edit-merge genesis name edit-monoid
                    Validate
                    #_InspectState]}
           (auto-props props {::debug false ::show-buttons true ::edit-merge merge ::genesis false ::edit-monoid hash-map, ::Validate (e/fn [_]) #_#_::InspectState (e/fn [_])})
           dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           show-buttons (cond
                          (boolean? show-buttons) show-buttons
                          (nil? show-buttons) false
                          (= ::smart (qualify show-buttons)) (not clean?))

           [form-t form-v field-validation :as form]
           (invert-fields-to-form edit-merge (e/as-vec edits))

           !form-validation (atom ::nil)
           form-validation (e/watch !form-validation)

           !tx-rejected-error (atom nil)
           tx-rejected-error (e/watch !tx-rejected-error)

           [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                 ::commit form :label "commit"  :disabled (or clean? field-validation)
                                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                                 :show-button show-buttons)
           [_ _ :as ?d] (FormDiscard! ::discard form :disabled clean? :label "discard" :show-button show-buttons)]
       (dom/p (dom/props {:data-role "errormessage"})
              (when (not= form-validation ::nil) (dom/text form-validation))
              (when tx-rejected-error (dom/text tx-rejected-error)))
       (e/amb
         (e/for [[btn-q [cmd form-v]] (e/amb ?cs ?d)]
           (case cmd ; does order of burning matter?
             ;; FIXME clicking discard while commit is busy turns submit button green
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             ;; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                             ;; FIXME can't distinguish between successful commit or discarded busy commit. Button will turn green in both cases. Confusing UX.
                             (partial (fn [ts] (doseq [t ts] (t))) (e/as-vec tempids))
                             ]
                         (case genesis
                           true (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                  (case (btn-q) (e/amb)) ; discard now and swallow cmd, we're done
                                  [btn-q ; its a t
                                   (nth discard 0) ; command
                                   (nth discard 1)]) ; prediction

                                        ; reset form and BOTH buttons, cancelling any in-flight commit
                           false
                           (let [t (after-ack btn-q #(do (clear-commits) (reset! !form-validation ::nil)))]
                             (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                               (case (t) (e/amb)) ; discard now and swallow cmd, we're done
                               [t
                                (nth discard 0) ; command
                                (nth discard 1)]))))
             ::commit (let [form-v form-v ; dirty subset
                            tempid btn-q
                            [form-v guess] (if commit
                                             [(commit form-v tempid #_dirty-form-guess)] ; guess and form would be =
                                             [form-v #_{e dirty-form}])] ; no entity id, only user can guess
                        (let [validation-message (Validate form-v)]
                          (reset! !form-validation validation-message)
                          (if validation-message
                            (do (btn-q  ; err value gets dropped, but form won't reset.
                                  ::invalid) ; controls interpret ::invalid as "validation error", not tx-rejected
                                (e/amb))
                            (let [t (case genesis
                                      true (do (reset-active-form-input! dom/node) btn-q) ; this is a q, transfer to list item
                                      false btn-q)] ; this is a t
                              [(unify-t t (fn ([] (reset! !tx-rejected-error nil))
                                            ([err] (reset! !tx-rejected-error err))))
                               (if name (edit-monoid name form-v) form-v) ; nested forms as fields in larger forms
                               guess]))))))

         (let [commit-edit (if commit (mapv debug-cleanup-form-edit [(commit form-v "-1")]) form-v)
               form-v-edit (if name (edit-monoid name commit-edit) commit-edit)]
           (e/When debug
             (dom/span (dom/text " " dirty-count " dirty"))
             (dom/pre #_(dom/props {:style {:min-height "4em"}})
                      (dom/text (pprint-str (if (= :verbose debug)
                                              {:fields form-v, :expected-commit form-v-edit, :fields-validation field-validation, :form-validation form-validation}
                                              form-v-edit)
                                  :margin 80))))))))))

(defmacro Form! [fields1 & {:as props}] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form events. e.g. submit, reset, invalid, etc…
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (let [props# ~props]
       (FormStatus
         (Form!* ~fields1 (dissoc props# :Accepted :Rejected :Busy)) ; place fields inside dom/form
         (select-keys props# [:Accepted :Rejected :Busy])))))


(e/defn FormStatus [edits & {:keys [Busy Accepted Rejected]
                             :or {Busy (e/fn [])
                                  Accepted (e/fn [])
                                  Rejected (e/fn [err])}}]
  (let [busy? (pos? (e/Count edits))
        !last-state (atom nil)]
    (when busy? (Busy)) ; Busy can happen in parallel in case of re-submit
    (let [[state message] (e/watch !last-state)]
      (case state
        ::accepted (Accepted)
        ::rejected (Rejected message)
        ()))
    (e/for [[t & args] edits]
      (into [(unify-t t
               (fn ([] (reset! !last-state [::accepted]))
                 ([err] (reset! !last-state [::rejected err]))))]
        args))))

;;; Experiments

#_(e/defn Reconcile-records [stable-kf sort-key as bs]
    (e/client
      (let [as! (e/as-vec as) ; todo differential reconciliation
            bs! (e/as-vec bs)]
        (js/console.log "Reconcile" {:as as! :bs bs!})
        (->> (merge-with (fn [a b] (or a b)) ; FIXME WIP this is only valid for create-new ; todo deep merge partial prediction (incl. edits)
               (index-by stable-kf as!)
               (index-by stable-kf bs!))
          vals
          (sort-by sort-key)
          #_(drop (count bs!)) ; todo fix glitch
          (e/diff-by stable-kf)))))

#_(e/declare Service)

#_(e/defn PendingController [stable-kf sort-key forms xs]
    (let [!pending (atom {}) ; [id -> guess]
          ps (val (e/diff-by key (e/watch !pending)))]
      (e/for [[t cmd guess :as form] forms #_(Service forms)]
        (prn 'PendingController cmd guess)
        ((fn [] (assert (<= (count guess) 1))))
        (let [[tempid guess] (first guess)]
          (case guess
            nil nil ; guess is optional
            ::retract nil ; todo
            (do (swap! !pending assoc tempid (assoc guess ::pending form))
                (e/on-unmount #(swap! !pending dissoc tempid)) ; FIXME hangs tab with infinite loop
                ))
          (e/amb)))
      (Reconcile-records stable-kf sort-key xs ps)))

#_(e/declare effects*)

#_(defmacro try-ok [& body] ; fixme inject sentinel
    `(try ~@body ::ok ; sentinel
          (catch Exception e# (doto ::fail (prn e#)))))

#_(e/defn Service [forms]
    (e/client ; client bias, t doesn't transfer
      (prn `Service (e/Count forms) 'forms (e/as-vec (second forms)))
      (e/for [[t form guess] forms]
        (let [[effect & args] form
              Effect (effects* effect (e/fn Default [& args] (doto ::effect-not-found (prn effect))))
              res (e/Apply Effect args)] ; effect handlers span client and server
          (prn 'final-res res)
          (case res
            nil (prn `res-was-nil-stop!)
            ::ok (t) ; sentinel, any other value is an error
            (t ::rejected)))))) ; feed error back into control to prompt for retry
