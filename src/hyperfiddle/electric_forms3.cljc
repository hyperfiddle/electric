(ns hyperfiddle.electric-forms3
  #?(:cljs (:require-macros hyperfiddle.electric-forms3))
  (:require [contrib.data :refer [index-by auto-props qualify]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

;; Simple controlled inputs (dataflow circuits)

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
          (if (dom/Focused?)
            (dom/On "change" #(-> % .-target .-checked parse) (e/snapshot checked))
            (set! (.-checked dom/node) checked))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

;; Simple uncontrolled inputs (sugar for the unvarying literal case)

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

;; Transactional inputs
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
  ([t1 t2 & ts] (unify-t (unify-t t1 t2) (apply unify-t ts))))

(defn after-ack [t1 f] (unify-t t1 (fn [& [_err]] (f))))
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
            editing? (dom/Focused? input-node)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)
            v (if waiting? ((fn [] (-> e .-target .-checked parse))) checked)
            validation-message (Validate v)]
        (when-not dirty? (set! (.-checked input-node) checked))
        (when error? (dom/props input-node {:aria-invalid true}))  ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (InputValidity input-node validation-message)
        (e/When waiting?
          (dom/props input-node {:aria-busy true})
          [t (edit-monoid k v) validation-message]))
      (e/When label (dom/label (dom/props {:for id}) (dom/text (label k)))))))

(e/defn LatestEdit [edits]
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
  (let [!selected (atom [nil authoritative-v nil])
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
    (e/When t
      (let [!authoritative-v (atom (e/snapshot authoritative-v))]
        (reset! !authoritative-v authoritative-v)
        [(after-ack t (fn [& _] (reset! !selected [nil @!authoritative-v nil])))
         (edit-monoid k v)
         errors]))))

(e/defn Button!
  "Transactional button with busy state. Disables when busy."
  [directive & {:keys [label disabled type form] :as props
                :or {type :button}}] ; default type in form is submit
  (dom/button (dom/text label) ; (if err "retry" label)
    (dom/props (-> props (dissoc :label :disabled) (assoc :type type)))
    (let [x (dom/On "click" identity nil) ; (constantly directive) forbidden - would work skip subsequent clicks
          [btn-t err] (e/Token x)] ; genesis
      (if disabled  ; don't set :disabled on <input type=submit> WHILE submit event bubbles, it prevents form submission
        (dom/props {:disabled disabled})
        (dom/props {({:submit :aria-disabled} type :disabled) (some? btn-t)}))
      (dom/props {:aria-busy (some? btn-t)})
      (dom/props {:aria-invalid (#(and (some? err) (not= err ::invalid)) err)}) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
      (dom/props {:data-tx-status (when (and (some? x) (nil? btn-t) (nil? err)) "accepted")})
      (if btn-t
        (let [[form-t form-v] form]
          [(after-ack btn-t ; reset controlled form and both buttons, cancelling any in-flight commit
             (fn [& _] (when form-t ; redirect error to button ("retry"), drop error, leave uncommitted form dirty
                         (form-t))))
           (if form-t [directive form-v] directive)]) ; compat
        (e/amb))))) ; None or Single

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

;; Forms

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
  [directive & {:keys [disabled show-button label form] :as props}]
  (e/client
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    (e/When show-button
      (let [[t err] (e/apply Button! directive (mapcat identity (-> props (dissoc :form) ; if we don't dissoc form, both the button and FormDiscard will try to burn the token and we get an NPE - seems like the `when true` bug.
                                                                  (assoc :type :reset))))]
        (t))) ; always safe to call, Button returns [t err] or (e/amb)
    (let [[t err] (e/Token (dom/On "reset" #(do #_(.log js/console %) (.preventDefault %)(.stopPropagation %)
                                                     (blur-active-form-input! (.-target %)) %) nil))]
      (if t ; TODO unify with FormSubmit! and Button!
        (let [[form-t form-v] form]
          [(unify-t t form-t) ; reset controlled form and both buttons, cancelling any in-flight commit
           (if form-t [directive form-v] [directive])]) ; compat
        (e/amb)))))

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

#?(:cljs (defn submitter [^js event] (.-submitter event))) ; sole purpose is to prevent inference warning on .-submitter

(e/defn FormSubmit! ; dom/node must be a form
  [directive & {:keys [disabled show-button label auto-submit form] :as props}]
  (e/client
    ;; Simulate submit by pressing Enter on <input> nodes
    ;; Don't simulate submit if there's a type=submit button. type=submit natively handles Enter.
    (when (and (not show-button) (not disabled))
      (dom/On "keypress" (fn [e]
                           ;; (js/console.log "keypress" dom/node)
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
                         (js/console.log "change" dom/node)
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

    (dom/On "invalid" #(.preventDefault %) nil {:capture true})

    (if-let [t
          ;; FIXME pressing Enter while an autosubmit commit is running will trigger a double submit and hang the app
             (let [[btn-t btn-err :as btn] (when show-button (Button! directive (-> props (dissoc :form :auto-submit :show-button) (assoc :type :submit)))) ; genesis ; (e/apply Button directive props) didn't work - props is a map
                ^js submit-event (dom/On "submit" #(do (.preventDefault %) ; always prevent browser native navigation
                                                       ;; (js/console.log "submit" (hash %) (event-is-from-this-form? %) (clj->js {:node dom/node :currentTarget (.-currentTarget %) :e  %}))
                                                       (when (event-is-from-this-form? %) %))
                                   nil)
                [t err] (if auto-submit (e/Token form) (e/Token submit-event))]
            btn submit-event ; force let branch
            (when (and t form)
              (when (and btn-t (submitter submit-event))
                (dom/props (submitter submit-event) {:disabled true})) ; hard-disable submitter button while form submits
              (unify-t t btn-t)))]
      ; TODO unify with FormSubmit! and Button!
      (let [[form-t form-v] form]
        [(fn ([] (t) (form-t)) ; reset controlled form and both buttons, cancelling any in-flight commit
           ([err] (t err))) ; redirect tx-error only to button ("retry"), leave uncommitted form dirty
         (if form-t [directive form-v] [directive nil])]) ; compat
      (e/amb))))

(e/defn FormSubmitGenesis!
  "Spawns a new tempid/token for each submit. You must monitor the spawned entity's
lifecycle (e.g. for errors) in an associated optimistic collection view!"
  [directive & {:keys [disabled show-button label form #_auto-submit] :as props}] ; auto-submit unsupported
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

(defn call [f] (f))

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
                                 ::commit :label "commit"  :disabled (e/Reconcile (or clean? field-validation)) ; FIXME e/Reconcile necessary to prevent Button! trashing
                                 :form form
                                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                                 :show-button show-buttons)
           [_ _ :as ?d] (FormDiscard! ::discard :form form :disabled clean? :label "discard" :show-button show-buttons)]
       (dom/p (dom/props {:data-role "errormessage"})
              (when (not= form-validation ::nil) (dom/text form-validation))
              (when tx-rejected-error (dom/text tx-rejected-error)))
       (e/amb
         (e/for [[btn-q [cmd form-v]] (e/amb ?cs ?d)]
           (case cmd ; does order of burning matter?
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             (partial #(run! call %) (e/as-vec tempids)) ; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
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
         (select-keys props# :Accepted :Rejected :Busy)))))


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

;; Experiments

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
