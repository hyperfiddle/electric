(ns hyperfiddle.electric-forms0
  #?(:cljs (:require-macros hyperfiddle.electric-forms0))
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
        (if (dom/Focused?) ; "don't damage user input"
          (dom/On "input" #(-> % .-target .-value (subs 0 maxlength) parse) (e/snapshot (str v)))
          (set! (.-value dom/node) (str v)))))))

(e/defn Checkbox [checked & {:keys [id label parse] :as props
                             :or {id (random-uuid) parse identity}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id})
        (dom/props (dissoc props :id :label :parse))
        ;; Dataflow circuit Checkbox won't track user focused state - unlike Input and transactional Checkbox!.
        ;; Because:
        ;;  - "don't damage user input" is well defined for tx controls (token span), not so much for a dataflow checkbox.
        ;;  - Safari don't focus checkboxes on click (only text inputs, so to match overall macos behavior)
        ;;    - Ticket: https://www.notion.so/hyperfiddle/electric-forms0-Checkbox-does-not-work-on-Safari-both-mac-and-ios-16fb4d1e85d180d69249e2630a063485?pvs=4
        ;; Alternatives:
        ;;  - browser-specific behavior
        ;;  - ?
        (e/amb (dom/On "change" #(-> % .-target .-checked parse) (e/snapshot checked))
          (set! (.-checked dom/node) checked)))
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

;; Transactional inputs
; Errors are forwarded in via token callback
; Errors are never sent out via signal, because consumers already saw it when they forwarded it in

(e/defn Input! [field-name ; fields are named like the DOM, <input name=...> - for coordination with form containers
                v & {:keys [maxlength type parse edit-monoid] :as props
                     :or {maxlength 100 type "text" parse identity edit-monoid hash-map}}]
  (e/client
    (dom/input (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
      (let [e (dom/On "input" identity nil) [t err] (e/Token e) ; reuse token until commit
            editing? (dom/Focused?)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)]
        (when-not dirty? (set! (.-value dom/node) v)) ; todo - submit must reset input while focused
        (when error? (dom/props {:aria-invalid true}))
        (when waiting? (dom/props {:aria-busy true}))
        (if waiting?
          (let [v' ((fn [] (-> e .-target .-value (subs 0 maxlength) parse)))
                edit (edit-monoid field-name v')] ; named field edit, a KV structure
            [t edit]) ; edit request, bubbles upward to service
          (e/amb)))))) ; return nothing, not nil - because edits are concurrent, also helps prevent spurious nils

(e/defn Checkbox! [k checked & {:keys [id type label parse edit-monoid] :as props
                                :or {id (random-uuid) type :checkbox parse identity edit-monoid hash-map}}]
  ; todo esc?
  (e/client
    (e/amb
      (let [[e t err input-node]
            (dom/input (dom/props {:type type, :id id}) (dom/props (dissoc props :id :label :parse))
                       (let [e (dom/On "change" identity nil) [t err] (e/Token e)] ; single txn, no concurrency
                         [e t err dom/node]))
            editing? (dom/Focused? input-node)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)]
        (when-not dirty? (set! (.-checked input-node) checked))
        (when error? (dom/props input-node {:aria-invalid true}))
        (when waiting? (dom/props input-node {:aria-busy true}))
        (if waiting? [t (edit-monoid k ((fn [] (-> e .-target .-checked parse))))] (e/amb)))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn RadioGroup [edits]
  (e/client
    (let [!only-edit (atom nil)
          [t edit] (e/watch !only-edit)]
      (swap! !only-edit (fn [[t _edit :as old] new] (when old (t)) new) edits)
      (if-not t
        (e/amb)
        [(fn ([] (t) (reset! !only-edit nil))
           ([err] (t err) (reset! !only-edit nil)))
         edit]))))

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
      (dom/props {:aria-invalid (some? err)})
      (dom/props {:class [(when (and (some? x) (nil? btn-t) (nil? err)) "hf-success")]})
      (if btn-t
        (let [[form-t form-v] form]
          [(fn token
             ([] (btn-t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
             ([err] (btn-t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
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
          (prn "click discard" form)
          [(fn token
             ([] (t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
             ([err] (t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
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

    (if-let [t
          ;; FIXME pressing Enter while an autosubmit commit is running will trigger a double submit and hang the app
          (let [[btn-t btn-err :as btn] (when show-button (e/apply Button! directive (mapcat identity (-> props (dissoc :form :auto-submit :show-button) (assoc :type :submit))))) ; genesis ; (e/apply Button directive props) didn't work - props is a map
                ^js submit-event (dom/On "submit" #(do (.preventDefault %) ; always prevent browser native navigation
                                                   ;; (js/console.log "submit" (hash %) (event-is-from-this-form? %) (clj->js {:node dom/node :currentTarget (.-currentTarget %) :e  %}))
                                                   (when (event-is-from-this-form? %) %))
                               nil)
                [t err] (if auto-submit (e/Token form) (e/Token submit-event))]
            btn submit-event ; force let branch
            (dom/props {:aria-invalid (some? err)})
            (when (and t form)
              (when (and btn-t (submitter submit-event))
                (dom/props (submitter submit-event) {:disabled true})) ; hard-disable submitter button while form submits
              (fn
                ([] (t) (when btn-t (btn-t)))
                ([err] (t err) (when btn-t (btn-t err))))))]
      ; TODO unify with FormSubmit! and Button!
      (let [[form-t form-v] form]
        [(fn token
           ([] (t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
           ([err] (t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
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
      (e/on-unmount #(prn "unmount genesis branch"))
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
          dirty-form (not-empty (apply edit-merge kvs)) ; collect fields into form, retain until commit/discard
          #_#_dirty-form-guess (apply merge (e/as-vec guess)) ; todo collisions
          form-t (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                   ([] (doseq [t ts] (t)))
                   #_([err] (doseq [t ts] (t err ::keep)))) ; we could route errors to dirty fields, but it clears dirty state
          ]
      [form-t dirty-form])))

(comment

  (invert-fields-to-form merge [])
  (invert-fields-to-form merge [[#() {:a "a"}] [#() {:b "b"}]])
  := [_ {:a "a", :b "b"}])

(defn call [f] (f))

(defn debug-cleanup-form-edit [[_cmd & _args :as form-edit]]
  (when form-edit
    (update form-edit 0 (fn [cmd]
                          (try (contrib.data/unqualify cmd)
                               (catch #?(:clj Throwable, :cljs :default) _
                                 cmd))))))

(e/defn InspectFormCommit [_commit]) ; to be rebound in dynamic scope

(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    edits ; concurrent edits are what give us dirty tracking
    & {:as props}]
   (e/client
     (let [{::keys [debug commit ; :commit fn must be side-effect free, :debug true will call :commit on every edit and present the result to the user
                    discard show-buttons auto-submit edit-merge genesis name edit-monoid
                    InspectState]}
           (auto-props props {::debug false ::show-buttons true ::edit-merge merge ::genesis false ::edit-monoid hash-map, ::InspectState (e/fn [_])})
           dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           show-buttons (cond
                          (boolean? show-buttons) show-buttons
                          (nil? show-buttons) false
                          (= ::smart (qualify show-buttons)) (not clean?))
           [form-t form-v :as form] (invert-fields-to-form edit-merge (e/as-vec edits))
           [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                 ::commit :label "commit"  :disabled clean?
                                 :form form
                                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                                 :show-button show-buttons)
           [_ _ :as ?d] (FormDiscard! ::discard :form form :disabled clean? :label "discard" :show-button show-buttons)]
       (InspectState form-v)
       (e/amb
         (e/for [[btn-q [cmd form-v]] (e/amb ?cs ?d)]
           (case cmd ; does order of burning matter?
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             (partial #(run! call %) (e/as-vec tempids))] ; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                         (case genesis
                           true (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                  (case (btn-q) (e/amb)) ; discard now and swallow cmd, we're done
                                  [btn-q ; its a t
                                   (nth discard 0) ; command
                                   (nth discard 1)]) ; prediction

                           ; reset form and BOTH buttons, cancelling any in-flight commit
                           false (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                   (case (do (btn-q) (clear-commits) (e/amb))) ; discard now and swallow cmd, we're done
                                   [(fn token
                                      ([] (btn-q) (clear-commits))
                                      ([err] (btn-q err)))
                                    (nth discard 0) ; command
                                    (nth discard 1)])))
             ::commit (let [form-v form-v ; dirty subset
                            tempid btn-q
                            [form-v guess] (if commit
                                             (commit form-v tempid #_dirty-form-guess) ; guess and form would be =
                                             [form-v #_{e dirty-form}]) ; no entity id, only user can guess
                            t (case genesis
                                true (do (reset-active-form-input! dom/node) btn-q) ; this is a q, transfer to list item
                                false btn-q)] ; this is a t
                        [t
                         (if name (edit-monoid name form-v) form-v) ; nested forms as fields in larger forms
                         guess])))

         (let [commit-edit (if commit (mapv debug-cleanup-form-edit (commit form-v "-1")) form-v)
               form-v-edit (if name (edit-monoid name commit-edit) commit-edit)]
           (InspectFormCommit form-v-edit)
           (e/When debug
             (dom/span (dom/text " " dirty-count " dirty"))
             (dom/pre #_(dom/props {:style {:min-height "4em"}})
                      (dom/text (pprint-str (if (= :verbose debug)
                                              {:fields form-v, :expected-commit form-v-edit}
                                              form-v-edit)
                                  :margin 80))))))))))

(defmacro Form! [fields1 & kwargs] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form!* ~fields1 ~@kwargs))) ; place fields inside dom/form

(e/defn Reconcile-records [stable-kf sort-key as bs]
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

(e/declare Service)

(e/defn PendingController [stable-kf sort-key forms xs]
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

(e/declare effects* #_{})

(defmacro try-ok [& body] ; fixme inject sentinel
  `(try ~@body ::ok ; sentinel
     (catch Exception e# (doto ::fail (prn e#)))))

(e/defn Service [forms]
  (e/client ; client bias, t doesn't transfer
    (prn `Service (e/Count forms) 'forms (e/as-vec (second forms)))
    (e/for [[t form guess] forms]
      (let [[effect & args] form
            Effect ((or effects* {}) effect (e/fn Default [& args] (doto ::effect-not-found (prn effect))))
            res (e/Apply Effect args)] ; effect handlers span client and server
        (prn 'final-res res)
        (case res
          nil (prn `res-was-nil-stop!)
          ::ok (t) ; sentinel, any other value is an error
          (t ::rejected)))))) ; feed error back into control to prompt for retry
