;; Fork of electric_forms0
;; - Add form-level validation
;; - Different form architecture, should be transparent to user
;; - can be swapped in place of forms0/Form!

(ns hyperfiddle.electric-forms1
  #?(:cljs (:require-macros hyperfiddle.electric-forms1))
  (:require [contrib.data :refer [auto-props qualify]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms0 :as forms0]))

#?(:cljs (defn- active-form-input [form]
           (when-let [focused-input (.-activeElement js/document)]
             (when (.contains form focused-input)
               focused-input))))

#?(:cljs (defn- blur-active-form-input! [form] (some-> (active-form-input form) (.blur))))

#?(:cljs (defn- reset-active-form-input! [form]
           (when-let [input (active-form-input form)]
             (set! (.-value input) ""))))

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


(e/defn FormDiscard! ; dom/node must be a form
  [directive & {:keys [disabled show-button label form] :as props}]
  (e/client
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    (e/When show-button
      (let [[t err] (e/apply forms0/Button! directive (mapcat identity (-> props (dissoc :form) ; if we don't dissoc form, both the button and FormDiscard will try to burn the token and we get an NPE - seems like the `when true` bug.
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


(e/defn FormSubmit! ; dom/node must be a form
  [directive & {:keys [disabled show-button label auto-submit form] :as props}]
  (e/client
    ;; Simulate submit by pressing Enter on <input> nodes
    ;; Don't simulate submit if there's a type=submit button. type=submit natively handles Enter.
    (when (and (not show-button) (not disabled))
      (dom/On "keypress" (fn [e] (let [target (.-target e)] ; submit form on enter
                                   (when (and (= "Enter" (.-key e)) (= "INPUT" (.-nodeName target)))
                                     (.stopPropagation e)
                                     (.requestSubmit (.-form target)) ; fire submit event
                                     nil))) nil))
    ;; Simulate autosubmit
    (when auto-submit
      (dom/On "change" (fn [e] (some->  e .-target .-form .requestSubmit) e) nil) ; checkboxes
      (dom/On "input"  (fn [e] (some->  e .-target .-form .requestSubmit) e) nil) ; text inputs
      )
    (if-let [t
             ;; FIXME pressing Enter while an autosubmit commit is running will trigger a double submit and hang the app
             (let [[btn-t btn-err :as btn] (e/When show-button (e/apply forms0/Button! directive (mapcat identity (-> props (dissoc :form :auto-submit :show-button) (assoc :type :submit))))) ; genesis ; (e/apply Button directive props) didn't work - props is a map
                   submit-event (dom/On "submit" #(doto % (.preventDefault)) nil)
                   [t err] (e/Token submit-event)]
               btn ; force let branch
               (dom/props {:aria-invalid (some? err)})
               (when t
                 (when btn-t (dom/props ((fn [^js e] (.-submitter e)) submit-event) {:disabled "true"})) ; hard-disable submitter button while form submits
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
    (e/When show-button (forms0/ButtonGenesis! directive :disabled disabled :label label :form form)) ; button will intercept submit events to act as submit!
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

(comment
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
                                    :margin 80)))))))))))

(defmacro Form! [fields1 & kwargs] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form!* ~fields1 ~@kwargs))) ; place fields inside dom/form

;; -----
(def report-validity? false)
(def valid? true)
(def validation-message nil)
(def !fields-validity (atom {}))
(def fields-valid? true)

(e/defn RegisterFieldValidity [name valid?]
  (swap! !fields-validity assoc name valid?)
  (e/on-unmount #(do (swap! !fields-validity dissoc name))))

(defn check-validity [name value required? user-validation-message]
  (or (not-empty user-validation-message)
    (when (and required? (nil? value))
      (str "Required"))))

;; TODO should probably move to respective inputs
;; validity should probably be set to input dom/node with .setCustomValidity
;; parent form could inspect inputs validity with form.elements[index].validity.valid and input.checkValidity()
;; But not clear how to render an error messages correctly placed next to an input is the validation message value is not in scope…
;; Also might not really be backed by actual inputs (e.g. sub id in block-sub-from-school, comes from the route - no picker.

(e/defn Field!* [{::keys [name Validate required?] :as props} Body]
  (let [!validation (atom nil)
        validation (e/watch !validation)]
    (binding [valid? (empty? validation), validation-message (not-empty validation)]
      (RegisterFieldValidity name valid?)
      (let [{::keys [name Validate required?] :as props} (auto-props props {::Validate (e/fn [_name _value] nil), ::required? false})
            [token value] (Body)
            value (first (e/as-vec (first (vals value)))) ; FIXME remove name from controls
            ]
        (reset! !validation (check-validity name value required? (Validate name value)))
        [token {name value}]))))

(defmacro Field! [props & body] `(Field!* ~props (e/fn [] ~@body)))

(defn fields-validity [fields] (every? true? (vals fields)))

(e/defn Form1!* [props Body]
  (let [!report-validity? (atom false)]
    (binding [!fields-validity (atom {})
              report-validity? (e/watch !report-validity?)]
      (binding [fields-valid? (fields-validity (e/watch !fields-validity))]
        (let [!validation (atom nil)
              validation  (e/watch !validation)]
          (binding [valid? (and fields-valid? (= ::ok validation))
                    validation-message validation]
            (dom/form ; for semantics, "submit" and "reset" events
              ;; (dom/On "submit" #(do (reset! !report-validity? true)) false {:capture true})
              (let [{::keys [edit-merge edit-monoid Validate commit name InspectState
                             ;; vvvvvvv
                             show-buttons auto-submit discard commit genesis debug
                             ;; ^^^^^^^
                             ]}
                    (auto-props props {::edit-merge merge, ::edit-monoid hash-map, ::Validate (e/fn [_value] (prn "form valid?" _value) nil) ::InspectState (e/fn [_])
                                       ;; vvvvvvv
                                       ::show-buttons true, ::genesis false
                                       ;; ^^^^^^^
                                       })
                    edits (Body)
                    dirty-count (e/Count edits)
                    [form-t form-v :as form] (invert-fields-to-form edit-merge (e/as-vec edits))
                    ;; vvvvvvv
                    clean? (zero? dirty-count)
                    show-buttons (cond
                                   (boolean? show-buttons) show-buttons
                                   (= ::smart (qualify show-buttons)) (not clean?))
                    [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                          ::commit
                                          :label "commit"
                                          :disabled (doto (or clean? #_(and report-validity? (not valid?))) (prn "DISABLED"))
                                          :form form
                                          :auto-submit auto-submit ; (when auto-submit dirty-form)
                                          :show-button show-buttons)
                    [_ _ :as ?d] (FormDiscard! ::discard :form form :disabled clean? :label "discard" :show-button show-buttons)
                    ;; ^^^^^^^^
                    ]
                (InspectState form-v)
                ;; vvvvvvvv
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
                      ::commit (cond
                                 (and (not report-validity?) (not valid?))
                                 (do (reset! !report-validity? true)
                                     (btn-q "invalid")
                                     (e/amb))

                                 ()
                                 (do (case (reset! !validation (Validate form-v))
                                       ::ok
                                       (let [form-v form-v ; dirty subset
                                             tempid btn-q
                                             [form-v guess] (if commit
                                                              (commit form-v tempid #_dirty-form-guess) ; guess and form would be =
                                                              [form-v #_{e dirty-form}]) ; no entity id, only user can guess
                                             t (case genesis
                                                 true (do (reset-active-form-input! dom/node) btn-q) ; this is a q, transfer to list item
                                                 false btn-q)] ; this is a t
                                         [(fn
                                            ([] (reset! !report-validity? false) (reset! !validation nil) (t))
                                            ([err] (reset! !report-validity? false) (reset! !validation nil) (t err)))
                                          (if name (edit-monoid name form-v) form-v) ; nested forms as fields in larger forms
                                          guess])
                                       (do (btn-q "invalid")
                                           (e/amb)))))))

                  (let [commit-edit (if commit (mapv debug-cleanup-form-edit (commit form-v "-1")) form-v)
                        form-v-edit (if name (edit-monoid name commit-edit) commit-edit)]
                    (InspectFormCommit form-v-edit)
                    (e/When debug
                      (dom/span (dom/text " " dirty-count " dirty"))
                      (dom/pre #_(dom/props {:style {:min-height "4em"}})
                               (dom/text (pprint-str (if (= :verbose debug)
                                                       {:fields form-v, :expected-commit form-v-edit}
                                                       form-v-edit)
                                           :margin 80)))))
                  ;; ^^^^^^^^
                  #_(dom/pre (dom/text (contrib.str/pprint-str {:dirty-count dirty-count, :form-v form-v, :fields-valid? fields-valid?, :fields-validity (e/watch !fields-validity)
                                                              :form-validity validation}))))))))))))

(defmacro Form1! [props & body] `(Form1!* ~props (e/fn [] ~@body)))