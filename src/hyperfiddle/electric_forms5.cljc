(ns hyperfiddle.electric-forms5
  #?(:cljs (:require-macros hyperfiddle.electric-forms5))
  (:require [contrib.data :refer [auto-props qualify]]
            [dustingetz.str :refer [pprint-str]]
            [clojure.set :as set]
            [missionary.core :as m]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing]]))

;;; Simple controlled inputs (dataflow circuits)

(e/defn Input [v & {:keys [maxlength type parse] :as props
                    :or {maxlength 100 type "text" parse identity}}]
  (e/client
    (dom/input
      (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
      (when-not (dom/Focused?)  ; "don't damage user input"
        (set! (.-value dom/node) (str v)))
      ;; event handler can't be guarded by focus – <input type=number> renders a
      ;; ↑↓ mouse control over the text input to increment/decrement a number.
      ;; Clicking those buttons don't focus the input.
      (dom/On "input" #(-> % .-target .-value (subs 0 maxlength) parse) (str v)) ; (str v) passes through
      )))

(e/defn Checkbox [checked & {:keys [id type label parse] :as props
                             :or {type "checkbox" id (random-uuid) parse identity}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type type, :id id})
        (dom/props (dissoc props :id :label :parse :type))
        ;; Dataflow circuit Checkbox won't track user focused state - unlike Input and transactional Checkbox!.
        ;; Because:
        ;;  - "don't damage user input" is well defined for tx controls (token span), not so much for a dataflow checkbox.
        ;;  - Safari don't focus checkboxes on click (only text inputs, so to match overall macos behavior)
        ;;    - Ticket: https://www.notion.so/hyperfiddle/electric-forms0-Checkbox-does-not-work-on-Safari-both-mac-and-ios-16fb4d1e85d180d69249e2630a063485?pvs=4
        ;; Alternatives:
        ;;  - browser-specific behavior
        ;;  - ?
        (set! (.-checked dom/node) checked)
        (dom/On "change" #(-> % .-target .-checked parse) checked)) ; checked passes through
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

(e/defn SetValidity
  ([throwable] (SetValidity dom/node throwable))
  ([node throwable]
   (let [message (str (ex-message throwable))]
     (when (not-empty message)
       (dom/props node {:data-errormessage message}))
     (.setCustomValidity node message)
     (e/on-unmount #(.setCustomValidity node "")))))

;;; Transactional inputs
; Errors are forwarded in via token callback
; Errors are never sent out via signal, because consumers already saw it when they forwarded it in

(e/defn Identity [a] a)
(e/defn Lift [f] (e/fn [& args] (apply f args)))
(defn form-valid? [x] ; works for all edits - control or form - "edit-valid?" name was judged less natural.
  (not (instance? #?(:clj Throwable, :cljs js/Error) x)))

(e/defn Input! [field-name ; fields are named like the DOM, <input name=...> - for coordination with form containers
                v & {:keys [maxlength type Parse Unparse] :as props
                     :or {maxlength 100, type "text", Parse Identity, Unparse (Lift str)}}]
  (e/client
    (dom/input
      (dom/props (-> props (dissoc :parse :Parse :Unparse) (assoc :maxLength maxlength :type type)))
      (let [e (dom/On "input" identity nil) [t err] (e/Token e) ; reuse token until commit
            editing? (dom/Focused?)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)
            unparsed-v (if waiting? ((fn [] (-> e .-target .-value))) (str (Unparse v))) ; user input has precedence
            parsed-v (Parse (subs unparsed-v 0 maxlength))]
        (SetValidity parsed-v)
        (when-not dirty? (set! (.-value dom/node) unparsed-v)) ; todo - submit must reset input while focused
        (when error? (dom/props {:aria-invalid true})) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (when waiting? (dom/props {:aria-busy true}))
        (e/When waiting? ; return nothing, not nil - because edits are concurrent, also helps prevent spurious nils
          [t {field-name parsed-v}])))))  ; edit request, bubbles upward to interpreter

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

(e/defn Checkbox! [k checked & {:keys [id type label Parse Unparse] :as props
                                :or {id (random-uuid), type :checkbox, Parse Identity, Unparse (Lift boolean)}}]
  ; todo esc?
  (e/client
    (e/amb
      (let [[e t err input-node]
            (dom/input (dom/props {:type type, :id id}) (dom/props (dissoc props :id :label :parse :Parse :Unparse))
                       (let [e (dom/On "change" identity nil) [t err] (e/Token e)] ; single txn, no concurrency
                         [e t err dom/node]))
            editing? (dom/Focused? input-node) ; never true on Safari (MacOs and iOS)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)
            unparsed-v (if waiting? ((fn [] (-> e .-target .-checked))) (Unparse checked)) ; user input has precedence
            parsed-v (Parse unparsed-v)]
        (SetValidity input-node parsed-v)
        (when (or (not dirty?) (#{"radio" :radio} type)) ; Radio's "don't damage user input" behavior handled at radiogroup level.
          (set! (.-checked input-node) unparsed-v)) ; FIXME DOM state isn't set when selecting two radios in a row in the same radiogroup.
        (when error? (dom/props input-node {:aria-invalid true}))  ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (e/When waiting?
          (dom/props input-node {:aria-busy true})
          [t {k parsed-v}]))
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
      (swap! !latest (fn [[old-t _old-kv] [_new-t _new-kv :as new]] (when old-t (old-t)) new) edits))
    (e/When latest latest)))

(e/defn Picker!
  [k selected-v Body &
   {:keys [as Parse Unparse]
    :or {as :div, Parse Identity, Unparse (Lift (constantly false))}
    :as props}]
  (let [Unparse (e/fn [selected-v] (when selected-v {selected-v (Unparse selected-v)}))
        !selected (atom [nil (e/snapshot ; "don't damage user input" – will track selected-v in absence of a token – see below
                               (identity  ; HACK prevent a crash if `selected-v` binding is (e/server ...)
                                 (Unparse selected-v)))])
        [t unparsed-v] (e/watch !selected)]
    (dom/With-element as
      (e/fn []
        (let [parsed-v (Parse unparsed-v)]
          (dom/props {:role "radiogroup", :data-errormessage (not-empty (ex-message parsed-v))})
          (dom/props (dissoc (set/rename-keys props {:required :aria-required}) :Parse :Unparse :as))
          (reset! !selected (Body parsed-v))
          (let [focused? (dom/Focused-in?)]
            focused? ; force event handler - temporary
            ;; TODO use focused? to put focus on aria-checked element on focus enter
            ;; TODO intercept arrow keys to focus next/previous/first/last checkable elements.
            (if (some? t) ; "don't damage user input" – only track authoritative value in absence of a token
              [(after-ack t (fn after [] (swap! !selected assoc 0 nil))) ; [t v] -> [nil v]
               {k parsed-v}]
              (e/When (not focused?)
                (swap! !selected assoc 1 (Unparse selected-v)) ; [nil v1] -> [nil v2]
                (e/amb)))))))))

(e/defn RadioPicker!
  [k selected-v
   & {:keys [as option-label Options Parse Unparse]
      :or   {as :dl, Parse Identity, Unparse Identity}
      :as   props}]
  (let [options (e/as-vec (Options))] ; all options get rendered anyway. Look for TablePicker! otherwise.
    (Picker! k selected-v
      (e/fn [selected-v] ; Unparse must map selected-v to a valid option
            (dom/props {:style {:--radiogroup-items-count (count options)}})
            (LatestEdit
              (e/for [index (IndexRing (count options) 0)]
                (let [x  (get options index)
                      id (random-uuid)]
                  (dom/dt (dom/label (dom/props {:for id}) (dom/text x)))
                  (dom/dd
                    (Checkbox! x (= selected-v x) :id id, :name k, :type :radio, :label option-label, :required (:required props)
                      :Parse (e/fn [_checked?] x) ; true -> k
                      ))))))
      :as as
      :Parse (e/fn [kv] (Parse (some-> kv first key)))
      :Unparse Unparse
      (dissoc props :as :Parse :Unparse :option-label :Options))))

(e/defn TablePicker! ; TODO G: might have damaged optimal siting – verify
  ;; TODO aria-compliant keyboard nav https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/radio_role#keyboard_interactions
  [k selected-v record-count Row
   & {:keys [Parse Unparse row-height]
      :or {Parse Identity, Unparse Identity, row-height 24}
      :as props}]
  (dom/div
    (dom/props {:class "Viewport", #_#_:style {:height "96px"}}) ; TODO cleanup
    (dom/props (dissoc props :Validate :row-height))
    (let [[offset limit] (Scroll-window row-height record-count dom/node {})]
      (e/amb
        (Picker! k selected-v
          (e/fn PickerBody [selected-index] ; Unparse must map selected-v to an index
            (dom/props {:style {:--row-height (str row-height "px") :top (str (* offset row-height) "px")}})
            (LatestEdit
              (e/for [index (IndexRing limit offset)] ; render all rows even when record-count < limit
                (dom/tr
                  (dom/props {:role "radio"
                              :style {:--order (inc index)}
                              :data-row-stripe (mod index 2)}) ; TODO move to parent node + css with nth-child
                  (dom/props {:tabindex "0" :aria-checked (= index selected-index)}) ; tabindex enables focus – items with role=radio must be focusable
                  ;; FIXME e/for forces transfer of return value: prevents site-neutral impl. Returning token forces this entire branch to run on client, and so Row is called on client.
                  (Row index)
                  (let [[t _err] (e/Token (e/amb ; click + space is aria-compliant
                                            (dom/On "click" identity nil) ;;
                                            (dom/On "keypress" #(when (= "Space" (.-code %)) (doto % (.preventDefault))) nil)))]
                    (e/When t [t index]))))))
          :as :table
          :Parse Parse
          :Unparse Unparse
          props)
        (dom/div (dom/props {:style {:height (str (contrib.data/clamp-left ; row count can exceed record count
                                                    (* row-height (- record-count limit)) 0) "px")}}))))))

;;; Edit/action -> command mapping

(defn directive->command
  ([directive token] (when token {::token token, ::name directive}))
  ([directive edit token]
   (when token
     (assoc edit
       ::token (unify-t token (::token edit))
       ::name directive))))

(e/defn Directive!
  ([directive token]
   (e/When token (directive->command directive token)))
  ([directive edit token]
   (e/When token (directive->command directive edit token))))

;;; Buttons

(e/defn Button* [{:keys [label] :as props}]
  (dom/button (dom/text label)
              (dom/props (dissoc props :label))
              [(dom/On "click" identity nil) dom/node]))

(e/defn Button "Simple button, return latest click event or nil."
  [& {:keys [label] :as props}]
  (first (Button* props)))

(e/defn Button!* [& {:keys [label] :as props}]
  (let [[event node] (Button* props)
        [btn-t err] (e/Token event)]
    [btn-t err event node]))

(e/defn TxButton!
  ;; Like `Button!*` with extra semantic markup reflecting tx status.
  ;; Users want `Button!` instead, mapping "button click tx" to a business command.
  "Transactional button with busy state. Disables when busy. To be styled with CSS:
  - button[aria-busy=true]{...} : tx is comitting
  - button[aria-invalid=true]{...} : tx failed
  - button[data-tx-status=accepted] : tx success
  - button:disabled{...} "
  [& {:keys [disabled type] :or {type :button} :as props}]
  (let [[btn-t err event node] (e/Reconcile ; HACK wtf? further props on node will unmount on first click without this
                                 (Button!* (-> props (assoc :type type) (dissoc :disabled))))]
    ;; Don't set :disabled on <input type=submit> before "submit" event has bubbled, it prevents form submission.
    ;; When "submit" event reaches <form>, native browser impl will check if the submitter node (e.g. submit button) has a "disabled=true" attr.
    ;; Instead, let the submit event propagate synchronously before setting :disabled, by queuing :disabled on the event loop.
    (dom/props node {:disabled (e/Task (m/sleep 0 (or disabled (some? btn-t))))})
    (dom/props node {:aria-busy (some? btn-t)})
    (dom/props node {:aria-invalid (#(and (some? err) (not= err ::invalid)) err)}) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
    (dom/props node {:data-tx-status (when (and (some? event) (nil? btn-t) (nil? err)) "accepted")}) ; FIXME can't distinguish between successful tx or tx canceled by clicking discard.
    (e/When btn-t [btn-t err]))) ; forward token to track tx-status ; should it return [t err]?

(e/defn Button! [value & {:keys [Parse] :or {Parse Identity} :as props}] ; User friendly API answering "what does the button do when clicked: it returns [t (Parse value)], ∅ otherwise."
  [(first (TxButton! (dissoc props :Parse))) (Parse value)])

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
  [directive [edits-t edits-kvs] & {:keys [show-button] :as props}]
  (e/client
    ;; reset form on <ESC>
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    ;; Render an <input type=reset>, natively resetting form on click.
    (e/When show-button (Button (-> props (assoc :type :reset) (dissoc :show-button))))
    ;; Handle form reset
    (let [[t _err] (e/Token (dom/On "reset" #(do (.preventDefault %) (.stopPropagation %) (blur-active-form-input! (.-target %)) %) nil)) ; TODO render error for failed custom :discard command, if any.
          #_(Directive! directive edits)]
      (e/When t
        [(unify-t t edits-t) [directive edits-kvs]] ; edits-kvs is unused, but command shape matches FormSubmit for consistency
        ))))

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

(defn stop-err-propagation [token] (when token (fn ([] (token)) ([_err]))))

(e/defn Amb->nil [table]
  (when (e/Some? table)
    table))

(e/defn FormSubmit! ; dom/node must be a form
  [directive [edits-t edits-kvs :as edits] & {:keys [disabled show-button auto-submit genesis] :as props}]
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
    (when (and auto-submit (not genesis)) ; Simulate autosubmit
      ;; G: auto-submit + genesis is not a thing. Only known use case is "Create new row" in a masterlist view.
      ;;    "Create new row" is a Form with a single [+] Button. It cannot collect user input. It creates an empty entity with
      ;;    generated id (imagine what auto-submit+genesis would imply for checkbox or text input). All candidate buttons are
      ;;    therefore submit buttons, and get the "auto-submit" behavior for free, since they are themselves the submit
      ;;    affordance. Therefore the only use case for auto-submit + genesis has no user input to "auto submit" and collapses to
      ;;    just "genesis".
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

    (let [submit-handler #(do (.preventDefault %) #_(js/console.log "submit" (hash %) (event-is-from-this-form? %) (clj->js {:node dom/node :currentTarget (.-currentTarget %) :e %}))
                              (when (and (not disabled) (event-is-from-this-form? %)) %))]
      (if-not genesis
        ;; Regular tx submit – txs are sequential.
        (let [;; We forward tx-status to submit button, so we need a handle to propagate token state. Triggering "submit" is not
              ;; enough. Unlike discard which is a simple <button type=reset> natively triggering a "reset" event on form, because
              ;; we don't render success/failure (could be revisited).
              [btn-t _] (when show-button (TxButton! (-> props (assoc :type :submit) (dissoc :auto-submit :show-button :genesis))))
              ;; Submit is the only authoritative event.
              ;; Native browser navigation must always be prevented
              submit-event (dom/On "submit" submit-handler nil)
              [t _err] (if auto-submit (e/Token edits) (e/Token submit-event))]
          btn-t ; force let branch to force-mount button
          submit-event ; always force-mount submit event handler, even if auto-submit=true, to prevent browser hard navigation on submit.
          (e/When (and t edits) ; in principle submit button should be disabled if edits = ∅. But semantics must be valid nonetheless.
            [(unify-t t edits-t (Amb->nil btn-t)) [directive edits-kvs]]))
        (do ; Genesis case – parallel racing txs. Button cannot report more than one tx status unambiguously. Use regular (non-tx) button to trigger submit.
          (when show-button (Button (-> props (assoc :type :submit, #_#_:data-role "genesis") (dissoc :auto-submit :show-button :genesis))))
          (e/for [[submit-q _err] (dom/On-all "submit" submit-handler nil)]
            (let [[edits-t edits-kvs] (e/snapshot edits)] ; snapshot to detach edits before any reference to edits, or spending the edits token would loop into this branch and cause a crash.
              (edits-t) ; immediately detach edits – clears user input.
              [(stop-err-propagation submit-q) [directive edits-kvs]])))))))

#_
(defn invert-fields-to-form [field-edits]
  (when (seq field-edits)
    {::token (let [tokens (map ::token field-edits)]
               (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                 ([] (doseq [t tokens] (t)))
                 ([_err] #_(doseq [t tokens] (t err ::keep)))))
     ::name ::form
     ::value (not-empty (into {} (map (juxt ::name ::value)) field-edits))  ; collect fields into form, retain until commit/discard
     ::validation (not-empty (remove nil? (map ::validation field-edits)))}))

#_
(comment

  (invert-fields-to-form merge [])
  (invert-fields-to-form merge [[#() {:a "a"}] [#() {:b "b"}]])
  := [_ {:a "a", :b "b"}]

  (invert-fields-to-form merge [[#() {:a "a"} "invalid a"] [#() {:b "b"} "invalid b"]])
  := [_ {:a "a", :b "b"} ["invalid a" "invalid b"]]
  )


(defn merge-edits [edits]
  [(->> (map first edits) ; extract all tokens
     (reduce unify-t) ; unify them into a single ack-all, fan-out token
     (stop-err-propagation) ; severs fan-out of err path, as we cannot report form failure at field level.
     )
   (into {} (map second) edits) ; merge all kvs.
   ])

(defn debug-cleanup-form-edit [[_cmd & _args :as form-edit]]
  (when form-edit
    (update form-edit 0 (fn [cmd]
                          (try (contrib.data/unqualify cmd)
                               (catch #?(:clj Throwable, :cljs :default) _
                                 cmd))))))

#_
(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    name edits ; concurrent edits are what give us dirty tracking
    & {:as props}]
   (e/client
     (let [{::keys [debug commit ; :commit fn must be side-effect free, :debug true will call :commit on every edit and present the result to the user
                    discard show-buttons auto-submit genesis
                    Parse Unparse]}
           (auto-props props {::debug false ::show-buttons true ::genesis false ::Parse Identity, ::Unparse Identity})
           form-name name
           edits       (e/as-vec edits)
           dirty-count (count edits)
           clean? (zero? dirty-count)
           show-buttons (cond
                          (boolean? show-buttons) show-buttons
                          (nil? show-buttons) false
                          (= ::smart (qualify show-buttons)) (and (not clean?) (not auto-submit)))

           [form-t form-v :as form]
           (merge-edits edits)

           parsed-form-v (Parse form-v)

           validation-message (not-empty (ex-message parsed-form-v))

           !form-validation (atom ::nil)
           form-validation (e/watch !form-validation)

           !tx-rejected-error (atom nil)
           tx-rejected-error (e/watch !tx-rejected-error)

           ?cs (FormSubmit! ::commit [form-t parsed-form-v]
                 :label "commit"
                 :disabled (e/Reconcile (or clean? validation-message)) ; FIXME reconcile shouldn't be necessary
                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                 :show-button show-buttons
                 :genesis genesis)
           ?d (FormDiscard! ::discard [form-t parsed-form-v] :disabled clean? :label "discard" :show-button show-buttons)]
       (dom/p (dom/props {:data-role "errormessage"})
              (when (not= form-validation ::nil) (dom/text form-validation))
              (when tx-rejected-error (dom/text tx-rejected-error)))
       (e/amb
         (e/for [[token [directive parsed-form-v] :as _edit] (e/amb ?cs ?d)] ; cs and d have form semantics – they really are <input type="submit"|"reset"… >
           (case directive ; does order of burning matter?
             ;; FIXME can't distinguish between successful commit or discarded busy commit. Button will turn green in both cases. Confusing UX.
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             ;; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                             (partial (fn [ts] (doseq [t ts] (t))) (map first (e/as-vec ?cs)))]
                         (case genesis
                           true (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                  (case (token) (e/amb)) ; discard now and swallow cmd, we're done
                                  nil ; FIXME not sure what this code path should do?
                                  #_[token ; its a t
                                     (nth discard 0) ; command
                                     (nth discard 1)]) ; prediction

                                        ; reset form and BOTH buttons, cancelling any in-flight commit
                           false
                           (let [t (after-ack token #(do (clear-commits) (reset! !form-validation ::nil)))]
                             (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                               (case (t) (e/amb)) ; discard now and swallow cmd, we're done
                               nil ; FIXME not sure what this code path should do?
                               #_[t
                                (nth discard 0) ; command
                                (nth discard 1)]))))
             ::commit (let [tempid token]
                        (reset! !form-validation validation-message)
                        (if validation-message
                          (do (tempid ; err value gets dropped, but form won't reset.
                                ::invalid) ; controls interpret ::invalid as "validation error", not tx-rejected
                              (e/amb))
                          (do
                            (when genesis (reset-active-form-input! dom/node))
                            [(unify-t tempid (fn ([] (reset! !tx-rejected-error nil))
                                               ([err] (reset! !tx-rejected-error err))))
                             {form-name parsed-form-v}])))))

         (e/When debug
           (dom/span (dom/text " " dirty-count " dirty"))
           (dom/pre (dom/text (pprint-str (if (= :verbose debug)
                                            {:form form-v, :parsed-form parsed-form-v, :validation validation-message}
                                            parsed-form-v)
                                :margin 80)))))))))

(def nogenesis identity)

(defn build-genesis! [genesis]
  (case genesis
    (true false nil) identity
    (fn [& _] (genesis))))

(e/defn Form!*
  ([value Fields ; concurrent edits are what give us dirty tracking
    & {:as props}]
   (e/client
     (let [{::keys [debug discard ; TODO implement discard
                    show-buttons auto-submit genesis
                    Parse Unparse]}
           (auto-props props {::debug false ::show-buttons true ::genesis nil ::Parse Identity, ::Unparse Identity})
           unparsed-value (Unparse value)
           edits (e/as-vec (Fields unparsed-value))
           dirty-count (count edits)
           clean? (empty? unparsed-value) #_(zero? dirty-count)
           show-buttons (cond
                          (boolean? show-buttons) show-buttons
                          (nil? show-buttons) false
                          (= ::smart (qualify show-buttons)) (and (not (zero? dirty-count)) (not auto-submit)))

           [form-t form-v :as form]
           (merge-edits edits)

           merged-form-v-with-unparsed-v (merge unparsed-value form-v)

           genesis! (build-genesis! genesis)
           !tempid (atom (e/snapshot (genesis! form-t))) ; generate one tempid per commit, not per edit

           parsed-form-v (Parse merged-form-v-with-unparsed-v (e/watch !tempid))

           validation-message (not-empty (ex-message parsed-form-v))

           #_#_!form-validation (atom ::nil)
           #_#_form-validation (e/watch !form-validation)

           !tx-rejected-error (atom nil)
           tx-rejected-error (e/watch !tx-rejected-error)

           ?cs (FormSubmit! ::commit [form-t parsed-form-v]
                 :label "commit"
                 :disabled (e/Reconcile (or (zero? dirty-count) validation-message)) ; FIXME reconcile shouldn't be necessary
                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                 :show-button show-buttons
                 :genesis genesis)
           ?d (FormDiscard! ::discard [form-t parsed-form-v] :disabled (zero? dirty-count) :label "discard" :show-button show-buttons)]
       (dom/p (dom/props {:data-role "errormessage"})
              (dom/text validation-message)
              (when tx-rejected-error (dom/text tx-rejected-error)))
       (e/amb
         (e/for [[token [directive parsed-form-v] :as _edit] (e/amb ?cs ?d)] ; cs and d have form semantics – they really are <input type="submit"|"reset"… >
           (case directive ; does order of burning matter?
             ;; FIXME can't distinguish between successful commit or discarded busy commit. Button will turn green in both cases. Confusing UX.
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             ;; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                             (partial (fn [ts] (doseq [t ts] (t))) (map first (e/as-vec ?cs)))]
                         (if genesis
                           (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                             (case (token) (e/amb)) ; discard now and swallow cmd, we're done
                             nil ; FIXME not sure what this code path should do?
                             #_[token ; its a t
                                (nth discard 0) ; command
                                (nth discard 1)]) ; prediction

                                        ; reset form and BOTH buttons, cancelling any in-flight commit
                           (let [t (after-ack token #(do (clear-commits) #_(reset! !form-validation ::nil)))]
                             (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                               (case (t) (e/amb)) ; discard now and swallow cmd, we're done
                               nil ; FIXME not sure what this code path should do?
                               #_[t
                                  (nth discard 0) ; command
                                  (nth discard 1)]))))
             ::commit (let [tempid token]
                        #_(reset! !form-validation validation-message)
                        (if validation-message
                          (do (tempid ; err value gets dropped, but form won't reset.
                                ::invalid) ; controls interpret ::invalid as "validation error", not tx-rejected
                              (e/amb))
                          (do
                            (when genesis (reset-active-form-input! dom/node))
                            (reset! !tempid (genesis! token)) ; ensure one tempid per commit
                            [(unify-t tempid (fn ([] (reset! !tx-rejected-error nil))
                                               ([err] (reset! !tx-rejected-error err))))
                             parsed-form-v])))))

         (e/When debug
           (dom/span (dom/text " " dirty-count " dirty"))
           (dom/pre (dom/text (pprint-str (if (= :verbose debug)
                                            {:unparsed unparsed-value
                                             :fields form-v,
                                             :form merged-form-v-with-unparsed-v
                                             :parsed-form parsed-form-v
                                             :validation validation-message}
                                            parsed-form-v)
                                :margin 80)))))))))

(defmacro Form! [value Fields & {:as props}] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form events. e.g. submit, reset, invalid, etc…
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (let [props# ~props]
       (Form!* ~value ~Fields (dissoc props#))
       #_(FormStatus
         (Form!* ~name ~fields1 (dissoc props# :Accepted :Rejected :Busy)) ; place fields inside dom/form
         (select-keys props# [:Accepted :Rejected :Busy])))))


#_
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
    (update edits ::token unify-t (fn ([] (reset! !last-state [::accepted]))
                                    ([err] (reset! !last-state [::rejected err]))))))


(defn interpret-result [token result]
  (when-some [[command value] result]
    (case command
      ::ok (do (token) nil)
      ::rejected (do (token value) nil)
      result)))

(e/defn Interpreter
  [effects commands]
  (e/client ; client bias, t doesn't transfer
    #_(prn `Interpreter (e/Count commands) 'commands (e/as-vec commands))
    (e/for [[token [name & args] :as command] commands]
      (if-let [Effect (effects name)]
        (let [res (e/Apply Effect args)]
          #_(prn 'final-res res)
          (case res
            nil nil
            ::ok (token)
            (token res)))
        command))))

#_
(e/defn Update [m k F] (assoc m k (F (get m k))))

#_
(e/defn Intercept
  ([F x Parse] (Update (F x) ::value Parse))
  ([F x Unparse Parse]
   (Update (F (Unparse x)) ::value Parse)))

(e/defn Parse "Map over an edit, supposedly turning an edit into a command. See `Unparse`."
  [F edit] (e/for [[t x] edit] [t (F x)]))

(e/defn Unparse
  "Turn a command into a plain value, loosing the ACK token. Supposedly turning a command into an optimistic record. See `Parse`."
  [F command] (e/for [command command] (F (get command 1)))) ; looses token

(defmacro try-ok [& body] ; fixme inject sentinel
  `(try ~@body ::ok ; sentinel
        (catch Exception e# (doto ::fail (prn e#)))))

(e/declare effects* #_{})

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
          (t ::rejected))))))

;; -----

(defn- now! [& _] #?(:clj (new java.util.Date) :cljs (new js/Date)))

(e/defn OptimisticView ; Experimental
  "Model a collection of authoritative records (supposedly from remote db) +
next expected records (expected to become authoritative in the near future,
supposedly local optimistic guesses).

Return expected records + authoritative records layered over (matched by keyfn),
effectively overwriting the corresponding expected ones (authoritative has
precedence over expected). When an authoritative record is retracted, the view
reflects it. When an optimistic records is removed, the view retains it,
potentially forever, expecting an authoritative record to overwrite it, idealy
asap.

Use case: Prevent optimistic row blink on tx accepted. We retain optimistic
 update between the time an async tx got accepted and the time db queries rerun
 and transfer new diff from server to client. Otherwise local optimistic guess
 might retract before authoritative value is received from server, resulting in
 a blink.
"
  ([key-fn authoritative-recs expected-recs]
   (OptimisticView key-fn now! compare authoritative-recs expected-recs)) ; wrong - ordering by time is not guaranteed to reflect authoritative-recs ordering.
  ([key-fn sort-key-fn comparator authoritative-recs expected-recs]
   ;; Not a perfect impl. No e/as-vec (good thing) but still maintain a piece of state and still contains an e/watch and e/diff-by.
   (let [!index (atom {:kvs {}, :order (sorted-map-by comparator) :rev-order {}})] ; maintain order separately, a single sorted map is not enough to model order + direct access on arbitrary key.

     ;; 1. save expected records
     (e/for [expected expected-recs]
       (let [k      (key-fn expected)
             sort-k (sort-key-fn expected)]
         (swap! !index #(-> % (update :kvs assoc k expected) (update :order assoc sort-k k) (update :rev-order assoc k sort-k)))
         ;; no cleanup on unmount, we potentially retain expected forever.
         ))

     ;; 2. overwrite by authoritatives, cleanup on removal.
     (e/for [authoritative authoritative-recs] ; defaulting to time ordering is wrong, e/for doesn't guarantee branches mount order.
       (let [k      (key-fn authoritative)
             sort-k (sort-key-fn)]
         (swap! !index (fn [{:keys [rev-order] :as index}]
                         (let [sort-k (get rev-order k sort-k)]
                           (-> index
                             (update :kvs assoc k authoritative)
                             (update :order assoc sort-k k)
                             (update :rev-order assoc k sort-k)))))
         (e/on-unmount #(swap! !index (fn [{:keys [rev-order] :as index}]
                                        (let [sort-k (get rev-order k sort-k)]
                                          (-> index
                                            (update :kvs dissoc k)
                                            (update :order dissoc sort-k)
                                            (update :rev-order dissoc k))))))))
     (let [{:keys [kvs order]} (e/watch !index)]
       (e/diff-by key-fn         ; ugly
         (map kvs (vals order))) ; ugly
       ))))


