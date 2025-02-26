(ns hyperfiddle.electric-forms4
  #?(:cljs (:require-macros hyperfiddle.electric-forms4))
  (:require [contrib.data :refer [index-by auto-props qualify]]
            [dustingetz.str :refer [pprint-str]]
            [contrib.css :refer [css-slugify]]
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

(def nil-t (constantly nil))

(e/defn Input! [field-name ; fields are named like the DOM, <input name=...> - for coordination with form containers
                v & {:keys [maxlength type parse Validate] :as props
                     :or {maxlength 100 type "text" parse identity Validate (e/fn [_])}}]
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
        (if waiting? ; return nothing, not nil - because edits are concurrent, also helps prevent spurious nils
          (let [v' ((fn [] (-> e .-target .-value (subs 0 maxlength) parse)))
                validation-message (Validate v')]
            (InputValidity validation-message)
            {::token t, ::name field-name, ::value v' ::validation validation-message})  ; edit request, bubbles upward to interpreter
          {::token nil-t, ::name field-name ::value v})))))

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

(e/defn Checkbox! [k checked & {:keys [id type label parse Validate] :as props
                                :or {id (random-uuid) type :checkbox parse identity Validate (e/fn [_])}}]
  ; todo esc?
  (e/client
    (e/amb
      (let [[e t err input-node]
            (dom/input (dom/props {:type type, :id id}) (dom/props (dissoc props :id :label :parse :Validate))
                       (let [e (dom/On "change" identity nil) [t err] (e/Token e)] ; single txn, no concurrency
                         [e t err dom/node]))
            editing? (dom/Focused? input-node) ; never true on Safari (MacOs and iOS)
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
          {::token t, ::name k ::value v, ::validation validation-message}))
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
      (swap! !latest (fn [{::keys [token]} new] (when token (token)) new) edits))
    (e/When latest latest)))

(e/defn Picker!
  [k authoritative-v Body &
   {:keys [as Validate]
    :or {as :div, Validate (e/fn [_])}
    :as props}]
  (let [!selected (atom {::token nil, ::value (e/snapshot  ; "don't damage user input" – will track authoritative-v in absence of a token – see below
                                                (identity authoritative-v)) ; HACK prevent a crash if `authoritative-v` binding is (e/server ...)
                         ::validation nil})
        {::keys [token value validation]} (e/watch !selected)]
    (dom/With-element as
      (e/fn []
        (dom/props {:role "radiogroup", :data-errormessage (not-empty (str (Validate value)))})
        (reset! !selected (Body value))
        (let [focused? (dom/Focused-in?)]
          focused? ; force event handler - temporary
          ;; TODO use focused? to put focus on aria-checked element on focus enter
          ;; TODO intercept arrow keys to focus next/previous/first/last checkable elements.
          (if (some? token) ; "don't damage user input" – only track authoritative value in absence of a token
            {::token (after-ack token (fn after [] (swap! !selected assoc ::token nil ::validation nil)))
             ::name k
             ::value value
             ::validation validation}
            (e/When (not focused?)
              (swap! !selected assoc ::value authoritative-v)
              (e/amb))))))))

(defn -checked? [v x]
  (when v (= v x)))

(e/defn RadioPicker!
  [k authoritative-v
   & {:keys [as option-label Options Validate]
      :or   {as :dl, Validate (e/fn [_])}
      :as   props}]
  (Picker! k authoritative-v
    (e/fn [selected-index]
      (let [options (e/as-vec (Options))] ; all options get rendered anyway. Look for TablePicker! otherwise.
        (LatestEdit
          (e/for [index (IndexRing (count options) 0)]
            (let [x  (get options index)
                  id (random-uuid)]
              (dom/dt (dom/label (dom/props {:for id}) (dom/text x)))
              (dom/dd
                (-> (Checkbox! x (-checked? selected-index x) :id id, :name k, :type :radio, :label option-label)
                  (assoc ::value x))))))))
    :as as
    :Validate Validate
    props))

(e/defn ^:deprecated Radio! "Deprecated in favor of `RadioPicker!`" [k authoritative-v & {:as props}] (RadioPicker! k authoritative-v props))

(e/defn TablePicker! ; TODO G: might have damaged optimal siting – verify
  ;; TODO aria-compliant keyboard nav https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/radio_role#keyboard_interactions
  [k authoritative-v record-count Row
   & {:keys [Validate row-height]
      :or {Validate (e/fn [_]), row-height 24}
      :as props}]
  (dom/div
    (dom/props {:class ["Viewport" (css-slugify k)], #_#_:style {:height "96px"}}) ; TODO cleanup
    (dom/props (dissoc props :Validate :row-height))
    (let [[offset limit] (Scroll-window row-height record-count dom/node {})]
      (e/amb
        (Picker! k authoritative-v
          (e/fn PickerBody [selected-index]
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
                    (e/When t {::token t, ::value index}))))))
          :as :table
          :Validate Validate
          props)
        (dom/div (dom/props {:style {:height (str (contrib.data/clamp-left ; row count can exceed record count
                                                    (* row-height (- record-count limit)) 0) "px")}}))))))

(e/defn TablePicker!2 ; TODO G: might have damaged optimal siting – verify
  ;; TODO aria-compliant keyboard nav https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/radio_role#keyboard_interactions
  [k authoritative-v record-count Row
   & {:keys [Validate row-height column-count as]
      :or {Validate (e/fn [_]), row-height 24, column-count 1, as :table}
      :as props}]
  (Picker! k authoritative-v
    (e/fn PickerBody [selected-index]
      (dom/props (dissoc props :Validate :row-height :column-count :as))
      (dom/props {:class ["hyperfiddle-electric-forms4__table-picker" (css-slugify k)]
                  :style {:--row-height (str row-height "px")
                          :--record-count record-count
                          :--column-count column-count}})

      (let [[offset limit] (Scroll-window row-height record-count dom/node {})]
        (dom/props {:style {:--offset offset, :--limit limit}})
        (dom/div (dom/props {:class "padder"}))
        (LatestEdit
          (e/for [index (IndexRing ; render all rows even when record-count < limit
                          (inc limit) ; render one extra row for pixel perfect scroll (bottom row do not blink in/out on scroll)
                          offset)]
            (dom/tr
              (dom/props {:role "radio"
                          :style {:--row-index index}
                          :data-row-stripe (mod index 2)}) ; TODO move to parent node + css with nth-child
              (dom/props {:tabindex "0" :aria-checked (= index selected-index)}) ; tabindex enables focus – items with role=radio must be focusable
              ;; FIXME e/for forces transfer of return value: prevents site-neutral impl. Returning token forces this entire branch to run on client, and so Row is called on client.
              (Row index)
              (let [[t _err] (e/Token (e/amb ; click + space is aria-compliant
                                        (dom/On "click" identity nil) ;;
                                        (dom/On "keypress" #(when (= "Space" (.-code %)) (doto % (.preventDefault))) nil)))] 
                (e/When t {::token t, ::value index})))))))
    :as as
    :Validate Validate
    props))

(def table-picker-css ; exported at end of file
  "

.hyperfiddle-electric-forms4__table-picker {display:grid; grid-template-columns: repeat(var(--column-count), 1fr); }
.hyperfiddle-electric-forms4__table-picker {height: 100%; overflow: hidden; min-height: calc(2 * var(--row-height)); }
.hyperfiddle-electric-forms4__table-picker {contain: size;} /* Essential! ensure row movements on scroll do not inflate parent containers when parent only has a min-height. Otherwise container will grow in a loop until all rows are rendered. */
.hyperfiddle-electric-forms4__table-picker {grid-auto-rows: var(--row-height);}
.hyperfiddle-electric-forms4__table-picker {overflow-y: scroll; overflow-x: hidden; position: relative;}

.hyperfiddle-electric-forms4__table-picker .padder {position: absolute; width: 1px; z-index: -1;}
.hyperfiddle-electric-forms4__table-picker .padder { height: calc(var(--record-count) * var(--row-height)); min-height: 100%;}

.hyperfiddle-electric-forms4__table-picker tr {display: contents;}
.hyperfiddle-electric-forms4__table-picker tr td { grid-row: calc(1 + var(--row-index)); }

/* cosmetic defaults */
.hyperfiddle-electric-forms4__table-picker tr[data-row-stripe='0'] td { background-color: #f2f2f2; }

.hyperfiddle-electric-forms4__table-picker tr:hover:has(*) td { background-color: #ddd; }
.hyperfiddle-electric-forms4__table-picker tr:is([aria-selected=true],[aria-checked=true]):has(*) td { color: white; background-color: #0064e1; /* finder color */ }
.hyperfiddle-electric-forms4__table-picker td:not(:has(*)) { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
"
)

;;; Edit/action -> command mapping

(defn token-v [token-map] (dissoc token-map ::token ::validation))

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

(e/defn Button! [[name value :as command] & {:as props}] ; User friendly API answering "what does the button do when clicked: it returns {::token token, ::name name, ::value value}, ∅ otherwise."
  {::token (first (TxButton! props))
   ::name  name
   ::value value})

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

(defn stop-err-propagation [token] (when token (fn ([] (token)) ([_err]))))

(e/defn Amb->nil [table]
  (when (e/Some? table)
    table))

(e/defn FormSubmit! ; dom/node must be a form
  [directive edits & {:keys [disabled show-button auto-submit genesis] :as props}]
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
            (Directive! directive edits (unify-t t (Amb->nil btn-t)))))
        (do ; Genesis case – parallel racing txs. Button cannot report more than one tx status unambiguously. Use regular (non-tx) button to trigger submit.
          (when show-button (Button (-> props (assoc :type :submit, #_#_:data-role "genesis") (dissoc :auto-submit :show-button :genesis))))
          (e/for [[submit-q _err] (dom/On-all "submit" submit-handler nil)]
            (let [edits (e/snapshot edits)] ; snapshot to detach edits before any reference to edits, or spending the edits token would loop into this branch and cause a crash.
              ((::token edits)) ; immediately detach edits – clears user input.
              {::token (stop-err-propagation submit-q)
               ::name directive
               ::value (::value edits)})))))))

(defn invert-fields-to-form [field-edits]
  (when (seq field-edits)
    {::token (let [tokens (map ::token field-edits)]
               (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                 ([] (doseq [t tokens] (t)))
                 ([_err] #_(doseq [t tokens] (t err ::keep)))))
     ::name ::form
     ::value (not-empty (into {} (map (juxt ::name ::value)) field-edits))  ; collect fields into form, retain until commit/discard
     ::validation (not-empty (remove nil? (map ::validation field-edits)))}))

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
    name edits ; concurrent edits are what give us dirty tracking
    & {:as props}]
   (e/client
     (let [{::keys [debug commit ; :commit fn must be side-effect free, :debug true will call :commit on every edit and present the result to the user
                    discard show-buttons auto-submit genesis
                    Validate
                    #_InspectState]}
           (auto-props props {::debug false ::show-buttons true ::genesis false ::Validate (e/fn [_]) #_#_::InspectState (e/fn [_])})
           form-name name
           dirty-count (count (remove #{nil-t} (map ::token (e/as-vec edits))))
           clean? (zero? dirty-count)
           show-buttons (cond
                          (boolean? show-buttons) show-buttons
                          (nil? show-buttons) false
                          (= ::smart (qualify show-buttons)) (and (not clean?) (not auto-submit)))

           {::keys [validation] :as form}
           (invert-fields-to-form (e/as-vec edits))

           !form-validation (atom ::nil)
           form-validation (e/watch !form-validation)

           !tx-rejected-error (atom nil)
           tx-rejected-error (e/watch !tx-rejected-error)

           ?cs (FormSubmit! ::commit form
                 :label "commit"
                 :disabled (e/Reconcile (or clean? validation)) ; FIXME reconcile shouldn't be necessary
                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                 :show-button show-buttons
                 :genesis genesis)
           ?d (FormDiscard! ::discard form :disabled clean? :label "discard" :show-button show-buttons)]
       (dom/p (dom/props {:data-role "errormessage"})
              (when (not= form-validation ::nil) (dom/text form-validation))
              (when tx-rejected-error (dom/text tx-rejected-error)))
       (e/amb
         (e/for [{::keys [token name value] :as edit} (e/amb ?cs ?d)] ; cs and d have form semantics – they really are <input type="submit"|"reset"… >
           (case name ; does order of burning matter?
             ;; FIXME can't distinguish between successful commit or discarded busy commit. Button will turn green in both cases. Confusing UX.
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             ;; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                             (partial (fn [ts] (doseq [t ts] (t))) (map ::token (e/as-vec ?cs)))]
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
             ::commit (let [form-v value ; dirty subset
                            tempid token
                            [form-v guess] (if commit
                                             (commit form-v tempid #_dirty-form-guess) ; guess and form would be =
                                             [form-v #_{e dirty-form}])] ; no entity id, only user can guess
                        (let [validation-message (Validate form-v)]
                          (reset! !form-validation validation-message)
                          (if validation-message
                            (do (token  ; err value gets dropped, but form won't reset.
                                  ::invalid) ; controls interpret ::invalid as "validation error", not tx-rejected
                                (e/amb))
                            (let [t (case genesis
                                      true (do (reset-active-form-input! dom/node) token) ; this is a q, transfer to list item
                                      false token)] ; this is a t
                              {::token (unify-t t (fn ([] (reset! !tx-rejected-error nil))
                                                    ([err] (reset! !tx-rejected-error err))))
                               ::name form-name
                               ::value form-v
                               ;; (if name {name form-v} form-v) ; nested forms as fields in larger forms ; FIXME
                               ::guess guess}))))))

         (let [commit-edit (if commit (commit (::value form) "-1") (::value form))
               form-v-edit (if name {name commit-edit} commit-edit)]
           (e/When debug
             (dom/span (dom/text " " dirty-count " dirty"))
             (dom/pre #_(dom/props {:style {:min-height "4em"}})
                      (dom/text (pprint-str (if (= :verbose debug)
                                              {:fields (::value form), :expected-commit form-v-edit, :fields-validation validation, :form-validation form-validation}
                                              form-v-edit)
                                  :margin 80))))))))))

(defmacro Form! [name fields1 & {:as props}] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form events. e.g. submit, reset, invalid, etc…
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (let [props# ~props]
       (Form!* ~name ~fields1 (dissoc props# :Accepted :Rejected :Busy))
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
  ;; Should top-level Interpreter inform user a command cannot be interpreted?
  ;; D & G initially said yes: an Interpreter delegates unknown commands until
  ;; they reach the outermost Interpreter, at which point unhandled commands
  ;; are considered an error. Upon reflexion, G thinks unhandled commands
  ;; should be ignored and dropped, because defining a "top level, outermost"
  ;; Interpreter implies defining a clear boundary between the user program
  ;; and the entrypoint. Such boundary do not seem obvious to define given
  ;; Electric's strong composition. One could have a use case for returning
  ;; uninterpreted commands for an other program to consume. 
  [effects commands]
  (e/client ; client bias, t doesn't transfer
    (prn `Interpreter (e/Count commands) 'commands (e/as-vec commands))
    (e/for [{::keys [token name value guess] :as command} commands]
      (if-let [Effect (effects name)]
        (let [[name value :as res] (interpret-result token (Effect value))]
          (prn 'final-res res)
          (e/When res
            (Interpreter effects (assoc command ::name name, ::value value) )))
        command))))

(e/defn FormStatus [form-edits Continuation]
  (let [!last-state (atom [nil nil nil])
        [status value err] (e/watch !last-state)]
    (swap! !last-state assoc 1 form-edits)
    (e/When (or status form-edits)
      (Continuation (or status ::busy) value err))
    (update form-edits ::token #(unify-t (fn ([] (reset! !last-state [::accepted form-edits nil]))
                                           ([err] (reset! !last-state [::rejected form-edits err]))) %))))

(e/defn AfterAck [form-edit F]
  (let [!state (atom [::nil nil])
        [state last-form] (e/watch !state)]
    (swap! !state assoc 1 form-edit)
    (e/amb
      (e/When (not= ::nil state)
        (F last-form state))
      (update form-edit ::token #(unify-t (fn self ([] (self nil)) ([err] (swap! !state assoc 0 err))) %)))))

(e/defn AfterSuccess [form-edit F]
  (AfterAck form-edit (e/fn [form state]
                        (when (nil? state)
                          (F (::value form))))))

(defn command [name value] {::token nil-t ::name name, ::value value})

(e/defn Update [m k F] (assoc m k (F (get m k))))

(e/defn Intercept
  ([F x Parse] (Update (F x) ::value Parse))
  ([F x Unparse Parse]
   (Update (F (Unparse x)) ::value Parse)))

;;; TODO
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

#_
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


#_(defmacro try-ok [& body] ; fixme inject sentinel
  `(try ~@body ::ok ; sentinel
        (catch Exception e# (doto ::fail (prn e#)))))


(def css (str table-picker-css)) ; exports