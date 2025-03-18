(ns hyperfiddle.electric-forms5
  #?(:cljs (:require-macros hyperfiddle.electric-forms5))
  (:require [contrib.data :refer [auto-props qualify]]
            [dustingetz.str :refer [pprint-str]]
            [clojure.set :as set]
            [missionary.core :as m]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing]]
            [missionary.core :as m]
            [hyperfiddle.electric-forms5.tokens :as t]))

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

#_ (defn form-valid? [x] ; works for all edits - control or form - "edit-valid?" name was judged less natural.
  (not (instance? #?(:clj Throwable, :cljs js/Error) x)))

(e/defn Input! [field-name ; fields are named like the DOM, <input name=...> - for coordination with form containers
                v & {:keys [as maxlength name type Parse Unparse] :as props
                     :or {as :input, maxlength 100, type "text", Parse Identity, Unparse (Lift str)}}]
  (e/client
    v ; ensure v is consumed to prevent surprising side effects on commit discard or dirty/not dirty (lazy let)
    (dom/element as
      (dom/props (-> props (dissoc :as :parse :Parse :Unparse) (assoc :maxLength maxlength :type type :name (or name (str field-name)))))
      (let [e (dom/On "input" identity nil) [t err] (e/Token e) ; reuse token until commit
            editing? (dom/Focused?)
            waiting? (some? t)
            error? (some? err)
            dirty? (e/Reconcile (or editing? waiting? error?))
            unparsed-v (e/Reconcile (if waiting? ((fn [] (-> e .-target .-value))) (str (Unparse v)))) ; user input has precedence
            parsed-v (Parse (subs unparsed-v 0 maxlength))]
        (SetValidity parsed-v)
        (when-not dirty? (set! (.-value dom/node) unparsed-v)) ; TODO - submit must reset input while focused
        (when error? (dom/props {:aria-invalid true})) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (when waiting? (dom/props {:aria-busy true}))
        (e/When waiting? ; return nothing, not nil - because edits are concurrent, also helps prevent spurious nils
          [t {field-name parsed-v}])))))  ; edit request, bubbles upward to interpreter

(e/defn Output [field-name ; fields are named like the DOM <input name=...> - for coordination with form containers
                v & {:keys [Unparse] :as props :or {Unparse (Lift str)}}]
  (e/client
    (dom/output
      (dom/props (dissoc props :Unparse))
      (dom/props {:for (str field-name)}) ; TODO support multiple values – https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for
      (dom/text (Unparse v)))
    (e/amb)))

(e/defn Textarea! [field-name v & props]
  (Input! field-name v :as :textarea props))

(defn after-ack "
proxy token t such that callback f! will run once the token is ack'ed. E.g. to ack many tokens at once."
  [t1 f!]
  (t/token `after-ack t1 (fn after-ack [& [_err]] (f!))))

(defn debug-t "dev/debug only - (unify-t token (debug-t \"message\"))"
  [message]
  (fn debug-t [& args] (apply prn message args)))

(e/defn Checkbox! [k checked & {:keys [id type label Parse Unparse] :as props
                                :or {id (random-uuid), type :checkbox, Parse Identity, Unparse (Lift boolean)}}]
  ; todo esc?
  (e/client
    checked ; ensure v is consumed to prevent surprising side effects on commit discard (lazy let)
    (e/amb
      (let [[e t err input-node]
            (dom/input (dom/props {:type type, :id id}) (dom/props (dissoc props :id :label :parse :Parse :Unparse))
                       (let [e (dom/On "change" identity nil) [t err] (e/Token e)] ; single txn, no concurrency
                         [e t err dom/node]))
            editing? (dom/Focused? input-node) ; never true on Safari (MacOs and iOS)
            waiting? (some? t)
            error? (some? err)
            dirty? (or editing? waiting? error?)
            unparsed-v (e/Reconcile (if waiting? ((fn [] (-> e .-target .-checked))) (Unparse checked))) ; user input has precedence
            parsed-v (Parse unparsed-v)]
        (SetValidity input-node parsed-v)
        (when (or (not dirty?) (#{"radio" :radio} type)) ; Radio's "don't damage user input" behavior handled at radiogroup level.
          (set! (.-checked input-node) unparsed-v)) ; FIXME DOM state isn't set when selecting two radios in a row in the same radiogroup.
        (when error? (dom/props input-node {:aria-invalid true}))  ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
        (e/When waiting?
          (dom/props input-node {:aria-busy true})
          [t {k parsed-v}]))
      (e/When label (dom/label (dom/props {:for id}) (dom/text (label k)))))))

(e/defn LatestEdit ; TODO ideally remove
  "
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
    (e/Reconcile (e/When latest latest))))

(e/defn LatestEdit2 ; TODO ideally remove
  [edits]
  (let [!latest (atom nil)
        latest (e/watch !latest)]
    (case (e/Count edits)
      0 (reset! !latest nil)
      (swap! !latest (fn [[old-t _old-kv] [new-t new-kv]] [(t/token `LatestEdit2 (t/token 'new new-t) (t/token 'old old-t)) new-kv]) edits))
    (e/Reconcile (e/When latest latest))))

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
              [(after-ack t (fn picker-after-ack [] (swap! !selected assoc 0 nil))) ; [t v] -> [nil v]
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

;;; Buttons

(e/defn Button* [{:keys [label] :as props}]
  (dom/button (dom/text label)
              (dom/props (dissoc props :label))
              [(dom/On "click" identity nil) dom/node]))

(e/defn Button "Simple button, return latest click event or nil."
  [& {:keys [label] :as props}]
  (first (Button* props)))

(e/defn TrackTx [F]
  (let [!err (atom nil)
        err (e/watch !err)
        [t & rest] (F err)]
    (into [(#(when % (t/token `TrackTx % (fn ([] (reset! !err nil))
                                           ([err] (reset! !err err))))) t)]
      rest)))

(defmacro reboot-by [keyfn sym & body]
  `(e/for [~sym (e/diff-by ~keyfn (e/as-vec ~sym))] ~@body))

(defmacro reboot-on [sym & body]
  `(reboot-by identity ~sym ~@body))

(defn button-strategy [user-t user-err controlled-t]
  (cond (or user-t user-err) ::user-has-precedence
        controlled-t         ::controlled
        :else                ::idle))

(e/defn Button!* [& {:keys [token] :as props}]
  (let [[event node] (Button* (dissoc props :token))
        [tracked-token tracked-err] (TrackTx (e/fn [err] [token err]))
        [btn-t btn-err] (reboot-on event (e/Token event))]
    (e/Reconcile
      (case (button-strategy btn-t btn-err tracked-token)
        ::user-has-precedence [(t/token `Button!* (t/token 'btn-t (e/snapshot btn-t)) (t/token 'tracked-token token)) btn-err event node]
        ::controlled [tracked-token tracked-err (js/Object.) node]
        ::idle [nil nil nil node]))))

(e/defn TxButton!
  ;; Like `Button!*` with extra semantic markup reflecting tx status.
  ;; Users want `Button!` instead, mapping "button click tx" to a business command.
  "Transactional button with busy state. Disables when busy. To be styled with CSS:
  - button[aria-busy=true]{...} : tx is comitting
  - button[aria-invalid=true]{...} : tx failed
  - button[data-tx-status=accepted] : tx success
  - button:disabled{...} "
  [& {:keys [disabled type token] :or {type :button} :as props}]
  (let [[btn-t err event node] (Button!* (-> props (assoc :type type) (dissoc :disabled)))]
    ;; Don't set :disabled on <input type=submit> before "submit" event has bubbled, it prevents form submission.
    ;; When "submit" event reaches <form>, native browser impl will check if the submitter node (e.g. submit button) has a "disabled=true" attr.
    ;; Instead, let the submit event propagate synchronously before setting :disabled, by queuing :disabled on the event loop.
    (dom/props node {:disabled (e/Task (m/sleep 0 (or disabled (and btn-t (nil? err)))))})
    (dom/props node {:aria-busy (some? btn-t)})
    (dom/props node {:aria-invalid (#(and (some? err) (not= err ::invalid)) err)}) ; not to be confused with CSS :invalid. Only set from failed tx (err in token). Not set if form fail to validate.
    (dom/props node {:data-tx-status (when (and (some? event) (nil? btn-t) (nil? err)) "accepted")}) ; FIXME can't distinguish between successful tx or tx canceled by clicking discard.
    (e/When btn-t [btn-t err]))) ; forward token to track tx-status ; should it return [t err]?

(e/defn Button! [command & {:keys [Parse] :or {Parse Identity} :as props}] ; User friendly API answering "what does the button do when clicked: it returns [t (Parse value)], ∅ otherwise."
  [(first (TxButton! (dissoc props :Parse))) (Parse command)])

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

(e/declare current-edit) ; [edit-t edit-kvs]

(e/defn FormDiscard! ; dom/node must be a form
  [directive [edits-t edits-kvs] & {:as props}]
  (e/client
    ;; reset form on <ESC>
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    ;; Handle form reset
    (let [[t _err] (e/Token (dom/On "reset" #(do (.preventDefault %) (.stopPropagation %) (blur-active-form-input! (.-target %)) %) nil))] ; TODO render error for failed custom :discard command, if any.
      (e/When t
        [(t/token `FormDiscard! t edits-t) [directive edits-kvs]] ; edits-kvs is unused, but command shape matches FormSubmit for consistency
        ))))

(e/declare *disabled-commit)
(e/declare *disabled-discard)

(e/defn DiscardButton! [& {:as props}]
  (Button! [::discard] :type :reset :label "discard" :disabled *disabled-discard props))

#?(:cljs
   (defn event-submit-form! [disabled ^js e]
     ;; (js/console.log "submit form" {:type (.-type e), :currentTarget (.-currentTarget e), :target (.-target e), :submitter (.-submitter e)}) ; debug why form got submitted
     (when-not disabled
       (when-let [form (some-> e .-target .-form)]
         (if-let [submit-btn (some (fn [element] (and (= "submit" (.-type element)) element)) (.-elements form))]
           (when (.checkValidity form)
             (.click submit-btn))
           (.requestSubmit form))))))

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

(defn stop-err-propagation [tok] (when tok (fn ([] (tok)) ([_err]))))

(defn -schedule "run f after current propagation turn" [f] (m/sp (m/? (m/sleep 0)) (f)))
(e/defn Schedule [f] (e/Task (-schedule f)))

#?(:cljs
   (defn form-submit-handler [disabled ^js event]
     (.preventDefault event)
     #_(js/console.log "submit" (clj->js {:hash (hash event), :from-this-form (event-is-from-this-form? event), :disabled disabled :node dom/node :currentTarget (.-currentTarget event) :e event}))
     (when (and (not disabled) (event-is-from-this-form? event))
       event)))

(e/defn FormSubmitHelper! [& {:keys [disabled auto-submit genesis]}]
  (e/client
    ;; We handle form validation manually, disable native browser submit prevention on invalid form.
    ;; Also hide native validation UI.
    (dom/On "invalid" #(.preventDefault %) nil {:capture true})

    ;; Simulate submit by pressing Enter on <input> nodes
    ;; Don't simulate submit if there's a type=submit button. type=submit natively handles Enter.
    (when (not disabled)
      (dom/On "keypress" (fn [e] ;; (js/console.log "keypress" dom/node)
                           (when (and (event-is-from-this-form? e) ; not from a nested form
                                   (= "Enter" (.-key e))
                                   (= "INPUT" (.-nodeName (.-target e))))
                             (.preventDefault e) ; prevent native form submission
                             (event-submit-form! disabled e) ; fire submit event
                             nil)) nil))
    (when (and auto-submit (not genesis)) ; Simulate autosubmit
      ;; G: auto-submit + genesis is not a thing. Only known use case is "Create new row" in a masterlist view.
      ;;    "Create new row" is a Form with a single [+] Button. It cannot collect user input. It creates an empty entity with
      ;;    generated id (imagine what auto-submit+genesis would imply for checkbox or text input). All candidate buttons are
      ;;    therefore submit buttons, and get the "auto-submit" behavior for free, since they are themselves the submit
      ;;    affordance. Therefore the only use case for auto-submit + genesis has no user input to "auto submit" and collapses to
      ;;    just "genesis".
      (let [event (e/amb
                    (dom/On "change" (fn [e] ; TODO consider commit on text input blur (as a change event when autosubmit = false)
                                       (.preventDefault e)
                                       #_(js/console.log "change" dom/node)
                                       (when (and (event-is-from-this-form? e) ; not from a nested form
                                               (instance? js/HTMLInputElement (.-target e))
                                               (= "checkbox" (.. e -target -type)))
                                         ;; (js/console.log "change" (hash e) e)
                                         e)) nil)
                    ;; all inputs but checkboxes
                    (dom/On "input"  (fn [e]
                                       ;; (js/console.log "input" dom/node)
                                       (when (and (event-is-from-this-form? e) ; not from a nested form
                                               (instance? js/HTMLInputElement (.-target e))
                                               (not= "checkbox" (.. e -target -type)))
                                         ;; (js/console.log "input" e)
                                         e)) nil))]
        (Schedule #(event-submit-form! disabled event))
        event)))
  (e/amb))

(e/defn FormSubmitSequential! ; dom/node must be a form
  [directive [edits-t edits-kvs] & {:keys [disabled token]}]
  ;; Regular tx submit – txs are sequential.
  (e/client
    (let [submit-event (dom/On "submit" (partial form-submit-handler disabled) nil) ; Submit is the only authoritative event.
          t (e/Reconcile
              (if submit-event ; user submit takes precedence over controlled form
                (let [submit-t (first (reboot-on submit-event (e/Token submit-event)))]
                  (e/When submit-t ; if submit is acked, then tracked-token is too
                    (t/token "merge user submit with authoritative token"
                      (t/token 'submit-event submit-t)
                      (t/token 'tracked-token (e/snapshot token)))))
                (e/When token ; controlled form case
                  (t/token 'tracked-token token))))]
      (e/When t
        [(t/token `FormSubmitSequential! t (t/token 'edits-t edits-t)) [directive edits-kvs]]))))

(e/defn FormSubmitConcurrent! ; dom/node must be a form
  [directive edits & {:keys [disabled]}]
  ;; Parallel racing txs. Button cannot report more than one tx status unambiguously.
  (e/client
    (e/for [[submit-q _err] (dom/On-all "submit" (partial form-submit-handler disabled) nil)]
      (let [[edits-t edits-kvs] (e/snapshot edits)] ; snapshot to detach current edit.
        (e/Reconcile
          (if edits-t ;; FIXME this conditional should probably be added here - to be tested.
            (do (edits-t) ; immediately detach edits – clears user input.
                (reset-active-form-input! dom/node)
                [(stop-err-propagation submit-q) [directive edits-kvs]])
            (do (submit-q) (e/amb))))))))

(e/declare *tracked-token)

(e/defn SubmitButton! [& {:keys [genesis] :as props}]
  (e/Reconcile
    (if genesis
      (do (Button :type :submit :label "commit" :disabled *disabled-commit (dissoc props :genesis)) ; do not track tx
          (e/amb))
      (Button! [::commit] :type :submit :label "commit" :token *tracked-token :disabled *disabled-commit (dissoc props :genesis)))))

(def form-command? #{::commit ::discard})

(defn split-edits-from-commands [edits]
  (let [form-command? (fn [[t x]] (and (vector? x) (form-command? (first x))))]
    [(remove form-command? edits)
     (filter form-command? edits)]))

(defn merge-edits [edits] ; FIXME only re-build token if underlying tokens changes
  [(when-some [tokens (seq (map first edits))]; extract all tokens
     (->> (apply t/token `merge-edits tokens) ; unify them into a single ack-all, fan-out token
       stop-err-propagation)) ; severs fan-out of err path, as we cannot report form failure at field level.
   (not-empty (into {} (map second) edits)) ; merge all kvs.
   ])

(defn directive [[t [directive & args] :as command]] directive)
(defn collect-commands [commands] (into {} (map (juxt directive identity)) commands))

(e/defn InterpretCommit! [[token [_commit parsed-form-v] :as _edit] commit-t form-valid?]
  (e/Reconcile
    (if form-valid?
      (let [[token err] (TrackTx (e/fn [err] [token err]))]
        [[(t/token `InterpretCommit! token ((e/capture-fn) (fn [& args] (when commit-t (apply commit-t args))))) parsed-form-v] err])
      (do (token ; err value gets dropped, but form won't reset.
            ::invalid) ; controls interpret ::invalid as "validation error", not tx-rejected
          (e/amb)))))

(e/defn InterpretDiscard! [[token [_discard & _ :as _edit]] discard-t commit-t all-commits genesis tracked-token]
  (let [token (t/token `InterpretDiscard! token discard-t commit-t)
        clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
        ;; FIXME bug workaround - ensure commits are burnt all at once, not one after the other. Crashes otherwise.
        (partial (fn [ts] (doseq [t ts] (t))) (map first all-commits))]
    (e/Reconcile
      (if (e/Reconcile genesis) ; why?
        (token) ; discard now and swallow cmd, we're done
        ;; reset form and BOTH buttons, cancelling any in-flight commit
        (let [token (-> token (after-ack #(when tracked-token (tracked-token ::discard))) (after-ack clear-commits))]
          (token)))))
  (e/amb))

(e/defn FormActions! [[form-t parsed-form-v] form-valid? commit-t discard-t genesis auto-submit tracked-token tracked-cmd]
  (FormSubmitHelper! :disabled *disabled-commit :auto-submit auto-submit :genesis genesis)
  (let [?commits (if (e/Reconcile genesis) ; WTF - required as of March 18 2025 - otherwise FromSubmitSequential! blinks
                   (FormSubmitConcurrent! ::commit [form-t parsed-form-v] :disabled *disabled-commit) ; not supported: controlled form with genesis
                   (FormSubmitSequential! ::commit [form-t parsed-form-v] :disabled *disabled-commit :token tracked-token :command tracked-cmd)) ; eventually controlled
        ?discards (FormDiscard! ::discard [form-t parsed-form-v] :disabled *disabled-discard)]
    (e/Reconcile ; FIXME not sure if required - March 18 2025
      (e/for [[_token [directive _] :as edit]
              (e/diff-by (comp first second) ; ouch! simplify
                (e/as-vec (e/amb ?discards (LatestEdit2 (e/amb ?commits)))))]
        (e/Reconcile
          (case directive ; does order of burning matter?
            ;; FIXME can't distinguish between successful commit or discarded busy commit. Button will turn green in both cases. Confusing UX.
            ::discard (InterpretDiscard! edit discard-t commit-t (e/as-vec ?commits) genesis tracked-token)
            ::commit (InterpretCommit! edit commit-t form-valid?)))))))

(e/defn FormDebugHelper [debug & {:syms [dirty-count unparsed-value form-v merged-form-v-with-unparsed-v parsed-form-v validation-message]}]
  (e/When debug
    (dom/span (dom/text " " dirty-count " dirty"))
    (dom/pre (dom/text (pprint-str (if (= :verbose debug)
                                     {:unparsed unparsed-value
                                      :fields form-v,
                                      :form merged-form-v-with-unparsed-v
                                      :parsed-form parsed-form-v
                                      :validation validation-message}
                                     parsed-form-v)
                         :margin 80)))
    (e/amb)))

(defmacro map-shorthand
  "(map-shorthand a b c) => {'a a, 'b b, 'c c}. Destructure with {:syms [a b c]}"
  [& symbolic-vals]
  (assert (every? symbol? symbolic-vals))
  (into {} (map vector (map #(list 'quote %) symbolic-vals) symbolic-vals)))

(e/defn SnapshotCommand [[t & _rest :as command]]
  (e/Reconcile
    (when t (e/snapshot command))))

(e/defn UnparseForm [Unparse value]
  (let [[t cmd] (e/Reconcile (when Unparse value))
        [t cmd [unparsed tempid]] (e/Reconcile (if Unparse (SnapshotCommand [t cmd (Unparse cmd)]) [nil nil [value nil]]))]
    [t cmd unparsed tempid]))

(e/defn Filter [pred x]
  (let [!x (atom (e/snapshot x))]
    (swap! !x (fn [old new] (if (pred new) new old)) x)
    (e/watch !x)))

(defn tempid-gen [tempid-fn]
  (fn [x] (if tempid-fn (tempid-fn) x)))

(e/defn Form!*
  ([value Fields & {:keys [debug show-buttons auto-submit genesis Parse Unparse tempid #_discard] ; TODO implement discard
                    :or {debug false, show-buttons false, genesis false}}]
   (e/client
     (let [next-tempid! (tempid-gen tempid)

           [tracked-token tracked-cmd unparsed-value tempid] (UnparseForm Unparse value)
           Parse (e/Reconcile (or Parse (e/fn [fields tempid] fields)))

           !disabled-commit (atom false)
           !disabled-discard (atom false)
           edits (binding ; dynamic scope because correct defaults must be injected in user-customizable commit/discard affordances - see SubmitButton! and DiscardButton!
                     [*tracked-token tracked-token
                      *disabled-commit (e/watch !disabled-commit)
                      *disabled-discard (e/watch !disabled-discard)]
                   (e/as-vec (e/amb (Fields unparsed-value) ; concurrent edits are what give us dirty tracking
                               (e/When show-buttons
                                 (e/amb (SubmitButton! :genesis genesis) (DiscardButton!))))))

           [edits commands] (split-edits-from-commands edits)
           dirty-count (count edits)
           [form-t form-v] (merge-edits edits)

           keyed-commands (collect-commands commands)
           [commit-t _] (::commit keyed-commands)
           [discard-t _] (::discard keyed-commands) ; supposed instant

           merged-form-v-with-unparsed-v (merge unparsed-value form-v)

           parsed-form-v (Parse merged-form-v-with-unparsed-v (Filter some? (e/Reconcile (or tempid (when form-t (e/snapshot (next-tempid! form-t)))))))

           validation-message (not-empty (ex-message parsed-form-v))]
       (reset! !disabled-commit (and (or (zero? dirty-count) validation-message) (not tracked-cmd)))
       (reset! !disabled-discard (and (zero? dirty-count) (not tracked-cmd)))
       (e/amb
         (let [[cmd err] (FormActions! [form-t parsed-form-v] (empty? validation-message) commit-t discard-t genesis auto-submit tracked-token tracked-cmd)]
           (dom/p (dom/props {:data-role "errormessage"})
                  (dom/text validation-message err))
           cmd)
         (FormDebugHelper debug (map-shorthand dirty-count unparsed-value form-v merged-form-v-with-unparsed-v parsed-form-v validation-message)))))))

(defmacro Form! [value Fields & {:as props}] ; note - the fields must be nested under this form - which is fragile and unobvious
  `(dom/form ; for form events. e.g. submit, reset, invalid, etc…
     (Form!* ~value ~Fields ~props)))

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
(e/defn Parse "Map over an edit, supposedly turning an edit into a command."
  [F edit] (e/for [[t x] edit] [t (F x)]))

(defmacro try-ok [& body] ; fixme inject sentinel
  `(try ~@body ::ok ; sentinel
        (catch InterruptedException e# ::interrupted)
        (catch Exception e# (doto ::fail (prn e#)))))

(e/declare effects* #_{})

(e/defn Service [forms]
  (e/client ; client bias, t doesn't transfer
    (prn `Service (e/Count forms) 'forms (e/as-vec (second forms)))
    (e/for [[t form guess] (e/diff-by first (e/as-vec forms))] ; reboot on new token
      (let [[effect & args] form
            Effect ((or effects* {}) effect (e/fn Default [& args] (doto ::effect-not-found (prn effect))))
            res (e/Apply Effect args)] ; effect handlers span client and server
        (prn 'final-res res)
        (case res
          nil (prn `res-was-nil-stop!) ; FIXME is it valid to ever return nil?
          ::interrupted (prn ::interrupted effect)
          ::ok (t) ; sentinel, any other value is an error
          (t ::rejected))))))

;; -----

(defn -add [index k sort-k record]
  (-> index
    (update :kvs assoc k record)
    (update :order assoc sort-k k)
    (update :rev-order assoc k sort-k)))

(defn -remove [index k sort-k]
  (-> index
    (update :kvs dissoc k)
    (update :order dissoc sort-k)
    (update :rev-order dissoc k)))

(e/defn Reconcile-by ; Experimental
  ([identify sort-key-fn authoritative-recs commands]
   (Reconcile-by identify identify sort-key-fn authoritative-recs commands))
  ([identify-record identify-command sort-key-fn authoritative-recs commands]
   (Reconcile-by identify-record identify-command sort-key-fn sort-key-fn authoritative-recs commands))
  ([identify-record identify-command sort-record-key-fn sort-command-keyfn authoritative-recs commands]
   (Reconcile-by identify-record identify-command sort-record-key-fn sort-command-keyfn compare authoritative-recs commands))
  ([identify-record identify-command sort-record-key-fn sort-command-keyfn comparator authoritative-recs commands]
   ;; Not a perfect impl. No e/as-vec (good thing) but still maintain a piece of state and still contains an e/watch and e/diff-by.

   (let [!index (atom {:kvs {}, :order (sorted-map-by comparator) :rev-order {}})] ; maintain order separately, a single sorted map is not enough to model order + direct access on arbitrary key.

     ;; 1. save expected records
     (e/for [[t cmd] commands]
       (let [k      (identify-command cmd)
             sort-k (sort-command-keyfn cmd)
             t (t/token `Reconcile-by t (fn ([]) ([err] (when (= ::discard err) (swap! !index -remove k sort-k)))))]
         (swap! !index -add k sort-k [nil [t cmd]])
         ))

     ;; 2. overwrite by authoritatives, cleanup on removal.
     (e/for [authoritative authoritative-recs] ; defaulting to time ordering is wrong, e/for doesn't guarantee branches mount order.
       (let [k      (identify-record authoritative)
             sort-k (sort-record-key-fn authoritative)]
         (swap! !index (fn [{:keys [rev-order] :as index}]
                         (let [prev-sort-k (get rev-order k sort-k)]
                           (-> (-remove index k prev-sort-k)
                             (-add k sort-k [authoritative nil])))))
         (e/on-unmount #(swap! !index (fn [{:keys [rev-order] :as index}]
                                        (let [sort-k (get rev-order k sort-k)]
                                          (-remove index k sort-k)))))))
     (let [{:keys [kvs order]} (e/watch !index)]
       (e/diff-by                       ; ugly
         (fn [[record command]] (if record (identify-record record) (identify-command (second command))))
         (map kvs (vals order)))        ; ugly
       ))))
