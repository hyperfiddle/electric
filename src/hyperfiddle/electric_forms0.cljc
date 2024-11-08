(ns hyperfiddle.electric-forms0
  #?(:cljs (:require-macros hyperfiddle.electric-forms0))
  (:require [contrib.data :refer [index-by]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

;; Simple controlled inputs (dataflow circuits)

(e/defn Input [v & {:keys [maxlength type parse] :as props
                    :or {maxlength 100 type "text" parse identity}}]
  (e/client
    (e/with-cycle [v (str v)] ; emits signal of current state
      (dom/input (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
        (when-not (dom/Focused?) (set! (.-value dom/node) v))
        (dom/On "input" #(-> % .-target .-value (subs 0 maxlength) parse) v))))) ; emit on boot, rebuild on reset

(e/defn Checkbox [checked & {:keys [id label parse] :as props
                             :or {id (random-uuid) parse identity}}]
  (e/client
    (e/amb
      (e/with-cycle [checked checked]
        (dom/input (dom/props {:type "checkbox", :id id})
          (dom/props (dissoc props :id :label :parse))
          (when-not (dom/Focused?) (set! (.-checked dom/node) checked))
          (dom/On "change" #(-> % .-target .-checked parse) checked)))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

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

; include the error for the pending monitor - unclear if right
; encapsulate errors, the form already saw it when forwarding here

(e/defn Checkbox! [k checked & {:keys [id label parse edit-monoid] :as props
                                :or {id (random-uuid) parse identity edit-monoid hash-map}}]
  ; todo esc?
  (e/client
    (e/amb
      (dom/div ; checkboxes don't have background so style wrapper div
        (dom/props {:style {:display "inline-block" :width "fit-content"}})
        (let [[e t err input-node]
              (dom/input (dom/props {:type "checkbox", :id id}) (dom/props (dissoc props :id :label :parse))
                (let [e (dom/On "change" identity) [t err] (e/Token e)] ; single txn, no concurrency
                  [e t err dom/node]))
              editing? (dom/Focused? input-node)
              waiting? (some? t)
              error? (some? err)
              dirty? (or editing? waiting? error?)]
          (when-not dirty? (set! (.-checked input-node) checked))
          (when error? (dom/props {:aria-invalid true}))
          (when waiting? (dom/props {:aria-busy true}))
          (if waiting? [t (edit-monoid k ((fn [] (-> e .-target .-checked parse))))] (e/amb))))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn Button!
  "Transactional button with busy state. Disables when busy."
  [directive & {:keys [label disabled type form] :as props
                :or {type :button}}] ; default type in form is submit
  (dom/button (dom/text label) ; (if err "retry" label)
    (dom/props (-> props (dissoc :label :disabled) (assoc :type type)))
    (let [x (dom/On "click" identity nil) ; (constantly directive) forbidden - would work skip subsequent clicks
          [btn-t err] (e/Token x)] ; genesis
      (dom/props {:disabled (or disabled (some? btn-t))})
      (dom/props {:aria-busy (some? btn-t)})
      (dom/props {:aria-invalid (some? err)})
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

(e/defn FormSubmit! ; dom/node must be a form
  [directive & {:keys [disabled show-button label auto-submit form] :as props}]
  (e/client
    (let [[t err] (e/amb
                    ;; FIXME pressing Enter while an autosubmit commit is running will trigger a double submit and hang the app
                    (e/When show-button (e/apply Button! directive (mapcat identity (-> props (dissoc :form) (assoc :type :submit))))) ; genesis ; (e/apply Button directive props) didn't work - props is a map
                    (let [submit-event (dom/On "submit" #(do (.preventDefault %) (.stopPropagation %) (when-not (or auto-submit disabled) %)) nil)]
                      submit-event ; force signal
                      (if auto-submit
                        (e/Token form)
                        (e/When (not show-button) ; show-buttons will render an <button type=submit> auto handling Enter
                          (do (dom/On "keypress" (fn [e] (let [target (.-target e)] ; submit form on enter
                                                           (when (and (= "Enter" (.-key e)) (= "INPUT" (.-nodeName target)))
                                                             (.stopPropagation e)
                                                             (.requestSubmit (.-form target)) ; fire submit event
                                                             nil))) nil)
                              (let [[t err :as token] (e/Token submit-event)]
                                (dom/props {:aria-invalid (some? err)})
                                token))))))]
      (if t ; TODO unify with FormSubmit! and Button!
        (let [[form-t form-v] form]
          [(fn token
             ([] (t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
             ([err] (t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
           (if form-t [directive form-v] [directive nil])]) ; compat
        (e/amb)))))

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

(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    edits ; concurrent edits are what give us dirty tracking
    & {:keys [debug commit discard show-buttons auto-submit edit-merge genesis name edit-monoid]
       :or {debug false show-buttons true edit-merge merge genesis false edit-monoid hash-map}}]
   (e/client
     (let [dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           show-buttons (case show-buttons ::smart (not clean?) show-buttons)
           [form-t form-v :as form] (invert-fields-to-form edit-merge (e/as-vec edits))
           [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                 ::commit :label "commit"  :disabled clean?
                                 :form form
                                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                                 :show-button show-buttons)
           [_ _ :as ?d] (FormDiscard! ::discard :form form :disabled clean? :label "discard" :show-button show-buttons)]
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

         (e/When debug
           (dom/span (dom/text " " dirty-count " dirty"))
           (dom/pre #_(dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str form-v :margin 80)))))))))

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

(declare Service)

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

(def effects* {})

(defmacro try-ok [& body] ; fixme inject sentinel
  `(try ~@body ::ok ; sentinel
     (catch Exception e# (doto ::fail (prn e#)))))

(e/defn Service [forms]
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