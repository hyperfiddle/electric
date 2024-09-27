(ns hyperfiddle.cqrs0
  #?(:cljs (:require-macros hyperfiddle.cqrs0))
  (:require [contrib.data :refer [index-by]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.input-zoo0 :refer [Button!]]))

; commit/discard with staging area
; inputs - emit as you type - with a token
; stage - monitor edits and accumulate them
; button - batch all edits into single token, chained with upstream tokens

(defmacro Field [e a control]
  `(e/for [[t# v#] ~control] ; zero or one edits
     [t# [[::update ~e ~a v#]]])) ; ::inline-edit, ::naked-edit ?

#?(:cljs (defn- blur-active-form-input! [form]
           (when-let [focused-input (.-activeElement js/document)]
             (when (.contains form focused-input)
               (.blur focused-input)))))

(e/defn FormDiscard! ; dom/node must be a form
  [directive & {:keys [disabled token show-button label] :as props}]
  (e/client
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %)
                       (.reset dom/node) nil) nil) ; proxy Esc to form's "reset" event
    (e/amb
      #_(e/When show-button) (Button! directive :disabled disabled :label label) ; todo fix
      (let [e (dom/On "reset" #(do (.log js/console %) (.preventDefault %)
                                 (blur-active-form-input! (.-target %)) %) nil)
            [t err] (e/RetryToken e)]
        (if t [t directive] (e/amb))))))

(e/defn FormSubmit!
  [directive & {:keys [disabled token show-button label auto-submit] :as props}]
  (e/client
    (e/amb
      #_(e/When show-button) (Button! directive :disabled disabled :label label) ; todo fix
      (let [e (dom/On "submit" #(do (.preventDefault %) (.stopPropagation %)
                                  (when-not disabled %)) nil)
            [t err] (e/RetryToken (or e auto-submit))]
        (when (some? err) (dom/props {:aria-invalid true})) ; glitch
        (if t [t directive] (e/amb))))))

(defn merge-cmds [& txs] (vec (apply concat txs))) ; datomic style

;; Same as cqrs0/Stage, but also handles form submit and reset events
(e/defn Form* "implies an explicit txn monoid" ; doesn't work on raw values
  ([[ts txs :as edits] ; amb destructure
    & {:keys [debug merge-cmds commit discard show-buttons auto-submit]
       :or {merge-cmds merge-cmds
            debug false
            show-buttons true}}]
   (e/client
     (let [form (not-empty (apply merge-cmds (e/as-vec txs))) ; retain until commit/discard
           form-t (let [ts (e/as-vec ts)]
                    (fn token
                      ([] (doseq [t ts] (t)))
                      #_([err] (doseq [t ts] (t err ::keep))))) ; we could route errors to dirty fields, but it clears dirty state
           dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           [btn-ts _ :as btns]
           (e/with-cycle* first [btns (e/amb)]
             (let [busy? (e/Some? btns)]
               (e/amb ; todo progress
                 (FormSubmit! ::commit :disabled (e/Reconcile (or busy? clean?))
                   :label (e/Reconcile (if busy? "commit" "commit"))
                   :show-button show-buttons :auto-submit (when auto-submit form))
                 (FormDiscard! ::discard :disabled clean? :label (if busy? "cancel" "discard") :show-button show-buttons)
                 (e/When debug (dom/span (dom/text " " dirty-count " dirty"))))))
           discard! (fn [] (btn-ts) (form-t))] ; reset controlled form and both buttons, cancelling any in-flight commit

       (e/amb
         (e/for [[btn-t cmd] btns]
           (case cmd ; does order of burning matter?
             ::discard (if discard ; user wants to run an effect on discard
                         [(fn token ; emit discard and stay busy (lag!)
                            ([] (discard!)) ; user says discard ok
                            ([err] (btn-t err))) ; user rejected discard
                          discard]
                         (case (discard!) (e/amb))) ; otherwise discard now and swallow cmd, we're done
             ::commit [(fn token
                         ([] (btn-t) (form-t)) ; commit ok, reset controlled form
                         ([err] (btn-t err) #_(form-t err))) ; leave dirty fields dirty, activates retry button
                       (if commit (commit form) form)])) ; commit as atomic batch

         (e/When debug
           (dom/pre (dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str form :margin 80)))))))))

(defmacro Form [fields1 & kwargs]
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form* ~fields1 ~@kwargs))) ; place fields inside dom/form

(e/defn Reconcile-records [stable-kf sort-key as bs]
  (e/client
    (let [as! (e/as-vec as) ; todo differential reconciliation
          bs! (e/as-vec bs)]
      (->> (merge
             (index-by stable-kf as!)
             (index-by stable-kf bs!))
        vals
        (sort-by sort-key)
        #_(drop (count bs!)) ; todo fix glitch
        (e/diff-by stable-kf)))))

(e/defn PendingController [kf sort-key edits xs]
  (let [!pending (atom {}) ; [id -> prediction]
        ps (val (e/diff-by key (e/watch !pending)))]
    (e/for [[t xcmd predictions] edits]
      (assert (= 1 (count predictions)))
      (let [[e record] (first predictions)]
        (prn 'pending-cmd xcmd)
        (swap! !pending assoc e (assoc record ::pending t))
        (e/on-unmount #(swap! !pending dissoc e))
        (e/amb)))
    (Reconcile-records kf sort-key xs ps)))

#?(:clj (defn run-batch! [cmds & {:keys [delay die]}] (prn 'cmds cmds)
          (try (Thread/sleep delay) (when die (assert false "die"))
            (doseq [[effect! & args] cmds]
              (apply effect! args)) ; transparent effect type - facilitates testing
            (doto ::ok (prn 'tx-ok)) ; sentinel, encodes "either" into single object (see todomvc command chaining)
            (catch InterruptedException _) ; never seen
            (catch Throwable e ::fail)))) ; anything but ::ok is an error

(e/defn Service
  [expand-tx-effects txs
   & {:keys [delay die]
      :or {delay 500 die true}}]
  (e/client ; client bias, t doesn't transfer
    (prn (e/Count txs) 'txs #_(e/as-vec (second txs)))
    (e/for [[t cmds] (e/Filter some? txs)] ; remove any accidental nils from dom
      (let [res (e/server ; secure effect interpretation
                  (e/Offload #(do (run-batch! (expand-tx-effects cmds)
                                    :delay delay :die die))))]
        res
        (case res
          ::ok (t) ; sentinel, unrecognized value is error
          (t res)))))) ; feed error back into control for retry affordance