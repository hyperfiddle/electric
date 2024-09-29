(ns hyperfiddle.cqrs0
  #?(:cljs (:require-macros hyperfiddle.cqrs0))
  (:require [contrib.data :refer [index-by]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.input-zoo0 :refer [Button!]]
            [missionary.core :as m]))

; commit/discard with staging area
; inputs - emit as you type - with a token
; stage - monitor edits and accumulate them
; button - batch all edits into single token, chained with upstream tokens

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

(e/defn Form*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    [ts kvs guess :as edits] ; concurrent edits are what give us dirty tracking
    & {:keys [debug commit discard show-buttons auto-submit]
       :or {debug false
            show-buttons true}}]
   (e/client
     (let [dirty-form (not-empty (apply merge (e/as-vec kvs))) ; collect fields into form, retain until commit/discard
           ;dirty-form-guess (apply merge (e/as-vec guess)) ; todo collisions
           form-t (let [ts (e/as-vec ts) #_(map first field-edits)]
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
                   :show-button show-buttons :auto-submit (when auto-submit dirty-form))
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
                          (nth discard 0) ; command
                          (nth discard 1)] ; prediction
                         (case (discard!) (e/amb))) ; otherwise discard now and swallow cmd, we're done
             ::commit (let [[dirty-form dirty-form-guess] (if commit (commit dirty-form #_dirty-form-guess) ; guess and form would be =
                                                            [dirty-form #_{e dirty-form}])] ; no entity id, only user can guess
                        [(fn token
                           ([] (btn-t) (form-t)) ; commit ok, reset controlled form
                           ([err] (btn-t err) #_(form-t err))) ; leave dirty fields dirty, activates retry button
                         dirty-form dirty-form-guess])))

         (e/When debug
           (dom/pre (dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str dirty-form :margin 80)))))))))

(defmacro Form [fields1 & kwargs]
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form* ~fields1 ~@kwargs))) ; place fields inside dom/form

(e/defn Reconcile-records [stable-kf sort-key as bs]
  (e/client
    (let [as! (e/as-vec as) ; todo differential reconciliation
          bs! (e/as-vec bs)]
      (->> (merge ; todo deep merge partial predictions
             (index-by stable-kf as!)
             (index-by stable-kf bs!))
        vals
        (sort-by sort-key)
        #_(drop (count bs!)) ; todo fix glitch
        (e/diff-by stable-kf)))))

(e/defn PendingController [kf sort-key edits xs]
  (let [!pending (atom {}) ; [id -> prediction]
        ps (val (e/diff-by key (e/watch !pending)))]
    (e/for [[t cmds predictions] edits]
      #_(prn 'PendingController t cmds predictions)
      (assert (= 1 (count predictions)))
      (let [[e record] (first predictions)]
          #_(prn 'pending-cmds cmds)
          (swap! !pending assoc e (assoc record ::pending t))
          (e/on-unmount #(swap! !pending dissoc e))
          (e/amb)))
    (Reconcile-records kf sort-key xs ps)))

(def *effects* {})

(e/defn Service
  [forms
   & {:keys [delay die] ; don't delay or die todomvc, client-only commands are impacted
      :or {delay 0, die false}}]
  (e/client ; client bias, t doesn't transfer
    (prn `Service (e/Count forms) 'forms (e/as-vec (second forms)))
    (e/for [[t form guess] forms #_(e/Filter some? forms)] ; remove any accidental nils from dom
      #_(case (e/Task (m/sleep delay form)) (if die [::die]))
      ;(prn 'Service form 'now!) (e/server (prn 'Service form 'now!))
      (let [[effect & args] form
            Effect (*effects* effect (e/fn default [& args] (doto ::effect-not-found (prn effect))))
            res (e/Apply Effect args)] ; effect handlers span client and server
        ;(prn 'Service form 'result res) (e/server (prn 'Service form 'result res))
        (prn 'final-res res)
        (case res
          nil (prn 'res-was-nil-stop!)
          ::ok (t) ; sentinel, any unrecognized value is an error
          (t ::rejected)))))) ; feed error back into control for retry affordance