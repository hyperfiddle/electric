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

(e/defn Field* [e a Control]
  (e/for [[t v] (Control)]
    [t [[::update e a v]]]))

(defmacro Field [e a body] `(Field* ~e ~a (e/fn [] ~body)))

(defn merge-cmds [& txs] (vec (apply concat txs))) ; datomic style

(e/defn Stage "implies an explicit txn monoid" ; doesn't work on raw values
  ([edits] (Stage edits :merge-cmds merge-cmds))
  ([[ts txs :as edits] ; amb destructure
    & {:keys [debug merge-cmds]
       :or {merge-cmds merge-cmds
            debug false}}]
   (e/client
     (let [form (apply merge-cmds (e/as-vec txs)) ; retain until commit/discard
           form-t (let [ts (e/as-vec ts)]
                    (fn token
                      ([] (doseq [t ts] (t)))
                      #_([err] (doseq [t ts] (t err ::keep))))) ; we could route errors to dirty fields, but it clears dirty state
           dirty-count (e/Count edits)
           [btn-ts _ :as btns]
           (e/with-cycle* first [btns (e/amb)]
             (let [busy? (e/Some? btns)]
               (e/amb ; todo progress
                 (Button! ::commit :disabled (zero? dirty-count) :label (if busy? "commit" "commit"))
                 (Button! ::discard :disabled (zero? dirty-count) :label (if busy? "cancel" "discard"))
                 (e/When debug (dom/span (dom/text " " dirty-count " dirty"))))))]
       (e/amb
         (e/for [[btn-t cmd] btns]
           (case cmd ; does order of burning matter?
             ::discard (case ((fn [] (btn-ts) (form-t))) ; clear any in-flight commit yet outstanding
                         (e/amb)) ; never seen, btn token already spent
             ::commit [(fn token
                         ([] (btn-t) (form-t)) ; reset controlled form
                         ([err] (btn-t err) #_(form-t err))) ; leave dirty fields dirty, activates retry button
                       form])) ; commit as atomic batch

         (e/When debug
           (dom/pre (dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str form :margin 80)))))))))

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
      (let [[id record] (first predictions)]
        (prn 'pending-cmd xcmd)
        (swap! !pending assoc id (assoc record ::pending t))
        (e/on-unmount #(swap! !pending dissoc id))
        (e/amb)))
    (Reconcile-records kf sort-key xs ps)))