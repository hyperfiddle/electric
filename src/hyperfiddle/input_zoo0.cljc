(ns hyperfiddle.input-zoo0
  (:require clojure.string
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

;; Crude uncontrolled inputs, rarely useful

(e/defn Input* [& {:keys [maxlength type parse] :as props
                   :or {maxlength 100 type "text" parse identity}}]
  (e/client ; explicit site on all controls for compat with neutral callers
    (dom/input (dom/props (-> props (dissoc :parse)
                            (assoc :maxLength maxlength :type type)))
      (dom/On "input" #(-> % .-target .-value (subs 0 maxlength) parse) ""))))

(e/defn Checkbox* [& {:keys [id label parse] :as props
                      :or {id (random-uuid) parse identity}}]
  (e/client
    (e/amb
      (dom/input (dom/props {:type "checkbox", :id id})
        (dom/props (dissoc props :id :label :parse))
        (dom/On "change" #(-> % .-target .-checked parse) false))
      (e/When label (dom/label (dom/props {:for id}) (dom/text label))))))

(e/defn InputSubmitCreate!
  "optimistic, cancel & retry are forwarded to optimistic list item's InputSubmit!
buffers (dirty), commit, discard bundled as enter/esc"
  [& {:keys [maxlength type parse] :as props
      :or {maxlength 100 type "text" parse identity}}]
  (e/client
    (dom/input (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
      (letfn [(read! [node] (not-empty (subs (.-value node) 0 maxlength)))
              (read-clear! [node] (when-some [v (read! node)] (set! (.-value node) "") v))
              (submit! [e] (let [k (.-key e)]
                             (cond
                               (= "Enter" k) (parse (read-clear! (.-target e)))
                               (= "Escape" k) (do (set! (.-value dom/node) "") nil)
                               () nil)))]
        #_(PendingMonitor) ; the optimistic list item is responsible for pending/retry affordances
        (dom/On-all "keydown" submit!)))))

(e/defn InputSubmitCreate?!
  "transactional chat input with busy state. Supports rapid submit, sending
concurrent in-flight submits to the server which race. ?! marks this control
as an anti-pattern because it has no error handling: rejected edits are silently
lost. Fixing this requires form semantics, see upcoming tutorial."
  [& {:keys [maxlength type parse] :as props
      :or {maxlength 100 type "text" parse identity}}]
  (e/client
    (dom/input (dom/props (-> props (dissoc :parse) (assoc :maxLength maxlength :type type)))
      (letfn [(read! [node] (not-empty (subs (.-value node) 0 maxlength)))
              (read-clear! [node] (when-some [v (read! node)] (set! (.-value node) "") v))
              (submit! [e] (let [k (.-key e)]
                             (cond
                               (= "Enter" k) (parse (read-clear! (.-target e)))
                               (= "Escape" k) (do (set! (.-value dom/node) "") nil)
                               () nil)))]
        (let [edits (dom/On-all "keydown" submit!)] ; concurrent pending submits
          (dom/props {:aria-busy (pos? (e/Count edits))})
          edits)))))