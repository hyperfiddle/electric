
(p/defn Focused? []
  (p/with-cycle [focused false]
    (let [blur (not (some? (Event. "blur" false)))
          focus (some? (Event. "focus" false))]
      (case focused
        true focus
        false blur))))

(p/defn Focused? []
  (p/with-cycle [focused false]
    (case focused
      true (not (some? (Event. "blur" false)))
      false (some? (Event. "focus" false)))))



(case focused
  true focus
  false blur)

(case focused
  true (if blur false true)
  false (if focus true false))


(p/with-cycle [s false]
  (boolean (if s (Event. "focus" false) (Event. "blur" false))))



(p/defn Input2b [controlled-value] ; broken
  (p/with-cycle [v ::unknown]
    (dom/input {:type "text"}
      (if (Focused?.)
        (some-> (Event. "input" false) .-target .-value) ; broken, impulse not state
        (do (.setAttribute dom/node "value" controlled-value) controlled-value)))))

(p/with-cycle [state {:edit? false}]
  (if (:edit? state)
    (merge state
           {:edit? (not (some? (Event. "blur" false)))}
           (when-some [e (Event. "input" false)]
             {:value (.-value (.-target e))})) ; use local value
    (do (.setAttribute node "value" (str controlled-value)) ; throw away local value
        {:edit? (some? (Event. "focus" false)) ; never busy - process synchronously
         :value controlled-value})))

(p/defn Input2c [controlled-value]
  (dom/input {:type "text"}
    (case (Focused?.)
      true (p/with-cycle [v ::unknown] (some-> (Event. "input" false) .-target .-value))
      false (do (.setAttribute dom/node "value" controlled-value) controlled-value))))

(p/defn Input2c [controlled-value]
  (dom/input {:type "text"}
    (p/with-cycle [v ::unknown]
      (case (Focused?.)
        true (some-> (Event. "input" false) .-target .-value)
        false (do (.setAttribute dom/node "value" controlled-value) controlled-value)))))

;; continued

(p/defn Focused? []
  (doto
    (p/with-cycle [focused false]
      (case focused
        true (not (some? (dom/Event. "blur" false)))
        false (some? (dom/Event. "focus" false))))
    (println 'Focused?)))

(p/defn Input2c [controlled-value]
  (dom/with
    (dom/dom-element dom/node "input")
    (.setAttribute dom/node "type" "text")
    (p/with-cycle [input-value nil]
      (case (Focused?.)
        false (do (.setAttribute dom/node "value" controlled-value) controlled-value)
        true (or (some-> (dom/Event. "input" false) .-target .-value) input-value)))))

(p/defn Input2c2 [controlled-value]
  (dom/with
    (dom/dom-element dom/node "input")
    (.setAttribute dom/node "type" "text")
    (case (Focused?.)
      false (do (.setAttribute dom/node "value" controlled-value) controlled-value)
      true (p/with-cycle [input-value controlled-value]
             (or (some-> (dom/Event. "input" false) .-target .-value) input-value)))))

(p/defn Input2d [controlled-value]
  (dom/element "input"
               (.setAttribute dom/node "type" "text")
               (p/with-cycle [input-value nil]
                 (case (Focused?.)
                   false (do (.setAttribute dom/node "value" controlled-value) controlled-value)
                   true (or (some-> (dom/Event. "input" false) .-target .-value) input-value)))))

(p/defn Input2e [controlled-value]
  (dom/input {:type "text"}
    (p/with-cycle [input-value nil]
      (case (Focused?.)
        false (do (.setAttribute dom/node "value" controlled-value) controlled-value)
        true (or (some-> (dom/Event. "input" false) .-target .-value) input-value)))))

; todo macro version