(ns .)

(let [input-fn    (case type
                    (:range :number) number-input-fn
                    (:text :textarea) text-input-fn)
      placeholder (or placeholder "")
      props       (cond-> {::dom/placeholder placeholder
                           ::ui/value        v
                           ::ui/blur-event   (p/fn [e]
                                               (swap! !state assoc k (input-fn e)))}
                          on-enter (assoc ::ui/keychord-event
                                          (p/fn [e]
                                            (case (:identifier e)
                                              "enter" (on-enter (input-fn e))))))]
  (if (= type :textarea)
    (ui/textarea ::dom/class [:w-100 :mih5] ... props)
    (do
      (ui/input ::ui/type    type
                ::dom/class  [:w-100]
                ::dom/min    min
                ::dom/max    max
                ::dom/step   step
                ...          props)
      (when (= type :range)
        (dom/span :text v)))))