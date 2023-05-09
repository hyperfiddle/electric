(ns dustin.y2023.mark)

(e/defn ElectrifyJS [o config special-property-handling
                     Init!]
  (let [special-properties (set (keys special-property-handling))]
    (when (e/for [property (remove #(special-properties %) (keys config))]
            (let [val (get config property)]
              (if (fn? val)
                (let [!event (atom [])]
                  (.addEventListener o (name property) (fn [e] (println "emitting" property) (swap! !event conj e)))
                  (e/for [event (e/watch !event)]
                    (println "handling" property)
                    (new val event))
                  (reset! !event []))
                (.set js/Reflect o (name property) (clj->js val)))))
      (dom/div (new Init! o)))
    (e/for [special-property (keys special-property-handling)]
      (let [Handler (get special-property-handling special-property)]
        (new Handler o (get config special-property))))
    o))