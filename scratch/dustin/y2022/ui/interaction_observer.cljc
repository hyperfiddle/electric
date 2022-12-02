

(p/defn IntersectionObserverDemo []
  (let [data (range)
        !render-count (atom 20)
        render-count (p/watch !render-count)]
    (dom/div {:class ["root-node"]}

      (let [obs (js/IntersectionObserver.
                  (fn [t] (swap! !render-count + 20))
                  #js {:root root-node})]
        (new (m/observe (fn [!]
                          (! nil)
                          #(j/call obs :disconnect))))

        (p/for [c (take render-count data)]
          (dom/div c))
        (dom/div
          (new (m/observe
                 (fn [!]
                   (j/call obs :observe dom/node)
                   (! nil)
                   #(j/call obs :unobserve dom/node))))
          "Loading...")))))


#?(:cljs
   (defn observe> [{:keys [target-node]}]
     ))

(p/defn IntersectionObserverDemo []
  (let [data (range)
        !render-count (atom 20)
        render-count (p/watch !render-count)]
    (dom/div {:class ["root-node"]}

      (let [obs (do (println `observer-mount)
                    ::obs)]
        (m/observe (fn [!] #(println `observer-unmount)))


        (p/for [c (take render-count data)]
          (dom/div c))
        (dom/div
          (pr-str obs)
          "Loading...")))))







(p/defn IntersectionObserverDemo []
  (let [data (range)
        !render-count (atom 20)
        render-count (p/watch !render-count)]
    (dom/div {:class ["root-node"]}
      (let [observer (js/IntersectionObserver.
                       (fn [t]
                         (swap! !render-count + 20))
                       #js {:root dom/node})]
        (p/for [c (take render-count data)]
          (dom/div c))
        (dom/div
          (j/call observer :observe dom/node)
          "Loading...")))))