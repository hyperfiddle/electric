(ns photon-eff1)


(p/def Throw)
(deftype TransactEffect [txn])

(let [!x (atom {:input1 "hello"
                :select1 "a"})
      x (p/watch !x)]

  (try
    (binding [Throw (p/fn [e]
                          ; interpreter
                          (cond (instance? TransactEffect e)
                                (let [busy (try
                                             (p/server (d/transact! (.-txn e))) ; impulse
                                             false (catch Pending _ true))]
                                  (assert busy)
                                  busy)))]

      (dom/dl
        (dom/dt "ui/input")
        (dom/dd (let [v (ui/input (:input1 x))]
                  (throw (->Effect [:input1 v]))))

        (dom/dt "button")
        (dom/dd (ui/button {::ui/input-event
                            (fn [e]
                              (let [busy (Throw.
                                           (new TransactEffect [{:db/id id, :task/status status}])
                                           busy)]
                                (dom/set-property! dom/node "aria-busy" busy)))}))

        (dom/dd (ui/button {::ui/input-event
                            (fn [e]
                              (let [busy (Throw. (new TransactEffect [{:db/id id, :task/status status}]))]
                                (dom/set-property! dom/node "aria-busy" busy)))}))

        ))


    (catch Effect [a v]
      (resume
        (swap! !x assoc a v))))

  (dom/pre (pprint-str x)))


; a continuation cares about WHEN you call it, you can choose NOT to call it
; you cannot defer the computation, you always need a current value in CT