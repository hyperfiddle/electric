(ns photon-eff2)

(defn ->Effect [])

lazy-seq

(def !busy (atom true))

(let [!x (atom {:input1 "hello"
                :select1 "a"})
      x (p/watch !x)]

  (try
    (dom/dl
      (dom/dt "ui/input")
      (dom/dd (let [v (ui/input (:input1 x))]
                (throw (->Effect [:input1 v]))
                #_(throw `(swap! !x assoc :input1 ~v))))

      (dom/dt "button")
      (dom/dd (ui/button {::ui/input-event
                          (fn [e]
                            (let [busy (throw (new TransactEffect [{:db/id id, :task/status status}]))]
                              (dom/set-property! dom/node "aria-busy" (p/watch !busy))))})))
    (catch TransactEffect txn
      (try
        (p/server (d/transact! txn))
        (resume false) (catch Pending _ (resume true)))


      (let [busy (try
                   (p/server (d/transact! txn))
                   false (catch Pending _ true))]

        ; what is this?
        [(resume busy)
         (resume (not busy))]

        ; what is this?
        (do (reset! !busy true)
            (reset! !busy false))

        ))

    (catch Effect [a v]
      (resume
        (swap! !x assoc a v))))

  (dom/pre (pprint-str x)))

; Today, in Photon, if 2 exceptions are thrown simultaneously, what is the meaning?
(defn slow-identity [x] (Thread/sleep 1100) x)
(tests
  rcf/*timeout* := 1000
  (with (p/run (tap (try
                      [(p/server (p/wrap (slow-identity 2)))
                       (assert false)]
                      (catch Pending _ (tap :pending))
                      (catch AssertionError _ (tap :error)))))
        % := :pending
        % := :error
        % := [_ _]
        ))

; before vector, evaluate arguments in strict order
; check that all args are successful
; if any are Failure, don't run vector, return FIRST exception (arbitrary!)
;    L: like in Java. Note Java cannot have simultaneous exceptions so there's not a quesiton here.
;    L: at least the result is consistent with the equivalent Clojure program
;        (if we chose the last exception, the meaning has changed in Clojure)
; What if you don't catch AssertionError? What if you don't catch Pending?
