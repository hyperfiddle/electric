(ns)

; L: the problem is how to express causality cycles in a photon program

(p/defn App []
  (let [!temperature (atom 0) temperature (p/watch !temperature)]
    (->> (ui/input {::ui/value temperature})
         js/parseFloat
         (reset! !temperature))))

(p/defn App []
  (let [!conn (atom {}) db (p/watch !conn)]
    (let [v (js/parseFloat (ui/input {::ui/value (:user/temperature db)}))]
      (swap! !conn {} {:user/temperature v})))) ; lose history

(p/defn App2 []
  (let [!conn (d/connect ...) db (p/watch !conn)]
    (let [v (js/parseFloat (ui/input {::ui/value (:user/temperature (d/entity db 1))}))]
      (d/transact! !conn [[:db/add 1 :user/temperature v]]))))

(let [!x (atom 0) x @x]
  (reset! !x (inc x)))

(doall (iterate inc 0))




; loop/recur is not about creating cycles
; it's about causality chains whose length is dynamic – unlike p/for




(let [!conn (atom {}) db (p/watch !conn)])

(p/defn App []
  (let [v1 (if open1 (js/parseFloat (ui/input {::ui/value (:user/temperature db)})))
        v2 (if open2 (js/parseFloat (ui/input {::ui/value (:user/temperature db)})))
        tx [(when v1 {:user/temperature v1})
            (when v2 {:user/temperature v2})]]
    tx))








(comment

  (def !conn ...)

  (p/defn UserBody []
    (when-some [e (ui/button2 busy "create-user")]
      [{:user/name (password)}]))

  (binding [db (p/watch !conn) #_(:db-after (d/with (p/watch !conn)))]
    (p/client
      (when-let [tx (Popover. UserBody)]
        (try (p/server (d/transact! !conn tx))
             (catch Pending e nil)))))



  (loop [db (p/watch !conn)]
    (p/client
      (when-some [tx (Popover. UserBody)]
        (recur
          (try (p/server (:db-after (d/with db tx)))
               (catch Pending e nil))))
      nil))



  (loop [x 0] (recur (inc x)))
  (iterate inc 0)



  )


