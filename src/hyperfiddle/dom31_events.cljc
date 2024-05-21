(ns hyperfiddle.dom31-events
  (:require
   [hyperfiddle.incseq :as i]
   [missionary.core :as m])
  (:import
   (missionary Cancelled)))

#?(:cljs
   (defn listen
     ([node typ] (listen node typ identity))
     ([node typ f] (listen node typ f {}))
     ([node typ f opts]
      (m/observe (fn [!]
                   (let [! #(! (f %)), opts (clj->js opts)]
                     (.addEventListener node typ ! opts)
                     #(.removeEventListener node typ ! opts)))))))

#?(:cljs
   (defn listen-some
     ([node typ] (listen-some node typ identity))
     ([node typ f] (listen-some node typ f {}))
     ([node typ f opts]
      (m/observe (fn [!]
                   (let [! #(some-> (f %) !), opts (clj->js opts)]
                     (.addEventListener node typ ! opts)
                     #(.removeEventListener node typ ! opts))))
      ;; alternative
      #_(m/eduction (filter some?) (listen node typ f opts)))))

(defn with-done [x]
  (m/ap
    (let [dfv (m/dfv)
          done! #(dfv nil)]
      (m/amb [x done!] [x (m/? dfv)]))))

(defn ->task ; "to task" P the reason I called it to task is because you get a
  ; value and there's a task to be run on that value (by the user) until the
  ; user calls `done!` .
  "Take a discrete `flow` and an optional `init` value. Produce `[init nil]` then
  `[value done!]` until `done!` is called, then immediately produce `[value
  nil]`. Or immediately produce another `[value' done!']` if `flow` produces a
  new value. No deduplication."
  ([flow] (->task nil flow))
  ([init flow]
   (->> (m/ap (let [v (m/?< flow)]
                (try (m/?> (with-done v))
                     (catch Cancelled _ (m/amb)))))
     (m/reductions {} [init nil]))))



(comment
  ;; left
  (defn ->default
    ([flow] (->default nil flow))
    ([init flow] (m/reductions {} init flow)))
  ;; right

  (defn with-done-2 []
    (m/ap
      (let [dfv   (m/dfv)
            done! #(dfv nil)]
        (m/amb done! (m/? dfv)))))

  (defn flip-flop [flow]
    (m/reductions {} nil
      (m/ap
        (do (m/?< flow)
            (try (m/?> (with-done-2))
                 (catch Cancelled _ (m/amb)))))))


  (input (props {:type "text"})
    (let [v     (e/input (->default nil (listen "input" #(.. % -target -value))))
          done! (e/input (flip-flop (e/pure v)))]
      (when (some? done!)
        (case (e/server (prn v))
          (done!)))))

  (e/defn Latch ; the opposite of a Latch in electronics is called a Pulse or "monostable multivibrator" or "One Shot"
    ([v] ($ Latch nil v))
    ([down v]
     (let [!state   (atom down)
           release! #(reset! !state down)]
       (reset! !state v)
       (when-not (= ::nil (e/watch !state))
         release!))))

  (input (props {:type "text"})
    (let [v        (e/input (->default nil (listen "input" #(.. % -target -value))))
          release! ($ Latch v)]
      (when release!
        (case (e/server (prn v))
          (release!)))))


  )

(defn ^:no-doc ->box
  ([] (->box nil))
  ([init] (let [o (object-array 1)]
            (aset o (int 0) init)
            (fn ([] (aget o (int 0))) ([v] (aset o (int 0) v))))))

(defn ->backpressured-task
  ([flow] (->backpressured-task nil flow))
  ([init flow]
   (->> (m/ap
          (let [busy? (->box)
                v (m/?> (m/eduction (remove (fn [_] (busy?))) flow))
                dfv (m/dfv), done! #(dfv nil)]
            (m/amb
              [v (busy? done!)]
              [v (busy? (m/? dfv))])))
     (m/reductions {} [init nil]))))


(comment
  (button
    (let [e        (e/input (->default nil (listen "click" identity)))
          release! ($ Latch e)]
      (when release!
        (let [h (hash (e/snapshot e))]
          (case (e/server (prn h))
            (release!))))))

  (e/defn DLatch
    ([v] ($ DLatch nil v))
    ([down v]
     (let [release! ($ Latch down v)]
       [(if release! (e/snapshot v) v) release!])))

  (button
    (let [[e release!] ($ DLatch (e/input (->default nil (listen "click" identity))))]
      (when release!
        (let [h (hash e)]
          (case (e/server (prn h))
            (release!))))))

  )

(defn ->tasks [flow]
  (m/ap
    (let [S (i/spine)]
      (m/amb S
        (let [v (m/?> flow), id (random-uuid)]
          (S id {} [v #(S id {} nil)])
          (m/amb))))))


(comment
  (e/client (prn (e/server (e/client (e/server 1)))))
  )
