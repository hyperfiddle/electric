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

(e/defn Listen
  ([event-name] ($ Listen event-name identity))
  ([event-name cb-fn] ($ Listen event-name cb-fn nil))
  ([event-name cb-fn init]
   (e/input (->default init (listen node event-name cb-fn)))))

(defn with-done [x]
  (m/ap
    (let [dfv (m/dfv)
          done! #(dfv nil)]
      (m/amb [x done!] [x (m/? dfv)]))))

(defn ->task ; "to task" P the reason I called it to task is because you get a
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

(e/defn LatchingRelay                        ; Normally Open
  [closed?]                                  ; boolean
  (let [!state  (atom (e/snapshot closed?))]
    (if (e/watch !state)
      #(reset! !state false)
      (do (when-some [closed? ($ Drop 1 closed?)] ; Drop replace first flow value with nil. Could also throw Pending.
            (reset! !state closed?))
          nil))))

(def release! (constantly nil))

(defmacro latching [release! [binding v] & body]
  `(let [~binding ~v]
     (when-let [~release! ($ LatchingRelay ~binding)]
       ~@body)))

(e/defn ->Task [event cb]
  (e/input (->task (listen node event cb))))

(comment
  ;; With ->task
  (input
    (let [[v done!] ($ ->Task "input" #(.. % -target -value))]
      (when done!
        (case (e/server (prn v))
          (done!)))))

  ;; With LatchingRelay
  (input
    (let [v (e/input (->default nil (listen node "input" #(.. % -target -value))))]
      (when-let [done! ($ LatchingRelay v)]
        (case (e/server (prn v))
          (done!)))))

  ;; With latching
  (input
    (latching [v ($ Listen "input" #(.. % -target -value))]
      (case (e/server (prn v))
        (release!))))
  )


#_
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

(e/defn DLatchingRelay [closed? v]
  (let [release! ($ LatchingRelay closed?)]
    [(if release! (e/snapshot v) v) release!]))

(defmacro d-latching [[lexical-sym v] & body]
  `(let [v#                    ~v
         [~lexical-sym done!#] ($ DLatchingRelay v# v#)]
     (when done!#
       (binding [release! done!#]
         ~@body))))

(comment
  ;; With ->backpressured-task
  (button
    (let [[event done!] (e/input (->backpressured-task (listen node "click")))]
      (when done!
        (let [h (hash event)]
          (case (e/server (prn h))
            (done!))))))

  ;; With DLatchingRelay
  (button
    (let [event         (e/input (->default nil (listen node "click")))
          [event done!] ($ DLatchingRelay event event)]
      (when done!
        (let [h (hash event)]
          (case (e/server (prn h))
            (done!))))))

  ;; With d-latching
  (button
    (d-latching [event (e/input (->default nil (listen node "click")))]
      (let [h (hash event)]
        (case (e/server (prn h))
          (release!)))))
  )


(defn ->tasks [flow]
  (m/ap
    (let [S (i/spine)]
      (m/amb S
        (let [v (m/?> flow), id (random-uuid)]
          (S id {} [v #(S id {} nil)])
          (m/amb))))))


(comment
  ;; With ->tasks
  (input
    (e/cursor [[v done!] (->tasks
                           (listen-some node "keyup"
                             #(when (= "Enter" (.-key %))
                                (when-some [v (not-empty (.. % -target -value))]
                                  (set! (.. % -target -value) "")
                                  v))))]
      (case (e/server (prn v))
        (done!))))
  )


;;; Scratch zone


#_(e/defn LatchingRelay ; the opposite of a Latch in electronics is called a Pulse or "monostable multivibrator" or "One Shot"
  ([v] ($ LatchingRelay nil v))
  ([down-state v]
   (let [!state   (atom down-state)
         release! #(reset! !state down-state)]
     (reset! !state v)
     (when-not (= down-state (e/watch !state))
       release!))))

#_(e/defn LatchingRelay2 ; Normally Open
  ([v] ($ LatchingRelay2 some? v))
  ([closed? v]
   (let [!closed  (atom false)
         release! #(reset! !closed false)]
     (reset! !closed (closed? v))
     (when (e/watch !closed)
       release!))))

#_(e/defn DLatchingRelay
  ([v] ($ DLatchingRelay nil v))
  ([down-state v]
   (let [release! ($ LatchingRelay down-state v)]
     [(if release! (e/snapshot v) v) release!])))

#_(e/defn DLatchingRelay2
  ([v] ($ DLatchingRelay2 some? v))
  ([closed? v]
   (let [release! ($ LatchingRelay2 closed? v)]
     [(if release! (e/snapshot v) v) release!])))


;; Goal: navigate back and forward, button reflects pending tx

(defn click-allowed? [entity]
  (case (:status entity)
    :paid        false
    :paying      false
    :downloading false ; could be allowed to redirect user to downloads page
    :queued      false ; could be allowed to cancel queued tx
    :downloaded  false ; could be allowed to start game
    #_:else      (some? entity)
    ))

(comment
  (let [persisted-state (e/server (query-payment-state db user game))
        [e release!]    (button
                          (case status
                            :not-clickable …
                            :clickable
                            ($ DLatchingRelay2 (partial click-allowed? persisted-state)
                              (e/input (->default nil (listen "click" identity))))))]
    (when release!
      (let [h (hash e)]
        (case (e/server (prn h))
          (release!))))))

(comment
  (let [persisted-state   (e/server (query-payment-state db user game))
        [entity release!] (button
                            (let [e      (e/input (->default nil (listen "click" identity)))
                                  entity (if e (event->entity e) persisted-state)]
                              ($ DLatchingRelay (click-allowed? entity) entity)))]
    (when release!
      (case (e/server (prn entity))
        (release!)))))


(comment
  (defmacro on [node event F]
    `(let [e#        (e/input (->default nil (listen ~node ~event identity)))
           release!# ($ LatchingRelay (some? e#))]
       (when release!#
         (->> ($ F v#) ; interpret first non-pending return value as latch RESET
           ((fn [~'_] (release!#)))))))

  (defmacro snapshotting-on [node event F]
    `(let [e#             (e/input (->default nil (listen ~node ~event identity)))
           [v# release!#] ($ DLatchingRelay (some? e#) e#)]
       (->> ($ F v#)  ; interpret first non-pending return value as latch RESET
         ((fn [~'_] (release!#)))))))


;;; Missionary impl of latching-relay and d-latching-relay

(defn ->done [] (m/ap (let [dfv (m/dfv)] (m/amb #(dfv nil) (m/? dfv)))))
(defn ->latching-relay [>v]
  (m/ap
    (m/?< >v)
    (try (m/?> (->done)) (catch missionary.Cancelled _ (m/amb)))))
(defn ->d-latching-relay [>v]
  (m/ap
    (let [closed? (->box false)
          v (m/?> (m/eduction (remove (fn [_] (closed?))) >v))
          dfv (m/dfv)]
      [v (m/amb (closed? #(dfv (closed? false))) (m/? dfv))])))
#_(defn ->d-latching-relay2 [>v]
  (m/ap
    (let [>v (m/stream >v)
          closed? (->box false)
          v (m/?> (m/eduction (remove (fn [_] (closed?))) >v))
          done! (m/?> (->latching-relay >v))]
      (m/amb [v (comp (fn [_] done!) #(closed? false))] ))))
#_(defn init>
  ([>v] (init> nil >v))
  ([init >v] (m/reductions {} init >v)))

;; Higher level API

(e/defn Listen [event-name cb-fn]
  ;; Listen and return latest DOM event + a `done!` latch. The latch is useful
  ;; to replay effects for each events. It can be ignored if not needed. The
  ;; latch closes on each new DOM event (if not already closed). Call done! to
  ;; open the latch. The latch is optional, one can ignore it. TODO Could we
  ;; have Listen without latch? would it be useful? e.g. Listen returns latest
  ;; event in continous time, LatchingRelay would lift the `v` input into
  ;; `e/pure`
  (let [>v (listen node event-name cb-fn)
        >lr (->latching-relay >v)]
    [(e/input (m/reductions {} nil >v))
     (e/input (m/reductions {} nil >lr))]))

(e/defn Capture [event-name cb-fn] ; like Listen, but user must call done! before seeing next event
  (e/input (m/reductions {} [nil nil] (->d-latching-relay (listen node event-name cb-fn)))))

(comment
  (input
    (let [>v    (listen node "input" #(-> % .-target .-value))
          >lr   (->latching-relay >v)
          v     (e/input (m/reductions {} nil >v))
          done! (e/input (m/reductions {} nil >lr))]
      (when done!
        (case (e/server (prn v))
          (done!)))))

  (input
    (let [[v done!] ($ Listen "input" #(-> % .-target .-value))]
      (when done!
        (case (e/server (prn v))
          (done!)))))

  (button
    (let [[v done!] (e/input (m/reductions {} [nil nil] (->d-latching-relay (listen node "click" hash))))]
      (when done!
        (case (e/server (prn v))
          (done!)))))

  (button
    (let [[v done!] ($ Capture "click" hash)]
      (when done!
        (case (e/server (prn v))
          (done!)))))
  )



;;; scratch zone



;; WIP does LatchingRelay / DLatchingRelay matches ->task and ->backpressured-task?
;;     G: yes
;; TODO see if ->tasks can be implemented with LatchingRelay
;; TODO compare new `on` impl with `for-event-pending-switch` and `do-event-pending`
;; TODO come up with a good api for listen and ->default
;; TODO? come up with a nice api for LatchingRelay + listen
;; TODO? come up with a nice api for DLatchingRelay + listen
;; TODO polish DOM3 API
;; TODO test event handling in v2: port UI5 TodoMVC V2 to dom3 event api.
;;      copy to another ns, rename $ to new and e/input to new
