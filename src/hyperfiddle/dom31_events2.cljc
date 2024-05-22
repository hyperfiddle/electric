(ns hyperfiddle.dom31-events2
  (:require [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  (:import [missionary Cancelled]))

#?(:cljs
   (defn listen "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)`. Does
  not relieve backpressure. `opts` can be a clojure map."
     ([node event-type] (listen node event-type identity))
     ([node event-type f] (listen node event-type f {}))
     ([node event-type f opts]
      (m/observe (fn [!]
                   (let [! #(! (f %)), opts (clj->js opts)]
                     (.addEventListener node event-type ! opts)
                     #(.removeEventListener node event-type ! opts)))))))

#?(:cljs
   (defn listen-some "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)` unless
  the result is `nil`. Does not relieve backpressure. `opts` can be a clojure
  map."
     ([node event-type] (listen-some node event-type identity))
     ([node event-type f] (listen-some node event-type f {}))
     ([node event-type f opts]
      (m/observe (fn [!]
                   (let [! #(some-> (f %) !), opts (clj->js opts)]
                     (.addEventListener node event-type ! opts)
                     #(.removeEventListener node event-type ! opts))))
      ;; alternative implementation
      #_(m/eduction (filter some?) (listen node typ f opts)))))

(defn ^:no-doc ->box
  ([] (->box nil))
  ([init] (let [o (doto (object-array 1) (aset (int 0) init))]
            (fn ([] (aget o (int 0))) ([v] (aset o (int 0) v))))))

(defn ^:no-doc ->done [] (m/ap (let [dfv (m/dfv)] (m/amb #(dfv nil) (m/? dfv)))))

(defn ->latching-relay "Takes a discrete flow `>v` and returns a new discrete flow such that

  - for every produced value from `>v` produces a fresh `done!` thunk
  - calling the latest `done!` thunk produces `nil`

  Useful to latch onto a discrete event like a DOM event. User is in full
  control of signaling completion by calling the `done!` thunk." [>v]
  (m/ap
    (m/?< >v)
    (try (m/?> (->done)) (catch missionary.Cancelled _ (m/amb)))))

(defn ->d-latching-relay "Takes a discrete flow `>v` and returns a new discrete flow such that

  - for every produced value `v` from `>v`
    - if the relay's last produced value was a `[v done!]`, noop
    - else produces `[v done!]`
  - calling the latest `done!` thunk produces `[v nil]`

  Useful e.g. when modeling a payment button. User is in full control of
  signaling completion by calling the `done!` thunk." [>v]
  (m/ap
    (let [closed? (->box false)
          v (m/?> (m/eduction (remove (fn [_] (closed?))) >v))
          dfv (m/dfv)]
      [v (m/amb (closed? #(dfv (closed? false))) (m/? dfv))])))

(e/defn Listen "Takes the same arguments as `addEventListener`. Returns a tuple
of `[v done!]` for every DOM event where `v` is `(f event)`. Returns `[v nil]`
after calling the `done!` thunk. Initially returns `[init-v nil]` where `init-v`
defaults to `nil`.

```clj
(dom/input
  (let [[v done!] ($ Listen \"input\" #(-> % .-target .-value))]
    (when done!
      (case (e/server (tx! v)) (done!)))))
```"
  ([event-type] ($ Listen event-type identity))
  ([event-type f] ($ Listen dom/node event-type f))
  ([node event-type f] ($ Listen node event-type f {}))
  ([node event-type f opts] ($ Listen node event-type f opts nil))
  ([node event-type f opts init-v]
   (e/client
     (let [>v (listen node event-type f opts)
           >lr (->latching-relay >v)]
       [(e/input (m/reductions {} init-v >v))
        (e/input (m/reductions {} nil >lr))]))))

(e/defn Grab [event-type cb-fn] ; like Listen, but user must call done! before seeing next event
  (e/input (m/reductions {} [nil nil] (->d-latching-relay (listen node event-type cb-fn)))))

(defn fork [flow]
  (m/ap
    (let [S (i/spine)]
      (m/amb S
        (let [v (m/?> flow), id (random-uuid)]
          (S id {} [v #(S id {} nil)])
          (m/amb))))))

(e/defn Fork [event-type cb-fn]
  (e/join (e/input (fork (listen-some node event-type cb-fn)))))

;; DONE does LatchingRelay / DLatchingRelay matches ->task and ->backpressured-task?
;;   YES
;; WIP see if ->tasks can be implemented with LatchingRelay
;;   P: no, not the same pattern
;; TODO compare new `on` impl with `for-event-pending-switch` and `do-event-pending`
;; TODO come up with a good api for listen and ->default
;; TODO? come up with a nice api for LatchingRelay + listen
;; TODO? come up with a nice api for DLatchingRelay + listen
;; TODO polish DOM3 API
;; TODO test event handling in v2: port UI5 TodoMVC V2 to dom3 event api.
;;      copy to another ns, rename $ to new and e/input to new
