(ns hyperfiddle.electric-dom3-events
  (:require [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  (:import [missionary Cancelled])
  #?(:cljs (:require-macros [hyperfiddle.electric-dom3-events])))

#?(:cljs
   (defn listen "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)`.
  Relieves backpressure. `opts` can be a clojure map."
     ([node event-type] (listen node event-type identity))
     ([node event-type f] (listen node event-type f {}))
     ([node event-type f opts]
      (->> (m/observe (fn [!]
                        (let [! #(! (f %)), opts (clj->js opts)]
                          (.addEventListener node event-type ! opts)
                          #(.removeEventListener node event-type ! opts))))
        (m/relieve {})))))

#?(:cljs
   (defn listen-some "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)` unless
  the result is `nil`. Relieves backpressure. `opts` can be a clojure map."
     ([node event-type] (listen-some node event-type identity))
     ([node event-type f] (listen-some node event-type f {}))
     ([node event-type f opts]
      (->> (m/observe (fn [!]
                        (let [! #(some-> (f %) !), opts (clj->js opts)]
                          (.addEventListener node event-type ! opts)
                          #(.removeEventListener node event-type ! opts))))
        (m/relieve {}))
      ;; alternative implementation
      #_(m/eduction (filter some?) (listen node typ f opts)))))

(defn ^:no-doc ->box
  ([] (->box nil))
  ([init] (let [o (doto (object-array 1) (aset (int 0) init))]
            (fn ([] (aget o (int 0))) ([v] (aset o (int 0) v))))))

(defn ^:no-doc ->done [] (m/ap (let [dfv (m/dfv)] (m/amb #(dfv nil) (m/? dfv)))))

(defn ->latching-relay "Takes a discrete flow `>v` and returns a new discrete flow such that

  - for every produced value from `>v` produces a fresh `release!` thunk
  - calling the latest `release!` thunk produces `nil`

  Useful to latch onto a discrete event like a DOM event. User is in full
  control of signaling completion by calling the `release!` thunk." [>v]
  (m/ap
    (m/?< >v)
    (try (m/?> (->done)) (catch missionary.Cancelled _ (m/amb)))))

(defn ->d-latching-relay "Takes a discrete flow `>v` and returns a new discrete flow such that

  - for every produced value `v` from `>v`
    - if the relay's last produced value was a `[v release!]`, noop
    - else produces `[v release!]`
  - calling the latest `release!` thunk produces `[v nil]`

  Useful e.g. when modeling a payment button. User is in full control of
  signaling completion by calling the `release!` thunk." [>v]
  (m/ap
    (let [closed? (->box false)
          v (m/?> (m/eduction (remove (fn [_] (closed?))) >v))
          dfv (m/dfv)]
      [v (m/amb (closed? #(dfv (closed? false))) (m/? dfv))])))

(e/defn EventListener "Takes the same arguments as `addEventListener`. Returns
the result of `(f event)`.

```clj
(dom/input
  (when-some [v ($ EventListener \"input\" #(-> % .-target .-value))]
    (prn v)))
```"
  ([event-type] ($ EventListener event-type identity))
  ([event-type f] ($ EventListener dom/node event-type f))
  ([node event-type f] ($ EventListener node event-type f {}))
  ([node event-type f opts] ($ EventListener node event-type f opts nil))
  ([node event-type f opts init-v]
   (e/client (e/input (m/reductions {} init-v (listen node event-type f opts))))))

(e/defn Listen "Takes the same arguments as `addEventListener`. Returns a tuple
of `[v release!]` for every DOM event where `v` is `(f event)`. Returns `[v
nil]` after calling the `release!` thunk. Initially returns `[init-v nil]` where
`init-v` defaults to `nil`.

```clj
(dom/input
  (let [[v release!] ($ Listen \"input\" #(-> % .-target .-value))]
    (when release!
      (case (e/server (tx! v)) (release!)))))
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

;; v2 `e/do-event` is an interpreter on top of `Hold` 

;; NOTE P and G thinks Hold does two things: latch and addEventListener. Maybe
;; we could unbundle it? What would a DLatchingRelay e/defn look like? would it
;; be discrete or continuous?
(e/defn Hold "Takes the same arguments as `addEventListener`. Holds a DOM event
and returns `[v release!]` where `v` is `(f event)`. Returns `[v nil]` after
calling the `release!` thunk. Drops events if one is currently held. Initially
returns `[init-v nil]` where `init-v` defaults to `nil`.

```clj
(dom/button
  (let [[v release!] ($ Hold \"click\" hash)
        busy? (boolean release!)]
    (dom/props {:disabled busy?, :aria-busy busy?})
    (when release!
      (case (e/server (tx! v)) (release!)))))
```"
  ([event-type] ($ Hold event-type identity))
  ([event-type f] ($ Hold dom/node event-type f))
  ([node event-type f] ($ Hold node event-type f {}))
  ([node event-type f opts] ($ Hold node event-type f opts nil))
  ([node event-type f opts init-v]
   (e/client
     (e/input (m/reductions {} [init-v nil] (->d-latching-relay (listen node event-type f opts)))))))

#_(defn fork [flow]
  (m/ap
    (let [S (i/spine)]
      (m/amb S
        (let [v (m/?> flow), id (random-uuid)]
          (S id {} [v #(S id {} nil)])
          (m/amb))))))

;; Leo: I would have split fork in two:
;;      - fork would have parallelism infinity only
;;      - I would have made an IncSeq->IncSeq function (e.g. i/take) that truncates the Spine up to concurrency-factor
;;        (i.e. equivalent of take but for incseqs)
;;      - G: note ops on incseqs can be infralinear, so transducers could work but wouldn't be ideal.
(defn fork
  ([flow] (fork ##Inf flow))
  ([n flow]
   (m/ap
     (let [!id (atom 0), S (i/spine), !running (atom (sorted-set))]
       (m/amb S
         (let [v (m/?> flow), id @!id, running (swap! !running conj (swap! !id inc))]
           (S id {} [v #(do (swap! !running disj id) (S id {} nil))])
           (run! #(S % {} nil) (take (- (count running) n) ; NOTE Leo: always return 0 or 1 because we add one event at a time
                                 running))
           (m/amb)))))))

;; Let's suppose we have i/take

(comment
  (defn fork* [flow]
    (m/ap
      (let [next-id! (partial swap! (atom 0) inc), S (i/spine)]
        (m/amb S
          (let [v (m/?> flow), id (next-id!)]
            (S id {} [v #(S id {} nil)])
            (m/amb))))))

  (defn fork [n flow] (i/take n (fork* flow)))
  )


(e/defn Fork "Takes the same arguments as `addEventListener`. For each DOM event
where `(f event)` isn't `nil` forks a new branch, allowing concurrent processing
of events. Each branch recieves `[v release!]` where `v` is `(f event)` and
calling the `release!` thunk unmounts the current branch. Optional
`concurrency-factor` limits the maximum number of active branches, defaults to
`##Inf`.

```clj
(dom/button
  (e/cursor [[v release!] ($ Fork \"click\" hash)]
    (case (e/server (add-todo! v)) (release!))))
```"
  ([event-type] ($ Fork event-type identity))
  ([event-type f] ($ Fork dom/node event-type f))
  ([node event-type f] ($ Fork node event-type f {}))
  ([node event-type f opts] ($ Fork node event-type f opts ##Inf))
  ([node event-type f opts concurrency-factor]
   (e/client (e/join (e/input (fork concurrency-factor (listen-some node event-type f opts)))))))

;; DONE does LatchingRelay / DLatchingRelay matches ->task and ->backpressured-task?
;;   YES
;; DONE see if ->tasks can be implemented with LatchingRelay
;;   no, not the same pattern, but same interface
;; DONE compare new `on` impl with `for-event-pending-switch` and `do-event-pending`
;; DONE come up with a good api for listen and ->default
;; DONE come up with a nice api for LatchingRelay + listen
;; DONE come up with a nice api for DLatchingRelay + listen

;; interpreter on top of `Fork` with concurrency of `1`, interpreting first
;; non-`Pending` as the release signal. Backwards compatible with `dom2/on`.
(e/defn On
  ([event-type F] ($ On dom/node event-type F))
  ([node event-type F]
   (let [!ret (atom [::init]), [state v] (e/watch !ret)]
     (e/cursor [[e release!] ($ Fork node event-type identity {} 1)]
       (try (case (reset! !ret [::ok ($ F e)]) (release!))
            (catch Pending e (reset! !ret [::pending e]))
            (catch Cancelled _ (release!))
            (catch #?(:clj Throwable :cljs :default) e (reset! !ret [::failed e]) (release!))))
     (case state (::init ::ok) v, (::pending ::failed) (throw v)))))

;; full `dom2/on` backward compatibility
(defmacro on
  ([typ F] `($ On ~typ ~F))
  ([node typ F] `($ On ~node ~typ ~F)))
