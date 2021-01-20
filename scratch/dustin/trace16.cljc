(ns dustin.trace16
  (:require [missionary.core :as m]
            [minitest :refer [tests]]))

;; TODO [2/2]
;; - [X] Pipe client to server      (16)
;; - [X] Pipe server back to client (16)

(def ast '(let [>input1  (input)
                >input2  (input)
                >a       (inc ~>input1)
                >b       (inc ~>input2)
                >special (f ~>b)
                >c       (vector ~>a ~>b ~>special)]))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

(defn from-trace! [id >trace]
  (->> >trace
       (m/transform (comp (filter #(contains? % id))
                          (map #(get % id))))
       (m/relieve {})
       (m/signal!)))

(defn init-client [inputs !replayers tracef]
  (let [>replayer (m/stream! (m/observe (fn [cb]
                                          (swap! !replayers conj cb)
                                          (fn []
                                            (swap! !replayers disj cb)))))
        >input1   (m/signal! (m/watch (get inputs '>input1)))
        >input2   (m/signal! (m/watch (get inputs '>input2)))
        >a        (from-trace! '>a >replayer)
        >b        (from-trace! '>b >replayer)
        >special  (from-trace! '>special >replayer)
        >c        (m/signal! (m/latest vector >a >b >special))
        >effects  (m/stream! (m/relieve merge (m/ap
                                               (amb=
                                                {'>input1 (m/?? >input1)}
                                                {'>input2 (m/?? >input2)}
                                                {'>a (m/?? >a)}
                                                {'>b (m/?? >b)}
                                                {'>special (m/?? >special)}
                                                ;; the compiler should figure
                                                ;; out >c should not be traced.
                                                {'>c (m/?? >c)}
                                                ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(defn init-server [_inputs !replayers tracef]
  (let [>replayer (m/stream! (m/observe (fn [cb]
                                          (swap! !replayers conj cb)
                                          (fn []
                                            (swap! !replayers disj cb)))))
        >input1   (from-trace! '>input1 >replayer)
        >input2   (from-trace! '>input2 >replayer)
        >a        (m/signal! (m/latest inc >input1))
        >b        (m/signal! (m/latest inc >input2))
        >special  (m/signal! (m/latest inc >b))
        >c        (m/signal! (m/latest vector >a >b >special))
        >effects  (m/stream! (m/relieve merge (m/ap
                                               (amb=
                                                {'>input1 (m/?? >input1)}
                                                {'>input2 (m/?? >input2)}
                                                {'>a (m/?? >a)}
                                                {'>b (m/?? >b)}
                                                {'>special (m/?? >special)}
                                                ;; the compiler should figure
                                                ;; out >c should not be traced.
                                                {'>c (m/?? >c)}))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(defprotocol IDirective ;; TODO rename
  (directive! [this cause]))

(defprotocol Observable
  (subscribe! [this listenf])
  (unsubscribe! [this listenf]))

(defprotocol IReplay
  (replay! [this effect]))

(defn log! [reactor]
  (let [!trace (atom [])]
    (subscribe! reactor #(swap! !trace conj %))
    !trace))

(deftype Reactor [cancel   ;; Stop the reactor
                  inputs ;; Atom<Map<Symbol, Atom>>
                  !callbacks
                  !replayers
                  ]
  IDirective
  (directive! [this [nom value]]
    (if-let [node (get @inputs nom)]
      (reset! node value)
      (throw (ex-info "Unknown node" {:name nom}))))
  IReplay
  (replay! [this frame-effects]
    (doseq [cb @!replayers]
      (cb frame-effects)))
  Observable
  (subscribe! [_ f] (swap! !callbacks conj f))
  (unsubscribe! [_ f] (swap! !callbacks disj f)))

(defn reactor! [inputs initf]
  (let [!inputs    (atom inputs)
        !callbacks (atom #{})
        !replayers (atom #{})
        task       (m/reactor
                    (initf inputs !replayers
                           (fn [effects]
                             (doseq [cb @!callbacks]
                               (cb effects)))))
        cancel     (task (fn [_] (prn "Success"))
                         prn)]
    (->Reactor cancel !inputs !callbacks !replayers)))


(defn state [trace] (reduce merge {} trace))

(tests
 (def input1 (atom 0)) ; initial values are missed because it propagates before
                       ; we subscribe. This is ok as we don't care about these
                       ; values.
 (def input2 (atom 0))

 (def r-client (reactor! {'>input1 input1
                          '>input2 input2}
                         init-client))

 (def r-server (reactor! {} init-server))

 (def !client-trace (log! r-client))
 (def !server-trace (log! r-server))

 (subscribe! r-client (fn [frame]
                        (replay! r-server frame)))

 (subscribe! r-server (fn [frame]
                        (replay! r-client frame)))

 (directive! r-client '[>input1 1])
 @!server-trace := '[{>input1 1
                      >a      2}]

 (directive! r-client '[>input2 2])
 @!server-trace := '[{>input1 1
                      >a      2}
                     {>input2  2
                      >b       3
                      >special 4
                      >c       [2 3 4]}]

 (state @!server-trace) := (state @!client-trace)

 ;; Client computes >c on it’s own, but Server did trace it and send it anyway.
 ;; This is wasteful and `init-server` should not get a `{'>c (m/?? >c)}` in
 ;; `amb=` for `>c`. How does the compiler knows that `>c` is computed on both
 ;; sides and therefore should not be traced?
 )
