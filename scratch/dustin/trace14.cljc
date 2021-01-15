(ns dustin.trace14
  (:require [missionary.core :as m]
            [minitest :refer [tests]]))

;; Reference
(def ast '(let [>ui (input)
                >b  (vector ~>ui)
                >c  (pr-str ~>b)]))

;; ------------------------------

(defn prn-str [a]
  (prn a)
  (pr-str a))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))


;; When putting holes in the dag, should we preserve the same topology?
;; Should >b listen to !replay-entrypoint AND >ui or only !replay-entrypoint?

;; Replay entrypoint can’t be an atom because if >b is missing from a frame, >b
;; will be nil.
;; Node >b should not react when '>b is not part of the replayed frame.
;; !replay-entrypoint should be discreet, not continuous.

(defn init-client [!registry !replay-entrypoint tracef]
  (let [>replayer (m/signal! (m/watch !replay-entrypoint))
        >ui       (m/signal! (m/watch (get @!registry '>ui)))
        >b        (m/signal! (m/latest (fn [frame _ui]
                                         (get frame '>b))
                                       >replayer
                                       >ui))
        >c        (m/signal! (m/latest prn-str >b))
        >effects  (m/stream! (m/relieve merge (m/ap
                                               (amb=
                                                {'>ui (m/?? >ui)}
                                                {'>b (m/?? >b)}
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
                  !registry ;; Atom<Map<Symbol, Atom>>
                  !state
                  !callbacks
                  !replay-entrypoint
                  ]
  IDirective
  (directive! [this [nom value]]
    (if-let [node (get @!registry nom)]
      (reset! node value)
      (throw (ex-info "Unknown node" {:name nom}))))
  IReplay
  (replay! [this frame-effects]
    (reset! !replay-entrypoint frame-effects))
  clojure.lang.IDeref
  (deref [this]
    (deref !state))
  Observable
  (subscribe! [_ f] (swap! !callbacks conj f))
  (unsubscribe! [_ f] (swap! !callbacks disj f)))

(defn reactor! [inputs initf]
  (let [!state             (atom {})
        !registry          (atom inputs)
        !callbacks         (atom #{})
        !replay-entrypoint (atom {})
        task               (m/reactor
                            (initf !registry !replay-entrypoint
                                   (fn [effects]
                                     (swap! !state merge effects)
                                     (doseq [cb @!callbacks]
                                       (cb effects)))))
        cancel             (task (fn [_] (prn "Success"))
                                 prn)]
    (->Reactor cancel !registry !state !callbacks !replay-entrypoint)))

(tests
  (def r (reactor! {'>ui (atom nil)} init-client))
  (def !trace (log! r))

  (replay! r '{>b [1]})
  (replay! r '{>b [2]})

  @!trace := '[{>b [1], >c "[1]"}
               {>b [2], >c "[2]"}]
  )

;; DONE
;; - directive! [x]
;; - trace      [x]
;; - replay!    [x]
