(ns dustin.trace15
  (:require [missionary.core :as m]
            [minitest :refer [tests]]))

;; - TODO [2/9]
;;   - [X] Does tracing and replaying introduces overhead because it introduces extra nodes? -> No.
;;   - [X] Answer questions
;;     - About Trace13 [2/2]
;;       - [X] Should effects (pulses) be grouped by frame? When replaying the
;;         trace, having the frame helps avoiding glitches. -> Yes
;;       - [X] When registering a continuous node, which value should be the
;;         default? Are we dealing with continuous nodes only? Should a node
;;         always be initialized? Can a node start in a non-initialized state?
;;         -> No default value because it should already be ready, if it’s not
;;         ready, nothing happens until it is.
;;     - About Trace 14 [3/3]
;;       - [X] When putting holes in the dag, should we preserve the same topology?
;;         Should >b listen to =!replay-entrypoint= AND >input1 or only
;;         =!replay-entrypoint=? -> This is a future decision.
;;       - [X] Replay entrypoint can’t be an atom because if >b is missing from
;;         a frame, >b will be nil. -> Atom implies continuous, so
;;         !replayers needs to be an event stream. We need to `find` '>b
;;         in the map and fire only if it’s present.
;;       - [X] Node >b should not react when '>b is not part of the replayed frame.
;;         =!replay-entrypoint= should be discreet, not continuous. -> We all agree.
;;   - [X] Is there a way for a more complex Applicative dag to invalidate our
;;     model?
;;   - [X] Make =!replay-entrypoint= discrete (15)


(def ast '(let [>input1  (input)
                >input2  (input)
                >a       (inc ~>input1)
                >b       (inc ~>input2)
                >passive (f ~>b)
                >c       (vector ~>a ~>b ~>passive)]))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

(defn init [inputs !replayers tracef]
  (let [>replayer (m/stream! (m/observe (fn [cb]
                                          (swap! !replayers conj cb)
                                          (fn []
                                            (swap! !replayers disj cb)))))
        >input1   (m/signal! (m/watch (get inputs '>input1)))
        >input2   (m/signal! (m/watch (get inputs '>input2)))
        >a        (m/signal! (m/latest inc >input1))
        >b        (m/signal! (m/latest inc >input2))
        >passive  (->> >replayer
                       (m/transform (comp (filter #(contains? % '>passive))
                                          (map #(get % '>passive))))
                       (m/relieve {})
                       (m/signal!))
        >c        (m/signal! (m/latest vector >a >b >passive))
        >effects  (m/stream! (m/relieve merge (m/ap
                                               (amb=
                                                {'>input1 (m/?? >input1)}
                                                {'>input2 (m/?? >input2)}
                                                {'>a (m/?? >a)}
                                                {'>b (m/?? >b)}
                                                {'>passive (m/?? >passive)}
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


(tests

  (def input1 (atom 0))
  (def input2 (atom 0))
  (def r (reactor! {'>input1 input1
                    '>input2 input2}
                   init))

  (def !trace (log! r))

  (directive! r '[>input1 1])
  (directive! r '[>input2 2])

  @!trace := '[{>input1 1
                >a      2}
               {>input2 2
                >b      3}]

  (replay! r '{>passive :foo})

  @!trace := '[{>input1 1
                >a      2}
               {>input2 2
                >b      3}
               {>passive :foo
                >c       [2 3 :foo]}]

  ;; A directive could produce multiple frames. At some point we have transform,
  ;; the transducer expands its inputs. A node might emit several values, but
  ;; only one values is allowed per frame, so many frames will run.

  ;; Checking the last frame is correct in this example, but not in the general
  ;; case, because some nodes might emits multiple values, producing multiple
  ;; frames.
)
