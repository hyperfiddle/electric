(ns dustin.trace17
  (:require [missionary.core :as m]
            [minitest :refer [tests]]))

(def ast '(let [>control (input)
                >p       (input)
                >q       (input)
                >cross   (bind >control (fn [control]
                                          (case control
                                            :p >p
                                            :q >q)))
                >z       (fmap vector >cross)]))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

(defn init [inputs _!replayers tracef]
  (let [>control (m/signal! (m/watch (get inputs '>control)))
        >p       (m/signal! (m/watch (get inputs '>p)))
        >q       (m/signal! (m/watch (get inputs '>q)))
        >cross   (m/signal! (m/relieve {} (m/ap (m/?! (case (m/?! >control)
                                                        :p >p
                                                        :q >q)))))
        >z       (m/signal! (m/latest vector >cross))
        >effects (m/stream! (m/relieve merge (m/ap
                                              (amb=
                                               {'>control (m/?? >control)}
                                               {'>p (m/?? >p)}
                                               {'>q (m/?? >q)}
                                               {'>cross (m/?? >cross)}
                                               {'>z (m/?? >z)}))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))
    nil))


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
 (def !control (atom :p))
 (def !p (atom nil))
 (def !q (atom nil))

 (def r (reactor! {'>control !control
                   '>p       !p
                   '>q       !q}
                  init))

 (def !trace (log! r))

 (directive! r '[>p 1])

 @!trace := '[{>p     1
               >cross 1                                     ; changed in trace18
               >z     [1]}]

 (directive! r '[>q 2])
 (directive! r '[>control :q])

 @!trace := '[{>p     1
               >cross 1
               >z     [1]}
              {>q 2}
              {>control :q
               >cross   2
               >z       [2]}]

 )
