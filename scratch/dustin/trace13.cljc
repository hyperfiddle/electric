(ns dustin.trace13
  (:require [missionary.core :as m]
            [minitest :refer [tests]]))

;; Reference
(def ast '(let [>ui (input)
                >b  (vector ~>ui)
                >c  (pr-str ~>b)]))

;; ------------------------------

(defn put-in! [!atom k v]
  (swap! !atom assoc k v)
  v)

(defn prn-str [a]
  (prn a)
  (pr-str a))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

;; - Should effects (pulses) be grouped by frame?
;; - When replaying the trace, having the frame helps avoiding glitches.
;; - When registering a continuous node, which value should be the default? Are
;;   we dealing with continuous nodes only?
;; - Should a node always be initialized?
;; - Can a node start in a non-initialized state?

(defn init [!registry tracef]
  (let [>ui      (m/signal! (m/watch (put-in! !registry '>ui (atom nil))))
        >b       (m/signal! (m/latest vector >ui))
        >c       (m/signal! (m/latest prn-str >b))
        >effects (m/stream! (m/relieve merge (m/ap
                                              (amb=
                                               {'>ui (m/?? >ui)}
                                               {'>b (m/?? >b)}
                                               {'>c (m/?? >c)}))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))
    nil))

(defprotocol IDirective ;; TODO rename
  (directive! [this cause]))

(defprotocol Observable
  (subscribe! [this listenf])
  (unsubscribe! [this listenf]))

(defn log! [reactor]
  (let [!trace (atom [])]
    (subscribe! reactor #(swap! !trace conj %))
    !trace))

(deftype Reactor [cancel   ;; Stop the reactor
                  !registry ;; Atom<Map<Symbol, Atom>>
                  !state
                  !callbacks
                  ]
  IDirective
  (directive! [this [nom value]]
    (if-let [node (get @!registry nom)]
      (reset! node value)
      (throw (ex-info "Unknown node" {:name nom}))))
  clojure.lang.IDeref
  (deref [this]
    (deref !state))
  Observable
  (subscribe! [_ f] (swap! !callbacks conj f))
  (unsubscribe! [_ f] (swap! !callbacks disj f)))

(defn reactor! []
  (let [!state     (atom {})
        !registry  (atom {})
        !callbacks (atom #{})
        task       (m/reactor (init !registry
                                    (fn [effects]
                                      (swap! !state merge effects)
                                      (doseq [cb @!callbacks]
                                        (cb effects)))))
        cancel     (task (fn [_] (prn "Success"))
                         (fn [err] (prn err)))]
    (->Reactor cancel !registry !state !callbacks)))

(tests
  (def r (reactor!))
  (def !trace (log! r))

  (directive! r '[>ui 1])
  (directive! r '[>ui 2])
  (directive! r '[>ui 3])

  @!trace  := '[{>ui 1, >b [1], >c "[1]"}
                {>ui 2, >b [2], >c "[2]"}
                {>ui 3, >b [3], >c "[3]"}]

  )

;; TODO
;; - directive! [x]
;; - trace      [-]
;; - replay!
