(ns leo.extend-seq3
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [clojure.set :as set]))


;We could assume that extend-seq works, continue with tracing it, and
;then come back to the UI. This is because the submission fiddle UI is adding too much
;complexity. Even if we're wrong with extend-seq's implementation (diffing) we will
;still learn a lot by continuing with it. Lets do this


(tests
  ; p-q-cross combined with alice-bob
  ; put a sequence into p or q

  (def ast '(let [>control (input)
                  >p       (input)
                  >q       (input)
                  >cross   (bind >control (fn [control]
                                            (extend-seq identity
                                              (fmap range
                                                (case control
                                                  :p >p
                                                  :q >q)))))


                  ]))



  (defn init [inputs _!replayers tracef]
    (let [>control (m/signal! (m/watch (get inputs '>control)))
          >p       (m/signal! (m/watch (get inputs '>p)))
          >q       (m/signal! (m/watch (get inputs '>q)))
          >cross   (m/signal! (m/relieve {} (m/ap (m/?! (case (m/?! >control)
                                                          :p >p
                                                          :q >q)))))
          >z       (m/signal! (m/latest vector >cross))
          >z       (extend-seq identity (m/ap (range (m/?! >cross))))
          >effects (m/stream! (m/relieve merge (m/ap
                                                 (amb=
                                                   {'>control (m/?? >control)}
                                                   {'>p (m/?? >p)}
                                                   {'>q (m/?? >q)}
                                                   {'>cross (m/?? >cross)}
                                                   {'>z (m/?? >z)}))))]
      (m/stream! (m/ap (tracef (m/?? >effects))))))

  (def !control (atom :p))
  (def !p (atom nil))
  (def !q (atom nil))

  (def r (reactor! {'>control !control
                    '>p       !p
                    '>q       !q}
           init))

  (def !trace (log! r))

  (directive! r '[>p 1])

  @!trace := '[{>p     5
                >cross >p
                >z     [0 1 2 3 4 5]}]

  (directive! r '[>q 4])
  (directive! r '[>control :q])

  @!trace := '[{>p     1
                >cross 1
                >z     [1]}
               {>q 4}
               {>control :q
                >cross   >q
                >z       [0 1 2 3 4]}]

  )