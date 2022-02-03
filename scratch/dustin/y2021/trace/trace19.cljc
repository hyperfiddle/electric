(ns dustin.trace19
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [dustin.trace17 :as trace]
            [leo.extend-seq :refer [extend-seq]]))


;We could assume that extend-seq works, continue with tracing it, and
;then come back to the UI. This is because the submission fiddle UI is adding too much
;complexity. Even if we're wrong with extend-seq's implementation (diffing) we will
;still learn a lot by continuing with it. Lets do this

(defn init [inputs _!replayers tracef]
  (let [>control (m/signal! (m/watch (get inputs '>control)))
        >p       (m/signal! (m/watch (get inputs '>p)))
        >q       (m/signal! (m/watch (get inputs '>q)))
        >cross   (m/signal! (m/relieve {} (m/ap (m/?! (case (m/?! >control)
                                                        :p >p
                                                        :q >q)))))
        >z       (m/signal! (m/latest range >cross))
        >>z      (m/signal! (extend-seq str >z))
        >effects (m/stream! (m/relieve merge (m/ap
                                              (trace/amb=
                                               {'>control (m/?? >control)}
                                               {'>p (m/?? >p)}
                                               {'>q (m/?? >q)}
                                               {'>cross (m/?? >cross)}
                                               {'>z (m/?? >z)}
                                               {'>>z (set (keys (m/?? >>z)))}
                                               (let [[k v] (m/?= (m/enumerate (m/?? >>z)))]
                                                 {['>>z k] (m/?? v)})
                                               ))))]
      (m/stream! (m/ap (tracef (m/?? >effects))))))

(tests

  (def !control (atom :p))
  (def !p (atom 0))
  (def !q (atom 0))

  (def r (trace/reactor! {'>control !control
                          '>p       !p
                          '>q       !q}
           init))

  (def !trace (trace/log! r))

  (trace/directive! r '[>p 3])

  @!trace := [{'>p        3
               '>cross    3
               '>z        '(0 1 2)
               ['>>z "0"] 0
               ['>>z "1"] 1
               ['>>z "2"] 2
               '>>z       #{"0" "1" "2"}}])


;; We could work on:
;; - [X] ids (path, uuids, whatever)
;; - [ ] nested extend-seq (this example is top-level)
;; - [ ] replay dynamic trace

