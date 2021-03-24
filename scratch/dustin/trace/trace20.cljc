(ns dustin.trace20
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [dustin.trace17 :as trace]
            [leo.extend-seq :refer [extend-seq]]))


;We could assume that extend-seq works, continue with tracing it, and
;then come back to the UI. This is because the submission fiddle UI is adding too much
;complexity. Even if we're wrong with extend-seq's implementation (diffing) we will
;still learn a lot by continuing with it. Lets do this

(def fmap m/latest)

(defn reactive-for [kf >as f]
  ;; realigning elements could happen here
  (fmap (fn [k>a]
          (into {}
                (for [[k >v] k>a]
                  [k (fmap f >v)])))
        (extend-seq kf >as)))

(def ast '(let [>control (input)
                >p       (input)
                >q       (input)
                >cross   (bind >control (fn [control]
                                          (reactive-for identity
                                                        (fmap range (case control
                                                                      :p >p
                                                                      :q >q))
                                                        str)))]))

;; (def ast '(let [>control (input)
;;                 >p       (input)
;;                 >q       (input)
;;                 >cross   (bind >control (fn [control]
;;                                           (fmap (fn [k>a]
;;                                                   (for [[k >v] k>a]
;;                                                     (fmap inc >v)))
;;                                                 (extend-seq identity (fmap range (case control
;;                                                                                    :p >p
;;                                                                                    :q >q))))))]))

(defn init [inputs _!replayers tracef]
  (let [>control (m/signal! (m/watch (get inputs '>control)))
        >p       (m/signal! (m/watch (get inputs '>p)))
        >q       (m/signal! (m/watch (get inputs '>q)))
        >cross   (m/signal! (m/relieve {} (m/ap (m/?!
                                                 (reactive-for identity
                                                               (m/latest range
                                                                         (case (m/?! >control)
                                                                           :p >p
                                                                           :q >q))
                                                               str)))))
        >effects (m/stream! (m/relieve merge (m/ap
                                              (trace/amb=
                                               {'>control (m/?? >control)}
                                               {'>p (m/?? >p)}
                                               {'>q (m/?? >q)}
                                               {'>cross (set (keys (m/?? >cross)))}
                                               (let [[k v] (m/?= (m/enumerate (m/?? >cross)))]
                                                 {['>cross k] (m/?? v)})
                                               ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(tests
  ; p-q-cross combined with alice-bob
  ; put a sequence into p or q

  (def !control (atom :p))
  (def !p (atom 0))
  (def !q (atom 0))

  (def r (trace/reactor! {'>control !control
                          '>p       !p
                          '>q       !q}
           init))

  (def !trace (trace/log! r))

  (trace/directive! r '[>p 3])

  @!trace := [{'>p         3
               '>cross     #{0 1 2}
               ['>cross 0] "0"
               ['>cross 1] "1"
               ['>cross 2] "2"}])


;; #+TODO: TODO(t!) POSTPONED(p@/!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;; We could work on:
;;
;; * DONE ids (path, uuids, whatever)
;;   CLOSED: [2021-01-20 Wed 10:40]
;; * POSTPONED nested extend-seq (this example is top-level)
;;   CLOSED: [2021-01-20 Wed 10:42]
;;
;;   If we want to identify nodes by their AST path, extend-seq calls need to be
;;   part of the AST. Is it an acceptable tradeoff?
;;
;; * TODO replay dynamic trace
;;   - State "TODO"       from              [2021-01-20 Wed 10:42]
;;
;;
;;   Loop -> interpret
