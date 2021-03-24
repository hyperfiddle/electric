(ns dustin.trace26
  (:require [dustin.trace17 :as trace]
            [dustin.trace22 :refer [diff-seq active-flows]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SERVER: Terminate retracted nodes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn focus-entity
  "Focus a cont flow of successive query results into a flow of successive elements with targetted identity."
  [kf id flow]
  (->> flow
       (m/transform
        (comp
         (map (fn [xs]
                (reduce (fn [r x]
                          (if (= id (kf x))
                            (reduced x) r))
                        ::not-found xs)))
         (take-while (complement #{::not-found}))
         (dedupe)))
       (m/relieve {})))

(defn init-server [inputs _!replayers tracef]
  (let [>control        (m/signal! (m/watch (get inputs '>control)))
        >p              (m/signal! (m/watch (get inputs '>p)))
        >q              (m/signal! (m/watch (get inputs '>q)))
        ;; It’s up to the compiler to explode `reactive-for` into these 4 nodes
        >cross-in       (m/signal! (m/relieve {} (m/ap (range (m/?! (case (m/?! >control)
                                                                      :p >p
                                                                      :q >q))))))
        >cross-diff     (m/stream! (diff-seq identity >cross-in))
        >cross-registry (m/signal! (m/transform (active-flows (fn [child-id]
                                                                (focus-entity identity child-id >cross-in)))
                                                >cross-diff))
        >cross-out      (m/signal! (m/latest (fn [k>a] (into {}
                                                            (for [[k >v] k>a]
                                                              [k (m/latest str >v)])))
                                             >cross-registry))
        ;; --------------------------------------------------------------------
        >effects        (m/stream! (m/relieve merge (m/ap
                                                     (trace/amb=
                                                      {['>control] (m/?? >control)}
                                                      {['>p] (m/?? >p)}
                                                      {['>q] (m/?? >q)}
                                                      {'[>cross] (m/?? >cross-diff)}
                                                      ;; TODO trace according to diff
                                                      (let [[k >v] (m/?= (m/enumerate (m/?? >cross-registry)))]
                                                        ;; termination notice for reference
                                                        (trace/amb=
                                                         {['>cross k] (m/?? >v)}
                                                         ;; For test purposes only, to see that the right thing happened
                                                         {[::terminated '>cross k] (m/? (m/aggregate (constantly nil) >v))}))
                                                      ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(tests

 (def !control (atom :p))
 (def !p (atom 0))
 (def !q (atom 0))

 (def r (trace/reactor! {'>control !control
                         '>p       !p
                         '>q       !q} init-server))

 (def !trace (trace/log! r))

 (trace/directive! r '[>p 3])

 @!trace := '[{[>p]       3
               [>cross]   [#{0 1 2} #{}]
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}]


 (trace/directive! r '[>p 2])

 @!trace := '[{[>p]       3
               [>cross]   [#{0 1 2} #{}]
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}
              {[>p]                    2
               [>cross]                [#{} #{2}]
               [>cross 0]              0   ;; FIXME don’t retrace this one
               [>cross 1]              1   ;; FIXME don’t retrace this one
               [::terminated >cross 2] nil ;; For test purposes only
               }]
 )
