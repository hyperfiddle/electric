(ns dustin.trace25
  (:require [dustin.trace17 :as trace]
            [minitest :refer [tests]]
            [missionary.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIENT: Terminate retracted nodes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn from-trace! [node-path >trace] ;; ['>cross 2]
  (let [diff-path (pop node-path) ;;  ['>cross]
        node-id   (peek node-path)] ;; 2
    (->> >trace
         (m/transform (comp (take-while (fn [frame]
                                          ;; If we’ve got a diff in this frame
                                          (if-let [[_adds rets] (get frame diff-path)]
                                            ;; if this node is not to be retracted, continue.
                                            (not (contains? rets node-id))
                                            ;; if we don’t have a diff for this node, continue.
                                            true
                                            )))
                            ;; TODO What if a node is terminated and modified in the
                            ;; same frame?
                            (filter #(contains? % node-path))
                            (map #(get % node-path))))
         (m/relieve {})
         (m/signal!))))

(defn active-flows [factoryf]
  (fn [rf]
    (let [active (volatile! {})]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r [created retracted]]
         (rf r (vswap! active #(as-> % $
                                 (reduce dissoc $ retracted)
                                 (reduce (fn [r id]
                                           (assoc r id (factoryf id)))
                                         $ created)))))))))

(defn from-trace-factory! [id >trace]
  ;; Parallel with active-flows
  (->> >trace
       (m/transform (comp (filter #(contains? % [id]))
                          (map #(get % [id]))
                          (active-flows (fn [child-id]
                                          (from-trace! [id child-id] >trace)))))
       (m/relieve {})
       (m/signal!)))

(defn init-client [inputs !replayers tracef]
  (let [>replayer   (m/stream! (m/observe (fn [cb]
                                            (swap! !replayers conj cb)
                                            (fn []
                                              (swap! !replayers disj cb)))))
        >control    (from-trace! ['>control] >replayer)
        >p          (from-trace! ['>p] >replayer)
        >q          (from-trace! ['>q] >replayer)
        >cross      (from-trace-factory! '>cross >replayer) ;; Signal {:id Signal}
        >cross-diff (from-trace! '[>cross] >replayer)       #_ (m/stream! (diff-seq identity >cross))
        >effects    (m/stream! (m/relieve merge (m/ap
                                                 (trace/amb=
                                                  {['>control] (m/?? >control)}
                                                  {['>p] (m/?? >p)}
                                                  {['>q] (m/?? >q)}
                                                  {'[>cross] (m/?? >cross-diff)}
                                                  (let [[k >v] (m/?= (m/enumerate (m/?? >cross)))]
                                                    ;; termination notice for reference
                                                    (m/stream! (m/ap (m/? (m/aggregate (constantly nil) >v))
                                                                     (prn k "terminated")))
                                                    ;; Trace
                                                    {['>cross k] (m/?? >v)})
                                                  ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(tests

 (def !control (atom :p))
 (def !p (atom 0))
 (def !q (atom 0))

 (def r (trace/reactor! {} init-client))

 (def !trace (trace/log! r))

 (trace/replay! r '{[>p]       3
                    [>cross]   [#{0 1 2} #{}]
                    [>cross 0] 0
                    [>cross 1] 1
                    [>cross 2] 2})

 @!trace := '[{[>p]       3
               [>cross]   [#{0 1 2} #{}]
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}]


 (trace/replay! r '{[>p]       2
                    [>cross]   [#{} #{2}]
                    [>cross 0] 0 ;; FIXME don’t retrace this one
                    [>cross 1] 1 ;; FIXME don’t retrace this one
                    })
 @!trace := '[{[>p]       3
               [>cross]   [#{0 1 2} #{}]
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}
              {[>p]       2
               [>cross]   [#{} #{2}]
               [>cross 0] 0 ;; FIXME don’t retrace this one
               [>cross 1] 1 ;; FIXME don’t retrace this one
               }]

 ;; Force-set values for child nodes, [>cross 2] has been retracted so should be
 ;; gone.
 (trace/replay! r '{[>cross 0] :update
                    [>cross 1] :update
                    [>cross 2] :gone})

 @!trace := '[{[>p]       3
               [>cross]   [#{0 1 2} #{}]
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}
              {[>p]       2
               [>cross]   [#{} #{2}]
               [>cross 0] 0 ;; FIXME don’t retrace this one
               [>cross 1] 1 ;; FIXME don’t retrace this one
               }
              {[>cross 0] :update
               [>cross 1] :update
               ;; nothing for [>cross 2]
               }]
 )
