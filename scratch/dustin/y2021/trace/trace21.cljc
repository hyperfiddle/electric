(ns dustin.trace21
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [dustin.trace17 :as trace]
            [leo.extend-seq :refer [diffp diff-by]]))

;;;;;;;;;;;;
;; CLIENT ;;
;;;;;;;;;;;;

(def ast '(let [>control (input)
                >p       (input)
                >q       (input)
                >cross   (bind >control (fn [control]
                                          (reactive-for identity
                                                        (fmap range (case control
                                                                      :p >p
                                                                      :q >q))
                                                        identity)))]))

(defn from-trace! [id >trace]
  (->> >trace
       (m/transform (comp (filter #(contains? % id))
                          (map #(get % id))))
       (m/relieve {})
       (m/signal!)))

(defn diff-seq [kf flow]
  (m/transform (diffp (partial diff-by kf) #{}) flow))

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
        >control    (from-trace! '>control >replayer)
        >p          (from-trace! '>p >replayer)
        >q          (from-trace! '>q >replayer)
        >cross      (from-trace-factory! '>cross >replayer) ;; Signal {:id Signal}
        >cross-diff (from-trace! '[>cross] >replayer)       #_ (m/stream! (diff-seq identity >cross))
        >effects    (m/stream! (m/relieve merge (m/ap
                                               (trace/amb=
                                                {'>control (m/?? >control)}
                                                {'>p (m/?? >p)}
                                                {'>q (m/?? >q)}
                                                {'[>cross] (m/?? >cross-diff)}
                                                (let [[k v] (m/?= (m/enumerate (m/?? >cross)))]
                                                  {['>cross k] (m/?? v)})
                                                ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

;; (trace! (replay reactor t)) = t

(tests

 (def r (trace/reactor! {} init-client))

 (def !trace (trace/log! r))

 (trace/replay! r '{>p         3
                    [>cross]   [#{0 1 2} #{}] ;; this is a diff
                    [>cross 0] 0
                    [>cross 1] 1
                    [>cross 2] 2})

 @!trace := '[{>p         3
               [>cross]   [#{0 1 2} #{}] ;; this is a diff
               [>cross 0] 0
               [>cross 1] 1
               [>cross 2] 2}])

;; We did learn:
;;
;; - In order to trace `reactive-for`, we need to split up:
;;   - its diff, because we *do* get it from the trace;
;;   - its registry (eg. `{0 _, 1 _}`) because we *don’t* get it from the trace.
;;
;;
;; - Steps to trace and replay a `reactive-for` node:
;;   1. Don’t trace its value under its own name, so this line `{>node []}` do
;;      not appear in the trace. *In a trace*, reactive-for nodes don’t have
;;      values.
;;   2. Trace its diff under `{[>node] [#{adds} #{rets}]}`
;;
;;      It implies we need to add a `>node-diff` technical node to trace the
;;      diff for this node.
;;
;;   3. Trace its children under `{[>node id] value}`
;;   4. hydrate it using `(from-trace-factory! '>node >trace)`
;;
;;      `from-trace-factory!` re-uses the same logic as `reactive-for` and
;;      reconstruct the registry from the diff.
;;

