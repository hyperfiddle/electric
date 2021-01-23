(ns dustin.trace24
  (:require [missionary.core :as m]
            [leo.extend-seq :refer [diffp diff-by extend-seq]]
            [dustin.trace17 :as trace]
            [minitest :refer [tests]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don’t retrace children again for every diff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn diff-seq
  "Takes a flow of seqs and returns a flow of diffs (by `kf`)"
  [kf flow]
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

(def fmap m/latest)

(defn reactive-for [kf >as f]
  ;; realigning elements could happen here
  (fmap (fn [k>a]
          (into {}
            (for [[k >v] k>a]
              [k (fmap f >v)])))
    (extend-seq kf >as)))

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
        (remove #{::not-found})
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
                                                              [k (fmap str >v)])))
                                             >cross-registry))
        ;; --------------------------------------------------------------------
        >effects        (m/stream! (m/relieve merge (m/ap
                                                     (trace/amb=
                                                      {'>control (m/?? >control)}
                                                      {'>p (m/?? >p)}
                                                      {'>q (m/?? >q)}
                                                      {'[>cross] (m/?? >cross-diff)}
                                                      ;; TODO latest should be sample, but it crashes. Investigate
                                                      (let [[k >v] (m/?= (m/enumerate (m/?? (m/sample (fn [r [ids _]]
                                                                                                        (select-keys r ids))
                                                                                                      >cross-registry >cross-diff))))]
                                                        {['>cross k] (m/?? >v)})
                                                      ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

;; (trace! (replay reactor t)) = t

(tests

  (def !control (atom :p))
  (def !p (atom 0))
  (def !q (atom 0))

  (def r (trace/reactor! {'>control !control
                          '>p       !p
                          '>q       !q}
           init-server))

  (def !trace (trace/log! r))

  (trace/directive! r '[>p 3])

  @!trace := '[{>p         3
                [>cross]   [#{0 1 2} #{}]
                [>cross 0] 0
                [>cross 1] 1
                [>cross 2] 2}]

  (trace/directive! r '[>p 4])

  @!trace := '[{>p         3
                [>cross]   [#{0 1 2} #{}]
                [>cross 0] 0
                [>cross 1] 1
                [>cross 2] 2}
               {>p         4
                [>cross]   [#{3} #{}]
                [>cross 3] 3}]

  )

