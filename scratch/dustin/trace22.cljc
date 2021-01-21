(ns dustin.trace22
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [dustin.trace17 :as trace]
            [leo.extend-seq :refer [extend-seq focus-entity diffp diff-by]]))

;;;;;;;;;;;;
;; SERVER ;;
;;;;;;;;;;;;

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

(def fmap m/latest)

(defn reactive-for [kf >as f]
  ;; realigning elements could happen here
  (fmap (fn [k>a]
          (into {}
                (for [[k >v] k>a]
                  [k (fmap f >v)])))
        (extend-seq kf >as)))

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
                                                      (let [[k >v] (m/?= (m/enumerate (m/?? >cross-registry)))]
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
               [>cross 2] 2}])

;; * We did learn:
;;
;; - In order to produce a diff-tracing server, we need to split up
;;   `reactive-for` into 4 steps:
;;   - `>node-in` computes the input sequence of the reactive-for (aka pre-diff
;;     value). We don’t have to trace this value.
;;   - `>node-diff` computes the diff between previous version and current
;;     version, emits `[#{adds} #{rets}]`. We can trace this.
;;   - `>node-registry` computes a map of `{id flow}`, where flow is focused and
;;     stable. We leverage its map structure to produce trace elements for
;;     children flows `{[>node child-id] value}`.
;;   - `>node-out` final result of reactive-for (as a sequence). We don’t have
;;     to trace it.
;;
;; * TODO Next [0/5]
;;
;; ** TODO [#A] multiple levels of nesting
;;
;; ** TODO [#B] Handle #{rets} from diff
;;
;;    Focused flows are removed form the registry but don’t terminate. We must
;;    terminate them manually to avoid memory leaks.
;;
;; ** TODO [#B] Ordered diff
;;
;; ** TODO [#C] hfql recursivity/loops
;;    Is it an HFQL only concern?
;;
;; ** TODO [#C] dataflow compiler
;;
