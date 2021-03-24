(ns dustin.trace23
  (:require [dustin.trace17 :as trace :refer [reactor!]]
            [dustin.trace22 :refer [active-flows diff-seq]]
            [leo.extend-seq :refer [focus-entity]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

;;;;;;;;;;;;;;;;;;
;; Nested Binds ;;
;;;;;;;;;;;;;;;;;;

(def cartesian-product '(for [age ages]
                          (for [name names]
                            [name age])))
;; => [["Jack" 42]
;;     ["Alice" 22]]

(defmacro rfor [kf [bound-sym >flow & bindings] & body]
  `(reactive-for ~kf ~>flow (fn [~bound-sym]
                              ~(if (empty? bindings)
                                 `(do ~@body)
                                 `(rfor ~kf ~bindings ~@body)))))

(def ast '(let [>ages    (input)
                >names   (input)
                >product (rfor identity [age >ages]
                               (rfor identity [name >names]
                                     [name age]))]))

(defn init-server [inputs _!replayers tracef]
  (let [>ages           (m/signal! (m/watch (get inputs '>ages)))
        >names          (m/signal! (m/watch (get inputs '>names)))
        >ages-diff      (m/stream! (diff-seq identity >ages))
        >ages-registry  (m/signal! (m/transform (active-flows (fn [child-id]
                                                                (focus-entity identity child-id >ages)))
                                                >ages-diff))
        >ages-out       (m/signal! (m/latest (fn [k>a] (into {}
                                                            (for [[k >v] k>a]
                                                              [k >v])))
                                             >ages-registry))
        >names-diff     (m/stream! (diff-seq identity >names))
        >names-registry (m/signal! (m/transform (active-flows (fn [child-id]
                                                                (focus-entity identity child-id >names)))
                                                >names-diff))
        >product        (m/signal! (m/latest (fn [ages> names>] (into {}
                                                                     (for [[k1 >v1] ages>
                                                                           [k2 >v2] names>]
                                                                       [[k1 k2] (m/latest vector >v1 >v2)])))
                                             >ages-registry
                                             >names-registry))


        >effects (m/stream! (m/relieve merge (m/ap
                                              (trace/amb=
                                               {'>ages (m/?? >ages)}
                                               {'>names (m/?? >names)}
                                               {'[>product] (m/?? >names-diff)}
                                               ;; TODO trace according to diff
                                               (let [[k >v] (m/?= (m/enumerate (m/?? >product)))]
                                                 {(into ['>product] k) (m/?? >v)})
                                               ))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))

(tests
 (def !ages (atom []))
 (def !names (atom []))

 (def r (reactor! {'>names !ages
                   '>ages  !names}
                  init-server))

 (def !trace (trace/log! r))

 (trace/directive! r '[>ages [42 25 27]])
 (trace/directive! r '[>names ["Alice" "Bob" "John"]])

 @!trace := '[{>ages [42 25 27]}
              {>names                  ["Alice" "Bob" "John"]
               [>product]              [#{"Alice" "John" "Bob"} #{}]
               [>product 42 "Alice"] [42 "Alice"]
               [>product 25 "Alice"] [25 "Alice"]
               [>product 27 "Alice"] [27 "Alice"]
               [>product 42 "Bob"]   [42 "Bob"]
               [>product 25 "Bob"]   [25 "Bob"]
               [>product 27 "Bob"]   [27 "Bob"]
               [>product 42 "John"]  [42 "John"]
               [>product 25 "John"]  [25 "John"]
               [>product 27 "John"]  [27 "John"]}])


;; (def ast '(let [>input (input)]
;;             (rfor [a identity >input
;;                    b identity (range a)]
;;                   [a b])))
;; (for [a (range 3)
;;       b (range a)]
;;   [a b])
;; ;; => ([1 0] [2 0] [2 1])

;; '[{>input   3 ["Alice" "Bob" "John"]
;;    [>a]     [#{0 1 2} #{}]
;;    [>a 0]   [#{} #{}]
;;    [>a 1]   [#{0} #{}]
;;    [>a 2]   [#{0 1} #{}]
;;    [>a 1 0] [1 0]
;;    [>a 2 0] [2 0]
;;    [>a 2 1] [2 1]}]


;; * We did learn
;;
;;   - In order to trace nested `reactive-for` (cartesian product), the compiler
;;     need to hoist names to the top-level let. Does it mean the compiler need to
;;     produce a single, flat let?
;;
;;   - It might happen that node ids are just points in the AST or paths to
;;     points in the AST. Think sourcemaps but with dynamic DAG.
