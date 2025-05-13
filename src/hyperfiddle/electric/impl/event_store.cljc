(ns hyperfiddle.electric.impl.event-store
  (:refer-clojure :exclude [empty read]))

(def empty {::events [], ::projections {}})
(defn project [es k init rf]
  (update es ::projections assoc k {::rf rf, ::init init, ::v (reduce rf init (::events es))}))
(defn read [es k] (-> es ::projections (get k) ::v))
(defn act [es evt]
  (update es ::projections (fn [pj*] (update-vals pj* (fn [pj] (update pj ::v (::rf pj) evt))))))

(defn add [es evt]
  (act (update es ::events conj evt) evt))

(comment
  (-> empty (add {:foo 1, :bar 2})
    (project :foo+ 0 (fn [ac nx] (+ ac (:foo nx))))
    (add {:foo 10, :bar 20})
    (project :bar* 1 (fn [ac nx] (* ac (:bar nx))))
    (read :bar*))
  )
