(ns hyperfiddle.walk)

(declare has-meta?)
#?(:clj (defn has-meta? [o] (instance? clojure.lang.IMeta o)))

(defn walk [inner outer form]
  (cond
    (list? form)      (outer form (apply list (map inner form)))
    (map-entry? form) (outer form (first {(inner (key form)) (inner (val form))}))
    (seq? form)       (outer form (doall (map inner form))) ;; Must be after `list?` and `map-entry?`
    (record? form)    (outer form (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form)      (outer form (into (empty form) (map inner form)))
    :else             (outer form form)))

(defn forward-metas [form form']
  (if (has-meta? form')
    (with-meta form' (merge (meta form) (meta form')))
    form'))

(defn prewalk [f form]
  (if (reduced? form)
    (unreduced form)
    (unreduced (walk (partial prewalk f) forward-metas (f form)))))

(defn postwalk [f form]
  (if (reduced? form)
    (unreduced form)
    (unreduced (walk (partial postwalk f) (fn [form form'] (forward-metas form (f (forward-metas form form'))))
                     form))))
