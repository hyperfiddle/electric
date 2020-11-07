(ns fixplz.match)


(defmacro match [& xs]
  (letfn
    [(matcher? [x]
       (and (form? x)
         (= 'let (first x))))
     (denote [xs]
       (cond (empty? xs)
             `(throw (ex-info "no match" {}))
             (matcher? (first xs))
             (let [[_ bs & r] (first xs)
                   bs (destructure bs) ; todo
                   vars (take-nth 2 bs)]
               `(let [~@bs]
                  (if (every? (comp not nil?) [~@vars])
                    (do ~@r)
                    ~(denote (rest xs)))))
             () (throw (ex-info "unexpected" {:x (first xs)}))))]
    (if (not (matcher? (first xs)))
      (let [[v & xs] xs]
        `(let [~'&_ ~v]
           ~(denote xs)))
      (denote xs))))
