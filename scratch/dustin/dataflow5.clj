(ns dustin.dataflow5)

(dataflow (inc x))
;x -> (inc %)

(main
  '(m/latest apply (get env 'inc) [(get env 'x)])
  {'inc (m/watch (atom clojure.core/inc))
   'x   (m/watch (atom 0))})






(let [mutable! (fn [seed]
                 (m/watch (atom seed)))]
  (compile-dataflow
    (def f (fn [y]
             (let [w (mutable! 0)]
               (if (odd? y)
                 (inc y)
                 (dec y)))))
    (inc (new f (inc x)))
    ))


(def f :! a -> m b
  (compile-dataflow
    (fn [y]
      (let [w (mutable! 0)]
        (if (odd? y)
          (inc y)
          (dec y))))))

(compile-dataflow (inc (new f (inc x))))
(compile-dataflow (inc (join (f (inc x)))))
(compile-dataflow (inc (join (submissions-query! (inc x)))))

(defn submissions-query!  :! a -> m b
  [x]
  (m/via m/cpu (d/q ...)))




(let [mutable! (fn [seed]
                 (m/watch (atom seed)))
      f (fn [y]
          (atom ...)
          (m/signal!
            (cp
              #_(let [w (mutable! 0)]
                  (if (odd? y)
                    (inc y)
                    (dec y))))))]
  (dataflow
    (inc (f. (inc x)))
    ))


reify























(dataflow

  (inc (inc (inc x)))

  )
x -> (inc %) -> (inc %) -> (inc %)

(run
  '(m/latest apply (get env 'inc) [(get env 'x)])

  {'inc ... 'x ...})


(dataflow

  (def f (ifn [y]
           (if (odd? y)
             (inc y)
             (dec y))))
  (inc (f. (inc x)))

  )

(dataflow


  (def f ...)
  (inc (join (f (inc x))))

  )


(fmap x (fn [%]

          ))







(+ (inc a) (inc a))                                         ; diamond



