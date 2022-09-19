(ns dustin.missionary.missionary
  (:require
    [missionary.core :as m]
    [hyperfiddle.rcf :refer [tests tap %]]))

(tests
    (def hello-task (m/sp (println "Hello")))
    (hello-task #(print ::success %) #(print ::failure %))

    (def sleep-task (m/sleep 1000))
    (def async-hello-task
      (m/sp                            ; async Sequential Process
        (m/? hello-task)
        (m/? sleep-task)
        (println "World")
        (m/? sleep-task)
        (println "!")))

    (async-hello-task #(print ::success %) #(print ::failure %))

  ;(? slowmo-hello-world)

  (def chatty-hello-world (join vector slowmo-hello-world slowmo-hello-world))
  (? chatty-hello-world)

  (? (m/join vector
       (m/sp (inc (m/? (m/sleep 1000 1))))
       (m/sp (inc (m/? (m/sleep 100 2))))))

  (? (join vector
       (m/sleep 1000 1)
       (m/sleep 100 2)))

  (def unreliable-hello-world
    (sp (println "hello")
      (? (m/sleep 300))
      (throw (ex-info "Something went wrong." {}))))

  (? (sp (try (? unreliable-hello-world)
              (catch Exception e e))))

  ; ? outside sp blocks on jvm
  (try (? unreliable-hello-world)
       (catch Exception e :caught))

  (def t (sp
           (try
             (println "hello")
             (? (m/sleep 100))
             (println "world")
             (? (m/sleep 100))
             (finally
               (println "!")))))
  (t prn prn)
  (? (m/absolve t))                                         ;?


  ; flow

  (def input (enumerate (range 10)))
  (? (aggregate + input))
  (? (aggregate conj '() input))

  (? (aggregate conj [] (m/zip vector input input)))

  (? (->> (m/zip vector
            (enumerate [1 2 3])
            (enumerate [:a :b :c]))
       (aggregate conj)))

  ;(? input)

  (? (aggregate conj (m/transform (partition-all 4) input)))

  (def >s
    (ap
      (let [x (?? (enumerate [1 2 3]))]
        (println x)
        (inc (? (m/sleep 300 x))))))

  (? (aggregate conj >s))

  (defn debounce [n >a]
    (ap
      (let [a (?! >a)]
        (? (m/sleep n a))
        #_(try
          (? (m/sleep n a))
          (catch Exception _ (?? m/none))))))

  (defn clock [intervals]
    (ap (let [n (?? (enumerate intervals))]
          (? (m/sleep n n)))))

  (? (aggregate (constantly :done) (clock [100 200 100 200])))

  (? (->> (clock [24 79 67 34 18 9 99 37])
       (debounce 50)
       (aggregate conj)))


  (def >as (clock [100 200 100 200]))
  (>as prn prn)

  (? (->>
       (ap
         (let [x (?= (enumerate [19 57 28 6 87]))]          ; fork in a way that will emit asap, out of order
           (? (m/sleep x x))))
       (aggregate conj)))

  (? (aggregate conj (ap (?= (enumerate [19 57 28 6 87])))))

  )


(comment
  (defn sleep-emit [delays]
    (ap (let [n (?? (enumerate delays))]
          (? (m/sleep n n)))))

  (? (aggregate conj
       (m/sample vector
         (sleep-emit [24 79 67 34])
         (sleep-emit [86 12 37 93])
         #_(m/watch r)
         )))

  (? (->>
       (m/sample vector
         (sleep-emit [24 79 67 34])
         (sleep-emit [86 12 37 93]))
       (aggregate conj)))


  )
