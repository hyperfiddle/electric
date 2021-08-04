(ns dustin.missionary7_reactor_doc
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m :refer [? ?? ?! ap]]))


(tests
  "reactor"
  ; reactor solves the problem of dags
  ; the problem of dags is two things:
  ; 1) concurrent execution of blocking things (e.g. threads)
  ; 2) incremental maintenance
  ;
  ; a dag can be seen as a tree where some nodes share common identity
  ; 1) parallel execution (m/latest is about this)
  ;      what if two inputs to latest share the same ref
  ; 2)

  (let [x 1
        j (inc x)
        k (dec x)
        y (vector x x)])

  (let [x 1]
    (vector
      (inc x)
      (dec x)))

  (def !x (atom 1))
  (let [>x (m/watch !x)]
    (m/latest vector                                        ; understands thread pools etc
      (m/latest inc >x)
      (cp (dec (?! >x)))                                    ; equivalent (need cp)
      >x))

  ; what's wrong with this?
  ; root latest has two branches and both refer to x
  ; x is an IO action
  ; the IO action x is executed twice

  ; Assertion: to write this where IO action X is executed once and shared,
  ; you have to give up on referential transparency
  ; For example:

  (cp
    (let [>x (m/watch !x)
          x (?! >x)]
      (vector (inc x) (dec x))
      ))


  ; What we want is to be explicit that >x should be shared between both branches of latest
  ; m/signal!


  (let [>x (m/signal! (m/watch !x))]
    (m/latest vector                                        ; understands thread pools etc
      (m/latest inc >x)
      (cp (dec (?! >x)))                                    ; equivalent (need cp)
      >x))

  ; now we've assigned internal name >x to the IO Action and it will be reused in both places

  ; however, we have to run in reactor, and nothing will happen until something discrete samples

  (defn foo [x]
    (m/signal! ...)
    (inc x))

  (def operating-system-task
    (m/reactor
      (let [>x (m/signal! (m/watch !x))
            >y (m/stream!
                 (m/latest (comp println vector)
                   (m/latest foo >x)
                   (cp (dec (?! >x)))
                   >x))]

        (m/stream! (ap (println (? (m/timeout 500 (m/sleep 1000 (?? >y)))))))
        (m/stream! (ap (println (? (m/timeout 1500 (m/sleep 1000 (?? >y))))))))))

  (def cancel (operating-system-task prn prn))
  ; run forever until we cancel

  ; the process returned by signal is compatible with the flow protocol
  ; when you run the program defined by this flow it will subscribe to this process


  ; Example of a cyclic reaction causing a recursive propogation cascade
  (m/reactor
    (let [!x (atom 0)
          >x (m/signal! (m/watch !x))]
      (m/stream!
        (ap (reset! !x (inc (?! >x)))))

      ))

  )





















