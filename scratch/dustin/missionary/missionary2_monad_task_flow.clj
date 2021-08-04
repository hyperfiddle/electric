(ns dustin.missionary2_monad_task_flow
  (:require
    [hyperfiddle.via :refer [via Do-via]]
    [minitest :refer [tests]]
    [missionary.core :as m :refer
     [? ?? ?! ?= sp ap join latest aggregate enumerate]]))


(tests
  "Task monad"

  (deftype Task []
      Do-via
      (resolver-for [R]
        {:Do.fmap   (fn [f >a] (sp (f (? >a))))
         :Do.pure   (fn [a] (sp a))
         :Do.fapply (fn [>f & >as] (apply join #(apply % %&) >f >as))
         ;:Do.bind   (fn [>a f] (sp (? (f (? >a)))))
         }))

  (def >a (m/sleep 1000 1))
  (def >z (via (->Task)
               (let [>b (inc ~>a)
                     >c (dec ~>a)]
                 (vector ~>b ~>c :x))))

  (? >z)
  := [2 0 :x]

  )

; What is a discrete flow?
; What is a continuous flow?

(tests
  "zip flows"

  (def >a (enumerate (range 3)))
  (def >z (m/zip vector >a >a))
  (? (aggregate conj >z))
  := [[0 0] [1 1] [2 2]]

  ;(? (aggregate conj (m/zip vector (ap (inc (?? >a))) (ap (dec (?? >a))) :x))) ; crash

  (? (aggregate conj (m/zip vector (ap (inc (?? >a))) (ap (dec (?? >a))) (ap :x))))
  ; truncated to shortest flow
  := [[1 -1 :x]]

  (? (aggregate conj (m/sample vector (m/watch (atom :x)) >a)))
  := [[:x 0] [:x 1] [:x 2]]

  (? (aggregate conj
       (m/sample vector
         (m/watch (atom :x))
         (enumerate (range 3))
       )))
  := [[:x 0] [:x 1] [:x 2]]

  (? (aggregate conj
       (m/sample vector
         (m/watch (atom :x))
         (enumerate (range 3))
         )))

  (? (aggregate conj
       (m/sample vector
         (m/relieve skip (enumerate (range 3)))

         (enumerate (range 3))
         )))
  := [[2 0] [2 1] [2 2]]


  (? (aggregate conj
       (m/sample vector
         (m/relieve skip (enumerate (range 3)))

         (m/gather)
         (enumerate (range 3))
         )))
  := [[2 0] [2 1] [2 2]]


  ; hyperfiddle inputs are refs, is the whole thing continuous?


  )


(tests
  "Flow monad"

  (defn skip [a b] b)

  (deftype Flow []
       Do-via
       (resolver-for [R]
         {:Do.fmap   (fn [f >a] (ap (f (?? >a))))
          :Do.pure   (fn [a] (m/watch (atom a)))

          ; discrete and continuous flows zipped
          ; make them all continuous and use latest
          ; relieve converts discrete -> continuous with a rf

          :Do.fapply (fn [>f & >as]
                       (apply m/latest #(apply % %&) >f >as)

                       #_(apply m/latest #(apply % %&)
                         (m/relieve skip >f)
                         (map (partial m/relieve skip) >as))
                       #_(apply m/zip #(apply % %&) >f >as))
          ;:Do.bind   (fn [>a f] (ap (?? (f (?? >a)))))
          }))

  (def r (atom 0))
  (def >a (m/watch r))
  (def >z (via (->Flow) (inc ~>a)))

  (?! (ap (let [x (?? >z)]
           (println x))))

  (reset! r 2)


  (? (aggregate conj (via (->Flow) (inc ~>a))))
  := [1 2 3]

  (? (aggregate conj (via (->Flow) (vector ~>a))))
  := [[0] [1] [2]]

  (? (aggregate conj (via (->Flow) (vector ~>a))))

  (? (aggregate conj (via (->Flow)
                       (vector ~(inc ~>a) ~(dec ~>a)))))
  := [2 0 :x]

  (def >z (via (->Flow)
               (let [>b (inc ~>a)
                     >c (dec ~>a)]
                 (vector ~>b ~>c :x))))

  (>z prn prn)

  (? (aggregate conj >z))
  := [2 0 :x]


  ; Flow monad
  (defn flow-unit [x] (m/ap x))
  (defn flow-concat-bind [f flow] (m/ap (m/?? (f (m/?? flow)))))
  (defn flow-switch-bind [f flow] (m/ap (m/?? (f (m/?! flow)))))
  (defn flow-gather-bind [f flow] (m/ap (m/?? (f (m/?= flow)))))

  )

(defn bind [>a f] (ap (?? (f (?? >a)))))
(defn pure [a] (ap a))

(comment
  "missionary bind tests"

  (? (aggregate conj (ap 1))) := [1]
  (? (aggregate conj (bind (pure 1) pure))) := [1]
  (? (aggregate conj (bind (pure 1) (fn [a] (pure (inc a)))))) := [2]


  (? (aggregate conj (atom 1))) := [1]


  (? (aggregate conj (bind (pure 1) (fn [a] (pure (inc a)))))) := [2]



  (def >p (atom nil))
  (def >q (atom nil))
  (def >control (atom nil))
  (def >cross (bind >control (fn [c]
                                  (case c
                                    :p >p
                                    :q >q))))
  (def >z (fmap vector >p >q >cross))
  (def s (history >z #_print))
  (do (put >control :q) (put >p 1) (put >q 2))
  @s := [[1 2 2]]

  @(cap (fmap inc (pure 1))) := 2
  @(cap (bind (fmap inc (pure 1)) (comp pure inc))) := 3

  )