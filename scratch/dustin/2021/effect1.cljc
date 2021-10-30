(ns dustin.2021.effect1)


(comment

  (declare via Do-via *this)

  (deftype Eval [state]
    Do-via
    (inject [R]
      {'Eval/log   (fn [msg]
                     (set! *this* (update *this :state conj msg)))
       'Eval/flush (fn []
                     (let [log (:state *this)]
                       (println log) (flush)
                       (set! (:state *this) [])))}))

  (via (->Eval [])
       (:require [Eval :as E :refer [log]])
       (let [_ (log "hello")
             _ (log "world")
             _ (log "hello")
             _ (log "earth")]
         (E/flush)))


  )




(comment


  (defn just [v] {:Maybe/just v})                           ; none is nil

  ; https://twitter.com/dustingetz/status/1305190850784309249
  ; https://twitter.com/dustingetz/status/1312762707842564104
  (deftype Maybe []
    Do-via
    (resolver-for [R]
      {:Do.pure   (fn [v] {:Maybe/just v})
       :Do.fmap   (fn [f & fvs]
                    (let [vs (map :Maybe/just fvs)]
                      (if (every? identity vs)
                        (just (apply f vs)))))
       :Do.fapply (fn [& avs] (apply ! :Do.fmap #(apply % %&) avs))
       :Do.bind   (fn [{v :Maybe/just} cont] (if v (cont v)))}))
  => hyperfiddle.via.Maybe

  (via (->Maybe)
       (for [f (just +)
             a (just 1)
             b ~(~f 10 ~a)]
         (pure (inc b))))
  => #:Maybe{:just 12}

  (via (->Maybe)
       (for [a ~(just 1)
             b ~(+ a ~(just 42))
             c (for [i (range (+ a 2))] i)]                    ; vanilla for
         (pure (+ a b (reduce + c)))))
  => #:Maybe{:just 47}
  )