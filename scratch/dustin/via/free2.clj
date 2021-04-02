(ns dustin.via.free2
  (:require
    [clojure.walk :refer [postwalk]]
    [dustin.via.free :refer [interpret-1 Interpreter]]
    [meander.epsilon :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]
    [missionary.core :as m]))

;(defprotocol Interpreter
;  (apply-id [M as])
;  (fapply [M ms])
;  (join [M mma])
;  (pure [M c])
;  #_(interpret-1 [M form])
;  #_(interpret [M ast]))

(defn interpret [interpreter effects ast]
  (clojure.walk/postwalk
    (partial interpret-1 interpreter effects)
    ast))

(deftype Eval-react [effects]
  Interpreter
  (pure [M c] (m/ap c))
  (fapply [M ms] (m/signal! (apply m/latest #(apply % %&) ms)))
  (join [M mma]
    (m/signal!
      (m/relieve {}
        (m/ap (m/?! (interpret M effects (m/?! mma))))))))

(defn run-react [effects ast]
  ((m/reactor
     (interpret (new Eval-react effects) effects ast))
   (fn [_] (println ::finished))
   (fn [e] (println ::crashed e))))

(defn log! [!result x]
  `~(m/ap (swap! !result conj x)))

(tests
  "react-style, one interpreter at top"
  (def !result (atom []))
  (def !x (atom 0)) (def x (m/watch !x))

  (defn main [a]
    `(+ @~x ~a))

  (run-react {'log (partial log! !result)
              'main main}
    `(log. @(main. 100)))

  (swap! !x inc)
  @!result := [100 101]
  )
