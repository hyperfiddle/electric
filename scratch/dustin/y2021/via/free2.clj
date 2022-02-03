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

(defn h1 [& children] `(h1 ~@children))
(defn div [& children] `(div ~@children))
(defn table [& children] `(table ~@children))
(defn span [& children] `(span ~@children))
(defn tr [& children] `(tr ~@children))
(defn email [& children] `(email ~@children))

;(defn fix [x & args] `(fix ~x ~@args))
;(def h1 (partial fix :h1))

(comment
  "render-table"
  (def !result (atom []))
  (def !xs (atom [{:id 100 :name "alice"} {:id 101 :name "bob"}]))

  ; Each component has a reactor signal for its past value
  ; Since they are not called from rfor – they are static calls –
  ; the react keys are not needed

  (defn render-table [xs]
    `@(div. @(h1. "title")
       @(table.
         @(span. ~(count xs))
         @(for [x :id xs] `(tr. @(email. ~x)))
         @(foreach. xs :id ~(fn [x]                         ; unquote here?
                              `(tr. @(email. ~x)))))))

  (run-react {'log (partial log! !result)
              'render-table render-table}
    `(log. (render-table. @~xs)))

  (swap! !xs assoc-in [0 :name] "alice2")
  @!result := _)

(defn map-by [f xs]
  (into {} (map (juxt f identity)) xs))

(tests
  (map-by :id [{:id 100 :name "alice"} {:id 101 :name "bob"}])
  := {100 {:id 100, :name "alice"},
      101 {:id 101, :name "bob"}})

;(defn entity [xs kf k]
;  (let [index (map-by kf xs)]
;    (m/signal! k (index k))))
