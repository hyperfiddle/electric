(ns dustin.dataflow3
  (:require
    [missionary.core :as m]))

; missionary separates reactor from behavior
; reactor = dag state (it varies over time)
; behavior = AST ?

(declare cp)

(defmacro via [body]
  `(cp ~body))


(comment

  (defn f [a] (via reactor (inc ~(pure 1))))
  (via reactor (inc ~(f)))


  ;(def reactor {})
  (via reactor (inc ~(via reactor (inc ~(pure 1)))))
  := #:Maybe{:just 3}

  (cp (inc (? >x)))                                         ; behavior is RT

  (via reactor (inc (? >x)))

  )


; why do we want composable reactor state?

(comment

  (def trace [{} ...])

  (trace (via reactor (inc ~(pure 1)) trace))
  := trace

  )

; what is a reactor?
; reactor is a process
; a process is a running program
; we want to connect to this process to publish its state


; process could be RT
(comment
  (def process {:ast   ast1
                :nodes '{>p       {:type input :val nil}
                         >q       {:type input :val nil}
                         >control {:type input :val :p}}})
  )

; if reactor process state is RT then reactor processes compose
