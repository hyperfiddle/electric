(ns dustin.trace5
  (:require [minitest :refer [tests]]
            [hyperfiddle.incremental :as I :refer [incr?]]))

(declare flow1 viz ast ->Flow compile-incr ast2 flow2 Flow)

(tests

  (defn g [x y]                                             ; not an incr function
    (vector x y))

  ; stack function is a problem
  (def ast-clj '[(defn h [>x >y]                            ; bad, not dataflow ast
                   (let [>q (fmapI vector >x >y)
                         >qq (fmapI identity >q)]))

                 (let [>a (input)
                       >b (fmapI inc >a)
                       >a (fmapI g >a >b)
                       >z (h >a >b)])])

  (clojure.core/eval ast-clj)
  := '[>b (inc ~>a)                                         ; symbolic flow ast
       >a (g ~>a ~>b)
       >q (vector ~>a ~>b)
       q ~>q
       z ~(case q :a >a :b >b)
       >x ~(pureI (vector z q))
       ; unwind
       >qq (identity ~>q)
       >z >qq]

  (Flow '[>b (inc ~>a)
          >a (g ~>a ~>b)
          >q (vector ~>a ~>b)
          q ~>q
          z ~(case ~>q :a >a :b >b)
          >x ~(pureI (vector z ~>q))
          ; unwind
          >qq (identity ~>q)
          >z >qq])
  :=                                                        ; reified flow instance


  := (Flow '(let [#_#_>a (input)
                  >b (fmap inc >a)
                  >a (fmap g >a >b)
                  >q (fmap vector >a >b)
                  >x (bind >q (fn [q]
                                (let [>z (case q :a >a :b >b)]
                                  (bind >z (fn [z]
                                             (if (and z q)
                                               (pureI
                                                 (vector z q))))))))
                  ; unwind lexical binds
                  >qq (fmap identity >q)
                  >z >qq]))

  ; if a node is shadowed, no other flow can connect to it,
  ; therefore it need not be included in the trace

  ; BUT when we reconstruct a node from EDN, we should see all nodes?
  ; OR is it impossible to reconstruct node from EDN w/o the ast?


  (def ast *1)

  ; seq, doseq

  '(do
     (def >a (input))
     (def >b (fmap inc >a))
     (def >a (fmap #?(:clj vector, :cljs array) >a >b)))

  '{>a (input)
    >b (fmap inc >a)
    >a (fmap #?(:clj vector, :cljs array) >a >b)}

  (def flow1 (->Flow ast {}))

  (viz flow1) := '{:edges    {>a nil
                              >b inc
                              >a vector}
                   :topology #{[>a -> >b]
                               [>b -> >a]
                               [>a -> >a]}
                   :dag      {}
                   :state    {}}

  (def ast2 '(let [>c (fmap f >a >b)]))

  (compile-incr '(let [>c (fmap f >a >b)])) := '(fmapI '>c f >a >b)

  ; flows connect by symbolic name, can't see shadowed names
  (def flow2 (Flow ast2 {'>a (get flow1 '>a)
                         '>b (get flow1 '>b)
                         '>q (get flow1 '>q)}))

  )

; shadow
; client venn diagram, can connect by symbol only
; stack
