(ns user.photon.photon-compiler-internals
  (:require [hyperfiddle.photon :as p]
            #?(:clj [hyperfiddle.photon-impl.compiler :refer [analyze]])
            #?(:clj [hyperfiddle.photon-impl.runtime :as r :refer [emit]])
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(tests
  "Photon compiler"
  (def !x (atom 0))

  ; client only first example
  (def env {})
  (analyze env '(let [x (new (m/watch !x))
                      a (p/server (inc x))]
                  (rcf/! (+ a (inc x)))))

  ; we analyze the DAG for connections - by analyzing dependencies in the AST

  ; Intermediate Language for DAGs.
  ; first element is the client DAG,
  ; second element is server DAG
  := [[:pub
       [:pub
        [:apply [:global :missionary.core/watch] [:global :user.photon.photon-compiler-internals/!x]]
        [:bind 0 1 [:variable [:sub 1]]]]
       [:pub
        [:output [:sub 1] [:input]]
        [:apply
         [:global :hyperfiddle.rcf/!]
         [:apply [:global :clojure.core/+] [:sub 1] [:apply [:global :clojure.core/inc] [:sub 2]]]]]]
      [:source [:output [:apply [:global :clojure.core/inc] [:input]] [:literal nil]]]]

  (def client (first *1))
  (def server (second *2))


  (tests
    "emitted reactive target program for client"
    (emit nil client) :=
    '(hyperfiddle.photon-impl.runtime/peer
       1 [] 1 0 0 0 1 1
       (clojure.core/fn
         [-frame -vars]
         (clojure.core/let
           [-pub-0
            (hyperfiddle.photon-impl.runtime/signal
              (clojure.core/let
                [-pub-0
                 (hyperfiddle.photon-impl.runtime/signal
                   (hyperfiddle.photon-impl.runtime/latest-apply
                     (hyperfiddle.photon-impl.runtime/pure missionary.core/watch)
                     (hyperfiddle.photon-impl.runtime/pure user.photon.photon-compiler-internals/!x)))]
                (hyperfiddle.photon-impl.runtime/latest-last
                  -pub-0
                  (clojure.core/let
                    [-prev (clojure.core/aget -vars 0)]
                    (clojure.core/aset -vars (clojure.core/int 0) -pub-0)
                    (clojure.core/let
                      [-res (hyperfiddle.photon-impl.runtime/variable -frame -vars 0 0 -pub-0)]
                      (clojure.core/aset -vars (clojure.core/int 0) -prev)
                      -res)))))]
           (hyperfiddle.photon-impl.runtime/latest-last
             -pub-0
             (clojure.core/let
               [-pub-1
                (hyperfiddle.photon-impl.runtime/signal
                  (do (hyperfiddle.photon-impl.runtime/output -frame 0 -pub-0) (hyperfiddle.photon-impl.runtime/input -frame 0)))]
               (hyperfiddle.photon-impl.runtime/latest-last
                 -pub-1
                 (hyperfiddle.photon-impl.runtime/latest-apply
                   (hyperfiddle.photon-impl.runtime/pure hyperfiddle.rcf/!)
                   (hyperfiddle.photon-impl.runtime/latest-apply
                     (hyperfiddle.photon-impl.runtime/pure clojure.core/+)
                     -pub-1
                     (hyperfiddle.photon-impl.runtime/latest-apply (hyperfiddle.photon-impl.runtime/pure clojure.core/inc) -pub-0))))))))))

  (tests
    "emitted server target program"
    (emit nil server) :=
    '(hyperfiddle.photon-impl.runtime/peer
       0 [] 0 1 0 0 1 1
       (clojure.core/fn
         [-frame -vars]
         (do
           (hyperfiddle.photon-impl.runtime/source -frame -vars 0 0)
           (do
             (hyperfiddle.photon-impl.runtime/output
               -frame 0
               (hyperfiddle.photon-impl.runtime/latest-apply
                 (hyperfiddle.photon-impl.runtime/pure clojure.core/inc)
                 (hyperfiddle.photon-impl.runtime/input -frame 0)))
             (hyperfiddle.photon-impl.runtime/pure (quote nil)))))))
  )
