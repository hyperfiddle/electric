(ns dustin.y2022.photon-compiler-old
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :refer [analyze]]
            [hyperfiddle.photon-impl.runtime :as r :refer [emit]]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))


(tests

  (analyze {} '(p/fn [n]
                 (let [x (+ n 2)]
                   (p/fn [y]
                     (+ y x)))))

  ; tuple of [client, server]
  := [[:constant
       [:pub
        [:node 0]
        [:pub
         [:apply [:global :clojure.core/+] [:sub 1] [:literal 2]]
         [:constant [:pub [:node 0] [:apply [:global :clojure.core/+] [:sub 1] [:sub 2]]]]]]]
      ; this is a nil server program
      [:target [:target [:nop] [:nop]] [:literal nil]]]

  ; p/fn is split into a constant + target (Yes p/fn, not reactive-quote)
  ; a target is a recipe to construct a piece of DAG that is constructed on demand by the remote peer

  (def client (first *1))
  (def server (second *2))

  (emit (comp symbol str) client) :=
  `(r/peer
     1 0 0 0 0 0 0
     (clojure.core/fn [~'nodes]
       (r/steady
         (r/constant
           0 0 0 0 2 0 0
           (clojure.core/fn [~'nodes]
             (clojure.core/let [~'pub0 (r/signal 0 (clojure.core/nth ~'nodes 0))]
               (clojure.core/let [~'pub1 (r/signal 1 (r/latest-apply (r/steady clojure.core/+) ~'pub0 (r/steady (quote 2))))]
                 (r/steady
                   (r/constant
                     0 0 0 0 1 0 0
                     (clojure.core/fn [~'nodes]
                       (clojure.core/let [~'pub2 (r/signal 0 (clojure.core/nth ~'nodes 0))]
                         (r/latest-apply (r/steady clojure.core/+) ~'pub2 ~'pub1))))))))))))

  ; this is a noop server, our current implementation produces clients and servers with matching closure structures
  ; so the closure IDs always align
  (emit (comp symbol str) server) :=
  `(r/peer
     0 0 1 0 0 0 0
     (clojure.core/fn [~'nodes]
       (do
         (r/target
           0 0 1 0 0 0 0
           (clojure.core/fn [~'nodes]
             (do (r/target
                   0 0 0 0 0 0 0
                   (clojure.core/fn [~'nodes]
                     nil)) nil)))
         (r/steady (quote nil)))))

  )
