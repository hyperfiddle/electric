(ns user.photon.photon-compiler-internals
  (:require [hyperfiddle.photon :as p]
            #?(:clj [hyperfiddle.photon-impl.compiler :refer [analyze]])
            #?(:clj [hyperfiddle.photon-impl.runtime :as r :refer [emit]])
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))


; Internals

(tests
  "Photon compiler"
  (def !x (atom 0))
  (analyze {} '(let [x (new (m/watch !x))
                     a (inc x)]
                 (rcf/! (+ a (inc x)))))
  := [[:pub
       [:pub
        [:apply [:global :missionary.core/watch] [:global :user.photon-compiler-internals/!x]]
        [:bind 0 1 [:variable [:sub 1]]]]
       [:pub
        [:apply [:global :clojure.core/inc] [:sub 1]]
        [:apply
         [:global :hyperfiddle.rcf/!]
         [:apply [:global :clojure.core/+] [:sub 1] [:apply [:global :clojure.core/inc] [:sub 2]]]]]]
      [:source [:literal nil]]]

  (def client (first *1))
  (def server (second *2))

  "emitted target is approximately the missionary program"
  (emit nil client) :=
  `(r/peer
     1 0 0 0 3 1 0
     (fn [~'nodes]
       (let [~'pub0 (r/signal 1 (let [~'pub0 #_"x" (r/signal 0 (r/latest-apply (r/steady m/watch) (r/steady !x)))]
                                  (let [~'nodes (assoc ~'nodes 0 ~'pub0)]
                                    (r/variable 0 ~'nodes ~'pub0))))]
         (let [~'pub1 #_"y" (r/signal 2 (r/latest-apply (r/steady inc) ~'pub0))]
           (r/latest-apply (r/steady rcf/!)
                           (r/latest-apply (r/steady +) ~'pub1
                                           (r/latest-apply (r/steady inc) ~'pub0)))))))

  (emit nil server) :=
  `(r/peer
     0 0 0 1 0 0 0
     (fn [~'nodes]
       (do (r/source 0 ~'nodes) (r/steady (quote nil)))))
  )
