(ns dustin.ana2
  (:require [hfdl.impl
             [compiler :refer [analyze emit emit-inst]]
             [util :refer [with-local]]]
            [hyperfiddle.rcf :refer [tests tap %]]))

(defn emit' [asm]
  (with-local
    hfdl.impl.compiler/slot
    hfdl.impl.compiler/slots-empty
    (emit-inst (comp symbol str) 0 asm)))

(defmacro analyze'
  ([body] (first (analyze {} body)))
  ([env body] (first (analyze ~env body))))

(tests
  (analyze' (+ 2 3))
  := [[:apply
       [:global :clojure.core/+]
       [:literal 2]
       [:literal 3]]]

  (emit' (first *1))
  := '(missionary.core/latest
        hfdl.impl.util/call
        (hfdl.impl.runtime/steady clojure.core/+)
        (hfdl.impl.runtime/steady (quote 2))
        (hfdl.impl.runtime/steady (quote 3))))

(tests



  (analyze' (let [a 10] (let [b 20] [(inc a) (dec a)])))
  (analyze' (let [a 10 b 20] [(inc a) (dec a)]))
  := [[:pub
       [:literal 10]
       [:pub
        [:literal 20]
        [:apply
         [:global :clojure.core/vector]
         [:apply [:global :clojure.core/inc] [:sub 2]]
         [:apply [:global :clojure.core/dec] [:sub 2]]]]]]

  (emit' (first (analyze' (let [a 10] [(inc a) (dec a)]))))

  (emit (comp symbol str)
        [[:pub
          [:literal 10]
          [:pub
           [:literal 20]
           [:apply
            [:global :clojure.core/vector]
            [:apply [:global :clojure.core/inc] [:sub 2]]
            [:apply [:global :clojure.core/dec] [:sub 2]]]]]])
  (emit' [:pub
          [:literal 10]
          [:pub
           [:literal 20]
           [:apply
            [:global :clojure.core/vector]
            [:apply [:global :clojure.core/inc] [:sub 2]]
            [:apply [:global :clojure.core/dec] [:sub 2]]]]])

  (analyze' (let [a #'10] a))
  := [[:pub
       [:constant [:literal 10]]
       [:sub 1]]]

  (analyze' (let [a #'10] ~a))
  := [[:pub
       [:constant [:literal 10]]
       [:variable [:sub 1]]]]

  (emit' [:pub
          [:constant [:literal 10]]
          [:variable [:sub 1]]])


  (analyze' (loop [a 10] (case a 0 0 (recur (dec a)))))

  (analyze' (if true :a :b))
  := [[:variable
       [:pub
        [:constant [:literal :b]]
        [:apply
         [:apply [:global :clojure.core/hash-map] [:literal nil] [:sub 1] [:literal false] [:sub 1]]
         [:literal true]
         [:constant [:literal :a]]]]]]

  (emit' [:variable
          [:pub
           [:constant [:literal :b]]
           [:apply
            [:apply [:global :clojure.core/hash-map] [:literal nil] [:sub 1] [:literal false] [:sub 1]]
            [:literal true]
            [:constant [:literal :a]]]]])

  (emit [[:variable
          [:pub
           [:constant [:literal :b]]
           [:apply
            [:apply [:global :clojure.core/hash-map] [:literal nil] [:sub 1] [:literal false] [:sub 1]]
            [:literal true]
            [:constant [:literal :a]]]]]])

  (analyze' ~(if true #':a #':b))
  := [[:variable
       [:variable
        [:pub
         [:constant [:constant [:literal :b]]]
         [:apply
          [:apply [:global :clojure.core/hash-map]
           [:literal nil] [:sub 1] [:literal false] [:sub 1]]
          [:literal true]
          [:constant [:constant [:literal :a]]]]]]]]

  (doto (def node) (alter-meta! assoc :macro true :node ()))
  (analyze' node)
  := [[[:literal nil] [:node 0]]
      [[:literal nil]]]

  (analyze' (def node))
  := [[[:literal nil] [:def 0]] [[:literal nil]]]

  )
