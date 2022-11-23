(ns dustingetz.y2022.dynamic-scope-rt-2
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests with tap %]]))

(hyperfiddle.rcf/enable!)

(def ^:dynamic x 0)

(tests
  "mapv is eager and sees dynamic x == 1"
  (binding [x 1]
    (mapv (fn [a] (+ x a)) [1 2 3]))
  := [2 3 4]

  "map is lazy and misses dynamic x==1 "
  (binding [x 1]
    (map (fn [a] (+ x a)) [1 2 3]))
  := [1 2 3] ; realized by the comparison

  "eduction is also lazy, and misses dynamic x == 1"
  (binding [x 1]
    (eduction (map (fn [a] (+ x a))) [1 2 3]))
  := [1 2 3] ; realized by comparison


  ; "Leo: proof that this is not about lazy seqs"

  ; Dustin - universal dynamic scope has different behavior?

  )

(tests
  (p/def x 0)
  (with (p/run (tap (binding [x 1]
                      (p/for [a [1 2 3]]
                        (+ x a)))))
    % := [2 3 4]))

(comment


  (fn [state]
    )



  (p/def X 0)
  (p/run

    (tap (new (binding [X 1]
                (p/fn [] X))))
    := 0

    (tap (binding [X 1]
           (new (p/fn [] X))))
    := 1

    )

  )




(tests
  ; https://clojure.org/guides/equality
  (range 3) := (vector 0 1 2))


(not-clojure
  "does dynamic scope break RT?"
  ; Is it RT?

  (while true)

  ; gamma
  (def env [{'inc ...} ; lexical env
            {}]) ; dynamic env

  (inc a) ; RT, ⊥

  ; L: decline to answer, program doesn't compile
  ; L: undefined



  (def inc ...)
  (let [a 1]
    (inc a)) ; L: RT
  ; RT
  ; Since this is not clojure, this does not even need to be said:
  ; under assumption alter-var-root and with-redefs is forbidden

  (def inc ...)
  (def ^:dynamic b)
  (binding [b 1]
    (inc b)) ; L: RT
  ; RT

  (inc b) ; not RT


  (def ^:dynamic b) ; clojure.lang.Var$Unbound
  user/b
  ; not RT



  (user/inc user/c)


  (inc 5) ; RT
  (foo 5)


  ; the point of RT

  (fn [state])




  (eval {'user/inc ...
         'user/c ::undefined} '(user/inc user/c))
  ; if inc is pure, the expr is RT

  ; Leo provide a snippet that is not RT

  user/b

  )



(defmacro partial-dynamic
  "Return a function calling given function `f` with given dynamic environment."
  [bindings f]
  `(cc/fn [& args#] (binding ~bindings (apply ~f args#))))

(comment




  ; second tradeoff -

  ;
  ;
  ;

  ; future work - can unify p/def and def, but needw discovery
  ; as to impact to unification across peers (db should not cross)






  (partial2 orders _ "alice")
  (fn [.] (orders . "alice"))

  )
