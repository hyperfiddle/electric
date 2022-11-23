(ns dustin.y2022.dynamic_scope_rt
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(foo 1)

(def println)
(def *out*)

(defn foo []
  (println *out*)
  )

; Types of scope
; - local
; - lexical
; - dynamic
; - global


; explicit vs implicit


(ns user (:require [hyperfiddle.api :refer [q]]))
(defn App [db]
  (fn [x]
    (q db x)))

; somerwhere else
(App .)



(ns user (:require [hyperfiddle.api :refer [*db* q]]))
(defn App [x]
  (q *db* x))

; somewhere else
(binding [*db* .]
  (App))



(ns user (:require [hyperfiddle.api :refer [*db*]]))
(p/defn App [x]
  (println x *db*))


(defn fib [x]                                             ; something slow
  (cond
    (= x 0) 1
    (= x 1) 1
    () (+ (fib (dec x))
          (fib (dec (dec x))))))

(defn entrypoint []
  (for [x (range 10)]
    (fib x)))

(defn entrypoint []
  (for [x (range 10)]
    (loop [x x]
      (cond
        (= x 0) 1
        (= x 1) 1
        () (+ (recur (dec x))
              (recur (dec (dec x))))))))

; abstraction has a cost
; weight of abstraction




(ns user.photon.photon-compiler-internals
  (:require [hyperfiddle.photon :as p]
            #?(:clj [hyperfiddle.photon-impl.compiler :refer [analyze]])
            #?(:clj [hyperfiddle.photon-impl.runtime :as r :refer [emit]])
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(def *db*)
;(p/def *db*)

(p/defn App [x]
  (println x *db*))

(meta #'App)

(tests
  (macroexpand
    '(p/run (p/fn [schema]
              (p/fn [x]
                (println x schema)))))
  )

(p/def F (p/fn [schema]
           (p/fn [x]
             (println x schema))))

; Simple =
; call graph has fewest number of nodes and lines

(let [schema .]
  (fn foo [x]
    (println schema x)))

; class (schema): def foo (x): println schema x

(def >F (m/latest )) ; ...
(binding [p/%1 . ]
  (m/ap (let [x (m/?> >F)]
          ..
          )))

(binding [schema .]
  (fn [x]
    (println schema x)))

(let [schema .]
  (let [x 1]
    (println x schema)))

(p/defn App [F]
  (binding [p/%2 1]
    (F.)))

(tests
  (p/run
    (binding [p/%0 .]
      (App. F))))





(comment

  (def x (fn []
           (println p/%1 p/%2)))

  (binding [p/%1 1 p/%2 db] (x))


  )





