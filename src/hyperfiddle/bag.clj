(ns hyperfiddle.bag
  #:clj-kondo {:config
              '{:lint-as #:hyperfiddle.fabric {defnode clojure.core/def}}}
  (:require [hyperfiddle.fabric :as f :refer [defnode]]
            [hyperfiddle.trace :as trace]
            [hyperfiddle.viz :as v]
            [minitest :refer [tests]]))

(let [[step end] (v/animation "/tmp/bag")]
  (defnode >a (f/input))
  (defnode >b (f/fmap identity >a))
  (defnode >c (f/fmap identity >a))
  (defnode >d (f/fmap vector >a >b >c))
  ;; (defnode >a' (f/input))
  ;; (defnode >b' (f/fmap identity >a'))
  ;; (defnode >c' (f/fmap identity >a'))
  ;; (defnode >d' (f/fmap vector >a' >b' >c'))
  ;; (defnode >a'' (f/input))
  (defnode >e (f/fmap vector #_>a' >d #_>a'' >a  #_>d'))
  ;; (defnode >e (f/fmap vector >a >d))
  (defnode >f (f/on >e prn))
  (step >f)
  (f/put >a 1)
  (step >f)
  (f/put >a' 1)
  (step >f)
  (f/put >a'' 1)
  (step >f)
  (end))

(tests
 (do
   (defnode >control (f/input))
   (defnode >a (f/input))
   (defnode >b (f/input))
   (defnode >cross (f/bindR >control (fn [c] (case c :a >a :b >b))))
   (defnode >x (f/fmap vector >cross))
   (f/on >x prn)

   (v/capture-gif "/tmp/bind" >x
                  (f/put >control :b)
                  (f/put >b 200)
                  (f/put >control :a)
                  (f/put >a 100))

   (trace/trace >x)) => '([>control :a] [>cross 100] [>x [100]]) ;; ✔
 )


