(ns dustin.trace11
  (:require [minitest :refer [tests]]
            [missionary.core :as m :refer [ap ?? ?! ? reactor stream! signal!]]
            [hyperfiddle.incremental :as I :refer [incr?]]))


; What is the AST?


(tests

  (compile (vector 1 ~>cross))
  := '(fmap vector (pure 1) >cross)
  := '(fmap (fn [cross]
              (vector 1 cross)) >cross)


  (compile2 '(fmap vector (pure 1) >cross))
  := (cp (vector 1 (?! >cross)))
  := (cp (vector 1 (doto (?! >cross) println)))



  (via {'fmap (fn [f >a]
                *ast*
                (meta >a)
                ; add trace here
                (m/latest f >a))}
    '(fmap vector (pure 1) >cross))
  := _


  (defn fmap-incremental [] ...)
  (defn fmap-trace [] ...)
  (def fmap-both (comp fmap-incremental fmap-trace))        ; doesn't work






  '(let [>simple (vector ~>control')
         >cross ~(case ~>control' :p >p' :q >q')
         >z (vector ~>cross)
         >z (identity ~>z)]))

'(put >control :q)
'(put >q 10)

(reduce + (range ~>q))
(reduceI + 0 >a)

