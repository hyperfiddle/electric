(ns dustin.trace3
  "missionary compatible traces? we think yes"
  (:require [minitest :refer [tests]]
            [missionary.core :refer [latest relieve watch ap ?!]]))


(tests

  (def ast
    '(let [>p (atom nil)
           >q (atom nil)
           >control (atom nil)
           >cross (case >control :p >p :q >q)
           >z (vector >cross)]))

  (def ast
    '(let [>p (atom nil)
           >q (atom nil)
           >control ~(atom nil)
           >cross ~(case >control :p >p :q >q)
           >z ~(vector ~>cross)]))

  (def >p (atom nil))
  (def >q (atom nil))
  (def >control (atom nil))
  (def ast
    '(let [>cross ~(case ~>control :p >p :q >q)
           >z (vector ~>cross)]))

  (def >p (atom nil))
  (def >q (atom nil))
  (def >control (atom nil))
  (def ast '(let [>cross (bind >control (fn foo [c]
                                           (case c :p >p :q >q)))
                  >z (fmap vector >cross)]))

  "this flow is free from the ast"
  (def flow
    (eval-via {'bind (fn [>a f] (cp (?! (f (?! >a)))))
               'fmap (fn [f & >as] (apply latest f >as))}
      ast))

  '(let [>cross (case >control :p >p :q >q)
         ?z (vector >cross)])

  (def >z
    (sp
      (let [>cross (case (?! >control) :p >p :q >q)
            z (vector (?! >cross))])))

  (def >z
    (sp
      (let [>cross (case ~>control :p >p :q >q)
            z (vector ~>cross)])))


  ; ast is a value
  ; can move ast to client
  ; monad is parameterized, can be continuous on client and discrete on server

  )