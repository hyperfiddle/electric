(ns dustin.trace32
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m]))


(declare ... _)

(tests
  "simple diamond"

  (def !a (atom 1))
  (def !b (atom 2))

  (def >a (m/watch !a))
  (def >b (m/watch !b))
  (def ast '(fmap + >a >b))                                 ;(def ast '(+ ~>a ~>b))

  ; this is a source map

  (tree-seq coll? identity ast) := '((fmap + >a >b) fmap + >a >b)
  (nth (tree-seq coll? identity ast) 0) := '(fmap + >a >b)
  (nth (tree-seq coll? identity ast) 1) := 'fmap
  (nth (tree-seq coll? identity ast) 2) := '+
  (nth (tree-seq coll? identity ast) 3) := '>a
  (nth (tree-seq coll? identity ast) 4) := '>b

  ; there is not always a symbol, there are anonymous nodes
  @trace
  := [{3 1
       4 2
       0 3}]

  (reset! !a 10)
  @trace
  := [{3 1
       4 2
       0 3}
      {3 10
       0 12}])

(tests
  "monad case"
  ;(def ast '(let [>cross (case ~>control :p >p :q >q)
  ;                >z (vector >cross)]))

  (def !control (atom :p))
  (def !p (atom 1))
  (def !q (atom 2))

  (def >control (m/watch !control))
  (def >p (m/watch !p))
  (def >q (m/watch !q))

  ;(fmap
  ;  vector
  ;  (bind >control (fn [control]
  ;                   (case control :p >p :q >q))))

  ; history sensitivity?
  (def ast
    '(let [>cross (bind >control (fn [control]
                                   (case control
                                     :p >p
                                     :q >q)))]
       (fmap vector >cross)))

  (def sourcemap (tree-seq coll? identity ast))
  (nth sourcemap 6) := '>control
  (nth sourcemap 3) := '>cross
  (nth sourcemap 15) := '>p
  (nth sourcemap 18) := '(fmap vector >cross)

  @trace
  := [{6  :p
       15 1
       3  1
       18 [1]}]

  (reset! !p 10)

  @trace := [{6  :p
              15 1
              3  1
              18 [1]}
             {15 10
              3  10
              18 [10]}]

  ;(reset! !control :q)
  )

(tests
  "reactive-list case"

  (def !control (atom :p))
  (def !p (atom 1))
  (def !q (atom 2))

  (def >control (m/watch !control))
  (def >p (m/watch !p))
  (def >q (m/watch !q))

  ;(compile '(reactive-for identity [>a (range (case ~>control :p >p :q >q))] ...))

  ; three nodes where two would suffice
  (def ast
    '(fmap vector (bind >control (fn [control]
                                   (fmap range
                                     (case control :p >p :q >q))))))

  (def ast2
    '(mlet [control >control
            cross (case control :p ^{:key :P} >p ^{:key :Q} :q >q)]
       (reactive-for [x keyword (range cross)]
         (vector (range cross)))))

  (def ast2
    '(mlet [cross (fmap #(case % :p >p :q >q) >control) ]
       (vector (range cross))))

  ; best one because no macros and only two nodes in dag
  (def ast
    '(bind >control (fn [control]
                      (fmap (comp vector range) (case control :p >p :q >q)))))

  (defn foo []
    (fmap (comp vector range) (case control :p >p :q >q)))

  (def ast
    '(bind >control (fn [control]
                      #_(foo control)
                      (fmap (comp vector range) (case control
                                                  :p ^{:key :P} >p
                                                  :q ^{:key :Q} >q)))))

  (def sourcemap (tree-seq coll? identity ast))
  (nth sourcemap 2) := '>control
  (nth sourcemap 13) := '(case control :p >p :q >q)
  (nth sourcemap 7) := '(fmap (comp vector range) (case control :p >p :q >q))
  (nth sourcemap 17) := '>p
  (nth sourcemap 19) := '>q
  (nth sourcemap 0) := '(bind >control (fn [control] (fmap (comp vector range) (case control :p >p :q >q))))
  ;(nth sourcemap 18) := '(fmap vector >cross)


  @trace := [{2 #_>control :p
              17 #_>p      1
              19 #_>q      2
              7            [[0]]

              [0 :P :0]       [[0]]
              [0 :P :1]       [[0]]
              [0 :P :2]       [[0]]
              [0 :Q :2]       [[0]]

              }]
  ; the point of a reactkey is to let us talk about what happened inside a continuation

  (reset! ...)
  @trace := [{2 #_control :p}
             {}]

  (def ast
    '(bindfor >control (fn [control]
                             #_(foo control)
                             (fmap (comp vector range) (case control
                                                         :p ^{:key :P} >p
                                                         :q ^{:key :Q} >q)))))



  @trace
  := [{[0]    #{{:id :a :name "alice"}
                {:id :b :name "bob"}}
       [1]    [#{:a :b} #{}]
       [1 :a] {:id :a :name "alice"}
       [1 :b] {:id :b :name "bob"}
       [2 :a] "alice"
       [2 :b] "bob"}]

  )

(tests
  "hfql submissions"
  )





(tests
  "is react-key about bind???"
  ; Dustin asserted: React key is not about diffing. React key is about bind, and changing topology

  (def ast
    '(let [>cross (bind >control (fn [control]
                                   (case control
                                     :p ^{:key :p} (fmap identity >p)
                                     :q ^{:key :q} (fmap identity >q)
                                     :r ^{:key :r} (m/watch (atom 42)))))]
       (fmap vector >cross)))

  )