(ns dustin.via1)


'[{(submissions needle) {:gender [(:db/ident gender)
                                  {(shirt-sizes gender :foo) [*]}]}}
  (genders) [*]]

'[{(submissions needle) {:gender [{(shirt-sizes gender :foo) [*]}]}}]

'{(submissions needle) {:gender {(shirt-sizes gender :foo) :db/id}}}


(def j :db/id)
(def h '(shirt-sizes gender))
(def g :gender)
(def f '(submissions needle))

{(f) {(g) {(h) (j)}}}

(defn apply' [f vs & [scope trace]]
  (case f
    keyword? [trace scope (clojure.core/apply f vs)]
    sexp? [trace scope (apply f (replace scope vs))]))


(def scope {})
(def trace [])
(def pulled-tree {})
(let [[scope v] (apply' f [] scope)
, [scope v] (apply' g [v] scope)
,, [scope v] (apply' h [v] scope)
,,, [scope v] (apply' j [v] scope)
,,, pulled-tree (assoc pulled-tree '(j v) {v ...})
                ,, pulled-tree (assoc pulled-tree '(h v) {v ...})
                               , pulled-tree (assoc pulled-tree '(g v) {v ...})
      pulled-tree (assoc pulled-tree '(f) {v ...})
      ]
  v)

{(f) {(g) {(h) (j)}}}
'{(submissions needle) {:gender {(shirt-sizes gender) :db/id}}}

:=
(mlet [v (f)
       v (g v)
       v (h v)                                              ; decend twice
       v (j v)]
  (return v))

(let' [v ~(f)
       v ~(g v)
       [hv qv] ~(vector ~(h v) ~(q v))]
  (return ...))


(defn hfql-bind [mv mf]
  (let [v ((mf scope) mv)]
    (assoc trace ...)
    (assoc pulled-tree ...)
    )
  [v trace pulled-tree]
  )


(bind (f)
  (fn [v]
    (bind
      (g v)
      (fn [v]
        (bind
          (h v)
          (fn [v]
            (bind
              (j v)
              (fn [v]
                (return v)))))))))

(run *1 scope trace)



(let [[trace scope v] (apply' f [] scope trace)
      [trace scope v] (apply' g [v] scope trace)
      [trace scope v] (apply' h [v] scope trace)
      [trace scope v] (apply' j [v] scope trace)]
  v)