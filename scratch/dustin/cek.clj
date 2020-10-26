(ns user.dustin.cek
  (:require
    [meander.epsilon :as m]))


(defn step [state]
  (m/rewrite state
    {:C (m/pred symbol? ?x)
     :E {?x (?lambda ?E*)}
     :K ?K}
    ;; =>
    {:C ?lambda
     :E ?E*
     :K ?K}

    {:C (?t1 ?t2)
     :E ?E
     :K ?K}
    ;; =>
    {:C ?t1
     :E ?E
     :K (EArg ?t2 ?E ?K)}

    {:C (lambda ?x ?t)
     :E ?E
     :K (EArg ?t* ?E* ?K)}
    ;; =>
    {:C ?t*
     :E ?E*
     :K (ECall (lambda ?x ?t) ?E ?K)}

    {:C (lambda ?x ?t :as ?lambda)
     :E ?E
     :K (ECall (lambda ?y ?t*) ?E* ?K)}
    ;; =>
    {:C ?t*
     :E {?y (?lambda ?E) & ?E*}
     :K ?K}

    ?state
    ;; =>
    ?state))

(defn steps [state]
  (let [x (iterate step state)]
    (m/rewrite x
      (!states ... ?state ?state & _)
      [!states ... ?state])))

(comment

  (def seed '{:C ((lambda x x) ((lambda y y) (lambda z z)))
              :E {}
              :K Done})
  (steps seed)

  (take 3 (iterate steps seed))
  ;; =>
  [{:C ((lambda x x) ((lambda y y) (lambda z z)))
    :E {}
    :K Done}
   {:C (lambda x x)
    :E {}
    :K (EArg ((lambda y y) (lambda z z)) {} Done)}
   {:C ((lambda y y) (lambda z z))
    :E {}
    :K (ECall (lambda x x) {} Done)}
   {:C (lambda y y)
    :E {}
    :K (EArg (lambda z z) {} (ECall (lambda x x) {} Done))}
   {:C (lambda z z)
    :E {}
    :K (ECall (lambda y y) {} (ECall (lambda x x) {} Done))}
   {:C y
    :E {y ((lambda z z) {})}
    :K (ECall (lambda x x) {} Done)}
   {:C (lambda z z)
    :E {}
    :K (ECall (lambda x x) {} Done)}
   {:C x
    :E {x ((lambda z z) {})}
    :K Done}
   {:C (lambda z z)
    :E {}
    :K Done}])
