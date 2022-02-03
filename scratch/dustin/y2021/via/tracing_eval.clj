(ns dustin.tracing-eval
  (:require
    [hyperfiddle.via :refer [via Do-via !]]
    [meander.epsilon :as m :refer [match]]
    [minitest :refer [tests]]))


(comment

  (deftype Clj []
    Do-via
    (resolver-for [_]
      {'let (fn [bindings body]
              (eval `(let ~bindings ~'a)))}))

  (via (tracing-eval)
    (mlet [$ >$]
      (shirt-sizes $ ~(:gender ~(submissions $ ~needle)))))

  (deftype tracing-eval []
    Do-via
    (resolver-for [H]
      {:Free.bind   (fn [mv mf] (doto `(mf ~mv) println clojure.core/eval))
       :Free.fapply (fn [af & avs] ...)
       :Free.fmap   (fn [f & fv] ...)
       :Free.pure   (fn [v] ...)
       }))

  (via (tracing-eval)
    (inc (inc (inc 1))))
  )

(defrecord tracing-eval []
  Do-via
  (resolver-for [_]
    {:F.inc (fn [a] (println a) (inc a))}
    #_(fn [special-form]
        (println 'special-form special-form)
        (case special-form
          ;let (fn [bindings body] (eval `(let ~bindings ~'a)))
          (fn [f & args]
            (println 'f f 'args args)
            (apply f args))))))

(tests
  (via (->tracing-eval)
    (! :F.inc (! :F.inc (! :F.inc 0))))
  #_(via (->tracing-eval)
      (inc (inc (inc 0))))


  )
