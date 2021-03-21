(ns dustin.compiler4
  (:require [minitest :refer [tests]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalize AST, implicit fmap, no bind, unify branches ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn normalize
  "Expands an all points in a given form

  `(normalize '{plus (lambda a b (+ a b))} '(plus 1 2))`
  := `(+ 1 2)`
  "
  ([registry form]
   (normalize registry form {}))
  ([registry form env]
   (if (seq? form)
     (let [[f & args] form]
       (if-some [[_ & args-body] (registry f)]
         (let [syms (butlast args-body)]
           (as-> (last args-body) body
             (normalize registry body (merge env (zipmap syms args)))
             (normalize registry body env)))
         (cons 'fmap (cons f (map #(normalize registry % env) args)))))
     (get env form form))))
​
​
(tests
 (normalize
  '{five  (lambda 5)
    plus  (lambda-applicative a b (+ a b))
    times (lambda-bind a b (m/reactor (if (odd? a) (* a b) (/ a b))))
    func  (lambda a b (plus a (times b b)))}
  '(func >input3 (plus >input1 >input2)))

 :=1 '((lambda a b (plus a (times b b))) >input3 ((lambda a b (+ a b)) >input1 >input2))
 :=2 '((plus >input3 (times ((lambda a b (+ a b)) >input1 >input2) ((lambda a b (+ a b)) >input1 >input2))) >input3)
 :=3 '(((lambda a b (+ a b)) >input3 ((lambda a b (* a b)) ((lambda a b (+ a b)) >input1 >input2) ((lambda a b (+ a b)) >input1 >input2))) >input3)
 :=4 '(((+ >input3 ((lambda a b (* a b)) ((+ >input1 >input2)) ((+ >input1 >input2))))) >input3)
 :=5 '(((+ >input3 ((lambda a b (* a b)) ((+ >input1 >input2)) ((+ >input1 >input2))))) >input3)

 := '(+ >input3 (* (+ >input1 >input2)
                   (+ >input1 >input2)))                    ; this can be optimized into one node

 := '(func >input3 (plus >input1 >input2))
 )


"too much dynamism"

'(func >input3 (plus >input1 >input2))
'(mlet [:let [x (+ >input1 >input2)]]
   (+ >input3 (* x x)))

'(>+ >input3 (if (odd? x) (>* >x >x) (>** >x >x)))
'(>+ >input3 ((fn [x] ...) >x))



(def conjv (fnil conj []))

(defn parenting [acc parent child]
  (-> acc
      (assoc-in [child :parent] parent)
      (update-in [parent :children] conjv child)))

(defn analyze-form [nodes form !env]
  (let [idx (count nodes)]
    (swap! !env assoc form idx)
    (if (coll? form)
      (let [[f & args] form]
        ;; fmap
        (reduce (fn [nodes form]
                  (if-let [child-index (get @!env form)]
                    (parenting nodes idx child-index)
                    (parenting (analyze-form nodes form !env) idx (count nodes))))
                (conj nodes
                      {:type 'fmap
                       :form form
                       :f    f})
                args)
        ;; TODO bind
        )
      (if (symbol? form)
        (conj nodes {:type 'user
                     :form form})
        (throw (ex-info "Unknown form." {:form form}))))))

(defn analyze [form]
  (analyze-form [] form (atom {})))

(tests
 (analyze '(+ >input3 (* (+ >input1 >input2) (+ >input1 >input2))))
 :=
 '[{:type fmap, :form (+ >input3 (* (+ >input1 >input2) (+ >input1 >input2))), :f +, :children [1 2]}
   {:type user, :form >input3, :parent 0}
   {:type fmap, :form (* (+ >input1 >input2) (+ >input1 >input2)), :f *, :children [3 3], :parent 0}
   {:type fmap, :form (+ >input1 >input2), :f +, :children [4 5], :parent 2}
   {:type user, :form >input1, :parent 3}
   {:type user, :form >input2, :parent 3}])
