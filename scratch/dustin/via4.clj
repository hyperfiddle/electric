(ns dustin.via4)

(declare compile fmap bind extend-seq pure)

(def ast '(let [>control (input)
                >p       (input)
                >q       (input)
                >cross   (bind >control (fn [control]
                                          (fmap (extend-seq identity
                                                  (fmap range (case control :p >p :q >q)))
                                            (fn [as>]
                                              (map (fn [>a] ...) as>)))))]))

; 1, ~ is await aka bind
(compile '(let [a 1
                control ~>control]
            (reactive-for identity [c [1 2 3]
                                    >a ~(range ~(case control :p >p :q >q))]
              ...)))
; 2
(compile '(reactive-for identity [>a ~(range ~(case ~>control :p >p :q >q))]
            ...))

; 3, ~ is ?!
(compile '(reactive-for identity [c [1 2 3]
                                  >a (cp ~(range (case ~>control :p >p :q >q)))]
            ...))

; for could automatically imply cp
(compile '(reactive-for identity [>a (range (case ~>control :p >p :q >q))]
            ...))

; Lessons:
; extend-seq travels with a map and a list callback


:= (bind >control (fn [control]
                    (map (fn [>a] ...)
                      (extend-seq identity
                        (fmap range (case control :p >p :q >q))))))

(let [a  1
      b (pure a)]
  (case (and a b) :p >p :q >q)                              ; :: Incr a
  (pure (+ a b)))

(bind (pure 1) (fn [a]
                 (bind (pure a) (fn [b]
                                  (pure (+ a b))))))

(bind (pure 1) (fn [a]
                 (fmap (pure 2) (fn [b]
                                  (+ a b)))))

(+ ~a ~b) := (fmap + a b)  ; parallel, (only if a and b are RT)
;; Leo's fear: can a macro break the meaning of the DSL by the macro lifting an expression into let
;;(which is allowed / safe in clojure) and our compiler does not give the same meaning between let-expr and
;;inlined-expr this could be counter-intuitive . So does it compose with other macros? Maybe not
; Leo is still looking for an example of broken code here.

(f expr1 expr2)
(let [a expr1 b expr2] (f a b))
; these are same in clojure


(compile (let [a ~x] ...))
:= (bind x (fn [a] ...))

(compile (let [a ~x] (let [a ~x] ...)))
:= (bind x (fn [a] ...))


(def ast2 ' (let [control (for [_ ~(range (case control :p >p :q >q))])]))

(bind (pure 1) (fn [stack #_a]
(bind (pure (get stack 'a)) (fn [stack #_b]
(pure (+ (get stack 'a) (get stack 'b)))))))


(+ ~a ~b)

(reagent [:table [:span (count @xs>)]])
(cp [:table [:span (count (?! xs>))]])
(via [:table [:span (count ~xs>)]])                         ; delimited breaks
(via ~[:table ~[:span ~(count ~xs>)]])                      ; monad one layer at a time
