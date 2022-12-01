(ns .
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))

(declare c->f f->c)

(let [t (atom 0) t (p/watch t)]
  (dom/dl
    (dom/dt "Celsius") (dom/dd (as-> (dom/element "input" {:step 0.5 :value t}) event
                                     (reset! t (-> event :target :value js/parseFloat))))
    (dom/dt "Fahrenheit") (dom/dd (as-> (dom/element "input" {:step 0.5 :value (c->f t)}) event
                                        (reset! t (-> event :target :value js/parseFloat f->c)))))
  t)

(p/with-cycle [t 0] ; todo dom element monoid
  (dom/dl
    (dom/dt "Celsius") (dom/dd (as-> (dom/element "input" {:step 0.5 :value t}) event
                                     (-> event :target :value js/parseFloat)))
    (dom/dt "Fahrenheit") (dom/dd (as-> (dom/element "input" {:step 0.5 :value (c->f t)}) event
                                        (-> event :target :value js/parseFloat f->c)))))

; L: this is wrong in theory but okay in practice
; the reset can be simultaneous - not in practice if the user is triggering the reset

(loop [t 0]
  (dom/dl
    (dom/dt "Celsius") (dom/dd (as-> (ui/input {:step 0.5 :value t}) event
                                     (recur (-> event :target :value js/parseFloat))))
    (dom/dt "Fahrenheit") (dom/dd (as-> (ui/input {:step 0.5 :value (c->f t)}) event
                                        (recur (-> event :target :value js/parseFloat f->c)))))
  t)

(loop [t 0]
  (do (as-> (ui/input {:step 0.5 :value t}) event
            (recur (-> event :target :value js/parseFloat)))
      (as-> (ui/input {:step 0.5 :value (c->f t)}) event
            (recur (-> event :target :value js/parseFloat f->c))))
  t)


(loop [a true]
  [(recur a)
   (recur (not a))])
:= [[[[.] [.]] [[.] [.]]] [[[.]] [[.]]]]
;:= [[[[] []] [[] []]] [[[] []] [[] []]]] ; cantor set

(let [a true]
  (let-rec [a (not a)]
           [a a]))

(loop [a true]
  (do (recur a)
      (recur (not a))))

; undefined in clojure, also compilation error due to non-tail recur call

; What if we add the


; Q: What is the computational essence of local state in FRP?
; Hypothesis: this structure matches the structure of loop/recur

(defn foo [a]
  (do (foo true)
      (foo (not true))))

(foo true) := ???


(defn foo1 [n]
  (if (pos? n)
    (foo (dec n))
    n))

(foo1 2) := ???
