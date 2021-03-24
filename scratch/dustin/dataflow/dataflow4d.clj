(ns dustin.dataflow4d
  (:require [missionary.core :as m]))


(def !inputs (atom '(1 2 3)))
(def !factor (atom 2))

(def imap
  (ifn [f xs]
    (when-some [[x & xs] (seq xs)]
      (cons ($ f x) ($ imap f xs)))))

(run-dag
  ($ imap
    (ifn [x] (* x @(m/watch !factor)))
    @(m/watch !inputs)))


(defn extend-seq [kf >xs]
  )

(def imap2
  ; this is an ifn
  (fn [kf f> >as]
    (let [>>bs (map f> (extend-seq kf >as))]
      (m/signal! (apply m/latest join >bs)))))


'($ f (dec n))
:= '(let [>a (m/signal! (m/latest dec >n))]
      (m/signal! (m/cp (m/?! (f> >a)))))


(defn unsequence [kf >xs]                                   ; :: Flow[List[A]] -> List[Flow[A]]
  ; use kf to stabilize this m/signal!
  ; inside dynamic multiplicity of the traversal
  (let [xs (m/? >xs)]
    (map (fn [x] (m/signal! #_kf (m/cp x))) xs)))

(def imap2
  ; this is an ifn
  (fn [kf f> >as]
    (let [bs> (map f> (unsequence kf >as))]
      (m/signal! (apply m/latest seq bs>)))))


; again
(def imap2
  (fn [kf f> >as]
    (let [bs> (map f>
                (map (fn [x] (m/signal! #_kf (m/cp x)))
                  (m/? >xs)))]
      (m/signal! (apply m/latest seq bs>)))))

; again
(def imap2
  (fn [kf f> >as]
    (let [bs> (map (fn [x]
                     ; use kf to stabilize this m/signal!
                     ; inside dynamic multiplicity of the traversal
                     (m/signal! #_kf (f> (m/cp x))))
                (m/? >as))]
      (m/signal! (apply m/latest seq bs>)))))




(defn ^:ifn imap [f> >as]
  (m/signal! nil
    (m/cp
      (let [bs> (map (fn [x]
                       (f> (m/cp x)))
                  (m/?! >as))]
        (m/?! (apply m/latest seq bs>))))))

(run-dag
  ($ imap
    (ifn identity [x] (* x @(m/watch !factor)))
    @(m/watch !inputs)))