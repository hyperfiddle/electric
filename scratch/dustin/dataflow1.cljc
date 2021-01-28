(ns dustin.dataflow1)

(def x 1)

(defn mf [a]
  (m/reactor (inc (?! (pure a))))
  (pure (inc a)))

(inc
  (let [a >x
        b (mf a)
        c (inc b)]
    (+ a b c)))
:= 6

; Clojure's let is the identity monad
(fmap inc
  (bind-id x (fn [a]
               (m/reactor
                 (if (odd? a)
                   nil
                   (bind-id (inc a) (fn [b]
                                      (m/reactor
                                        (bind-id (inc b) (fn [c]
                                                           (+ a b c)))))))))))
:= 6

(defn f [a]
  (get (m/reactor
         (bind-id (inc a) g))
    '>a))                             ; (inc a) -> (g %)
(defn g [b] (bind-id (inc b) h))
(defn h [c] (+ a b c))

(fmap inc (bind-id x f))                                    ; :: a -> m a
:= 6

x -> f

(->> >x
  (fmap inc)
  (fmap dec)
  (fmap inc))

'(inc (dec (inc >x)))                                       ; a -> b -> c -> ...
