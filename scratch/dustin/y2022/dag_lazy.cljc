(ns dustin.scratch
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(comment
  (def i (m/sleep 1000 42))
  '(- (inc i) (dec i))

  (def x (m/sleep 1000 6))
  (def y (m/sleep 1000 7))

  (p/defn third [a b c] c)

  '(let [b [x y]]
     (concat
       [(inc x) b]
       [b (dec y)]))

  ; 1
  (m/sp
    (let [b (m/? (m/join vector x y))]                      ; introduces causality/ordering that is not present in the DAG
      (m/? (m/join concat
                   (m/sp (m/join vector (m/sp (inc (m/? x))) b))
                   (m/sp (m/join vector b (m/sp (dec (m/? y)))))))))

  ; 2
  (m/sp
    (let [>b (future! (m/? (m/join vector x y)))]            ; allocate buffer, don't eval
      (m/? (m/join concat
                   (m/sp (m/join vector (m/sp (inc (m/? x))) >b))
                   (m/sp (m/join vector >b (m/sp (dec (m/? y)))))))))

  ; 3
  (def x (m/sp 6))
  (def y (m/sp 7))

  ;; not clojure, just a symbolic representation of the dag
  (quote
    (let [b [x y]]
      (concat [(inc x) b] [b (dec y)])))

  (defn node [n f & args] (m/sp (m/? (m/sleep n (m/? (apply m/join f args))))))
  (defn a [x] (node 1000 inc x))
  (defn b [x y] (node 1 vector x y))
  (defn c [x] (node 1 dec x))
  (defn d [x y] (node 1 vector x y))
  (defn e [x y] (node 1000 vector x y))
  (defn f [x y] (node 1 concat x y))

  (def dag
    (m/sp
      (let [>b (future! (b x y))]
        (m/? (f (d (a x) (join >b))
                (e (join >b) (c y)))))))

  (time (m/? dag)) := (7 [6 7] [6 7] 6)                            ;; runs in 1 second

  )

(comment
  "Dustin's idea"

  ; 3
  (defn fapply [f ma mb & mxs])
  (defn fmap [f mx])
  (defn bind [f mx])

  ; if 3 is late, it prevents 1 and 2
  (lazy
    (fapply (fn [%1 %2 %3]
              (fapply mconcat
                      (fapply mvector (pure %1) (pure %2))
                      (fapply mvector (pure %2) (pure %3))))
            (fmap inc x) b (fmap dec y)))

  (lazy
    (mlet [[%1 %2 %3] <- (fapply (fmap inc x) b (fmap dec y))
           [_ _] (fapply mconcat )
           _ <- (fapply mvector (pure %1) (pure %2))
           _ <- (fapply mvector (pure %2) (pure %3))

           ]

          )
    )
  )