(ns dustin.y2022.loop.photon-loop3
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-loop3)))

(defn c->f [c] {:post [(doto % println)]} (+ (* c (/ 9 5)) 32))
(defn f->c [f] {:post [(doto % println)]} (* (- f 32) (/ 5 9)))

(p/defn App1 []
  (let [!x (atom 0) x (p/watch !x)]
    (reset! !x (f->c (ui/input (c->f x))))
    (reset! !x (ui/input x))
    x))

; fix f  =  f (fix f)
; is isomorphic to
; Y g  =  g (Y g)

(p/defn Y! [Gen]
  (new (p/fn [!F]
         (reset! !F (new Gen (p/fn Rec [x] (new (p/watch !F) x))))) ; assignment statement
       (atom ::undefined)))

(p/def Factorial-gen (p/fn [Rec]
                       (p/fn [n]
                         (if (zero? n)
                           1
                           (* n (new Rec (dec n)))))))

(tests
  (def !n (atom 5))
  (with (p/run (tap (new
                      (Y!. Factorial-gen)
                      (p/watch !n))))
    % := 120))

(comment
  (defn fix [f]
    (let [x (f x)]
      x)))

(p/defn Y! [Gen]
  (new (p/fn [!F]
         (reset! !F (new Gen (p/fn Rec [x] (new (p/watch !F) x))))) ; assignment statement
       (atom ::undefined)))

(defn c->f [c] {:post [(doto % println)]} (+ (* c (/ 9 5)) 32))
(defn f->c [f] {:post [(doto % println)]} (* (- f 32) (/ 5 9)))

(p/defn Y! [Gen]
  (new (p/fn [!Loop]
         (let [Loop (Gen. (p/fn Rec [x]
                            ; construct only one Loop frame that is reused!
                            (new (p/watch !Loop) x)))]
           (reset! !Loop Loop)))
       (atom ::undefined)))

(p/defn App2 []
  (new (Y!. (p/fn Gen [Rec]
              (p/fn Loop [x]
                (Rec. (f->c (ui/input (c->f x))))
                #_(Rec. (ui/input x)))))
       "0"))

(p/defn Demo []
  #_(App1.)
  (App2.)
  #_(dom/pre (new (Y!. (p/fn [Rec]
                         (p/fn [n]
                           (if (zero? n)
                             1
                             (* n (new Rec (dec n)))))))
                  5)))

(p/defn Mfix [F]

  )

(let [x (new (Y!. (p/fn [Rec]
                    (p/fn [x]
                      (Rec. (f->c (ui/input (c->f x))))
                      (Rec. (ui/input x))
                      {:c x
                       :f x})))
             0)]
  x)

(let [x (new (Y!. (p/fn [Rec]
                    (p/fn [x]
                      (Rec. (f->c (ui/input (c->f x))))
                      (Rec. (ui/input x))
                      [:db/add x])))
             0)]
  x)

(loop [x 0]
  (recur (f->c (ui/input (c->f x))))
  (recur (ui/input x))
  [:db/add x])
