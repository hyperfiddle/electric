(ns dustin.y2022.loop.photon-loop4
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-loop4)))


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

(p/defn App2 []
  (new
    (let [!Loop (atom ::undefined)]
      (reset! !Loop
              (p/fn Loop [x]
                (new (p/watch !Loop)
                     (f->c (ui/input (c->f x)))))))
    "0"))

(p/defn App2 []
  (new
    (p/fn Loop [x]
      (new Loop (f->c (ui/input (c->f x)))))
    "0"))

(p/defn App2 []
  (new
    (p/fn Loop [x]
      (recur (f->c (ui/input (c->f x)))))
    "0"))

(p/defn with-cycle [init Body]
  (let [!x (atom init)]
    (reset! !x (Body. (p/watch !x)))))

(defmacro with-cycle [[s i] & body]
  `(let [a# (atom ~i) ~s (p/watch a#)]
     (reset! a# (do ~@body))))

; ok you're right, your reduced with-cycle  recur by p/fn has the non-tail-call ability
; and the macro is circular, it depends on the recur syntax which eliminates the need for the macro

(defmacro loop2 [[s v] & body]
  `(new (p/fn Loop [~'s] ~@body) ~v))

(loop2 [x "0"]
  (do (recur (f->c (ui/input (c->f x))))
      x))


; I want Y to let me do this without the recur syntax, but i don't know how to make it
; reuse the frame without needing the syntax

(p/defn Y! [Gen]
  (new (p/fn [!Loop]
         (let [Loop (Gen. (p/fn Rec [x]
                            (new (p/watch !Loop) x)))]
           (reset! !Loop Loop)))
       (atom ::undefined)))

(p/defn App2 []
  (new (Y!. (p/fn Gen [Rec]
              (p/fn Loop [x]
                (Rec. (f->c (ui/input (c->f x))))
                #_(Rec. (ui/input x)))))
       "0"))


(p/defn Y! [Gen]
  (new (p/fn [!Loop]
         (let [Loop (Gen. (p/fn Rec [X]
                            (new (p/watch !Loop) x)))]
           (reset! !Loop Loop)))
       (atom ::undefined)))

(p/defn App2 []
  (new (Y!. (p/fn Gen [Rec]
              (p/fn Loop [x]
                (Rec. (f->c (ui/input (c->f x))))
                #_(Rec. (ui/input x)))))
       "0"))