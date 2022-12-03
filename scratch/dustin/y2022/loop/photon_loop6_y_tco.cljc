(ns dustin.y2022.loop.photon-loop6-y-tco
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-loop6-y-tco)))

(hyperfiddle.rcf/enable!)

(def !x (atom "0"))

(p/defn Y-iterative [Gen]
  (new (p/fn [!Loop]
         (let [Loop (Gen. (fn rec [x']
                            (println 'rec x')
                            ; don't reconstruct Loop but call it again in same frame
                            (reset! !x x')))]
           (reset! !Loop Loop)))
       (atom ::undefined)))

(p/defn App2 []
  (new (Y-iterative.
         (p/fn Gen [rec]
           (p/fn Loop [x]
             (println 'Loop x)
             (new (m/observe (fn [!] (println 'ctor) (! nil) #(println 'dtor))))
             (rec (ui/input x)))))
       (p/watch !x)))

#?(:clj
   (tests
     (with (p/run (tap (App3.)))
       % := "0"
       (send! "1")
       % := "1"
       (send! "2")
       % := "2"
       )))


;(p/defn Y-iterative [Gen]
;  (new (p/fn [!Loop]
;         (let [Loop (Gen. (fn rec [>x]
;                            (m/latest @!Loop >x)))] ; cycle without
;           (reset! !Loop Loop)))
;       (atom ::undefined)))
;
;#?(:clj
;   (p/defn App3 []
;     (new (Y-iterative.
;            (p/fn Gen [rec]
;              (p/fn Loop [x]
;                (println 'Loop x)
;                (let [x' (new (m/observe (fn [!] (println 'construct x) (def send! !) #())))]
;                  (println 'x' x')
;                  (rec (p/fn [] x'))))))
;          "0")))
;#?(:clj
;   (tests
;     (with (p/run (App3.))
;       (send! "1")
;       )))

;(p/defn App2 []
;  (dom/h1 "yo")
;  (new (Y!. (p/fn Gen [rec]
;              (p/fn Loop [x]
;                (println 'Loop x)
;                (let [x' (new (m/observe (fn [!]
;                                           (println 'construct x)
;                                           (def go !)
;                                           #())))]
;                  (println 'x' x')
;                  (let [q (rec (p/fn [] x'))]
;                    (println 'q q)
;                    (doto (new q) (println 'after)))))))
;       "0"))

;;(p/defn App2 []
;;  (dom/h1 "yo")
;;  (new (Y!. (p/fn Gen [rec]
;;              (p/fn Loop [t]
;;                (let [a (ui/input t)
;;                      b (f->c (ui/input (c->f t)))]
;;                  (rec (p/fn [] x))))))
;;       "0"))


;(p/defn App2 []
;  (dom/h1 "yo")
;  (new (Y!. (p/fn Gen [rec]
;              (p/fn Loop [x]
;                (let [x' (f->c (ui/input (c->f x)))]
;                  (rec (p/fn [] x'))))))
;       "0"))