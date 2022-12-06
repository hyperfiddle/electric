(ns dustin.y2022.loop.photon-loop5
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-loop5)))

(hyperfiddle.rcf/enable!)

(p/def Fac*)
(p/defn Fac [n]
  (binding [Fac* (p/fn [n]
                   (if (zero? n)
                     1
                     (* n (Fac*. (dec n)))))]
    (Fac*. n)))

(tests (with (p/run (tap (Fac. 5))) % := 120 % := ::rcf/timeout))  ; ✅

(p/defn Fac2 [n]
  (if (zero? n)
    1
    (* n (recur (dec n))))) ; iterative but non tail call, undefined in clojure. Today Photon allocates frames

(tests (with (p/run (tap (Fac2. 5))) % := 120)) ; ✅

(p/defn Fac3 [n]
  (loop [n n]
    (if (zero? n)
      1
      (* n (recur (dec n)))))) ; today Photon allocates frames

(tests (with (p/run (tap (Fac3. 5))) % := 120)) ; ✅ ; wtf

(p/defn Fac4 [n]
  (let [!n (atom n) n (p/watch !n)]
    (if (zero? n)
      1
      (* n (reset! !n (dec n)))))) ; UB

(tests (with (p/run (tap (Fac4. 5))) % := 120)) ; ❌ (not (= 20 120))
; 20 12 6 2 0 1

(p/run
  (let [!x (atom 0) x (p/watch !x)]
    (reset! !x (ui/input x))
    x))

(p/run
  (loop [x 0]
    (recur (identity x))
    x))

(p/run
  (new (p/fn Rec [x]
         (if (> x 0)
           (Rec. (dec x))
           x))
       0))

; Is it possible to implement cyclic continuous flow in missionary without assignment?
; Dustin: No, because m is embedded in Clojure and Clojure is strict

(defn fix [f] (let [x (f x)] x))
; implement this in the CP monad without assignment

; Missionary's key insight is that you can unify discrete and continuous time
; under the same propogation algorithm
; Dustin says this may not be possible under continuous time, because CT propogation
; includes cycles and value recursion, which DT and acyclic propagation need not consider