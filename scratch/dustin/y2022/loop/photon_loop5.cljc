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