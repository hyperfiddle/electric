(ns user.photon-1
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(tests "hello world"
  (def dispose (p/run (rcf/! ::x)))
  % := ::x
  (dispose))

(tests "react based on a ref"
  (def !x (atom 0))
  (p/def X (m/watch !x))
  (def dispose (p/run (! (new X))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests "dataflow diamond"
  (def !x (atom 0))
  (p/def X (m/watch !x))
  (def dispose
    (p/run
      (let [x (new X)]
        (! (+ x x)))))
  % := 0
  (swap! !x inc)
  % := 2
  (swap! !x inc)
  % := 4
  (dispose))

(tests "broken dataflow diamond (no sharing)"
  (def !x (atom 0))
  (p/def X (m/watch !x))
  (def dispose (p/run (! (+ (new X) (new X)))))
  % := 0
  (swap! !x inc)
  % := 1                                                    ; glitch
  % := 2
  (swap! !x inc)
  % := 3                                                    ; glitch
  % := 4
  (dispose))

(tests "object sugar"
  (def !x (atom 0))
  (p/def X (m/watch !x))
  (def dispose (p/run (! (X.))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests "reactive function call"
  (def !x (atom 1)) (p/def X (m/watch !x))
  (def !f (atom +)) (p/def F (m/watch !f))
  (def dispose
    (p/run
      (let [f (F.)
            x (X.)]
        (! (f 0 x)))))
  % := 1
  (swap! !x inc)
  % := 2
  (reset! !f -)
  % := -2
  (dispose))

(tests
  "foreign clojure functions including core. map is not incremental, the args are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (def dispose
    (p/run
      (! (let [f  (new (m/watch !f))
               xs (new (m/watch !xs))]
           (clojure.core/map f xs)))))
  % := [2 3 4]
  (swap! !xs conj 4)
  % := [2 3 4 5]
  (reset! !f dec)
  % := [0 1 2 3])

; m/observe
;(defn watch [!x] (m/observe (fn [!] (add-watch !x ::k !) #(remove-watch !x ::k))))

; define a photon function
; call it

(defn hook [x !]
  (m/observe (fn [send!]
               (! 'mount)
               (send! x)
               #(! 'unmount))))

(p/defn Foo [x !]
  (new (hook x !)))

(tests "object lifecycle"
  (def !x (atom 0))
  (def dispose
    (p/run (!
             (let [x (new (m/watch !x))]
               (if (even? x)
                 (Foo. x !))))))
  % := 'mount
  % := 0
  (swap! !x inc)
  % := 'unmount
  % := nil
  (swap! !x inc)
  % := 'mount
  % := 2
  (dispose)
  % := 'unmount)
