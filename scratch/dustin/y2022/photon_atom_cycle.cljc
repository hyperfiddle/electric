(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(tests
  (with (p/run (let [!F (atom (p/fn [] ::init)), F (p/watch !F)]
                 (do
                   (let [x (! (new F))]
                     #_(let [y (do x 0)])
                     (reset! !F (p/fn [] x))))))
    % := ::init
    % := 0))

(comment
  (let-rec [F (p/fn [] (or x ::init))
            x (! (new F))]))

(tests
  (def <x (m/cp (let [!F (atom (m/cp ::init)), F (m/watch !F)]
                  (let [<x (m/?< F)]
                    (reset! !F (m/cp (! (m/?< <x))))))))
  (def it (<x #(println ::notify) #(println ::terminate)))
  @it
  % := ::init
  @it
  % := 0
  (it))


(comment
  (def <x (m/cp (let [!F (atom (m/cp ::init)), F (m/watch !F)]
                  (let [<x (m/?< F)]
                    (reset! !F (m/cp (! (m/?< <x))))))))
  ;=> #'dustin.scratch/<x
  (def it (<x #(println ::notify) #(println ::terminate)))
  :dustin.scratch/notify
  ;=> #'dustin.scratch/it
  @it
  :dustin.scratch/notify
  ;=> #object[clojure.core$partial$fn__5908 0x1899f70e "clojure.core$partial$fn__5908@1899f70e"]
  @it
  :dustin.scratch/notify
  :dustin.scratch/notify
  :dustin.scratch/notify
  :dustin.scratch/notify
  :dustin.scratch/notify
  )

(tests
  (def dispose ((m/reactor
                  (m/stream!
                    (let [!F (atom (m/cp ::init))]
                      (let [<<x (m/signal! (m/watch !F))
                            <x (m/signal! (m/cp (! (m/?< (m/?< <<x)))))]
                        (reset! !F (m/signal! (m/cp (m/?< <x))))))))
                println println))
  % := ::init
  % := 0
  (dispose))