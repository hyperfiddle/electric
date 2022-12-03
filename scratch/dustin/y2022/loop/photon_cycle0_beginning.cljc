(ns dustin.y2022.loop.photon-cycle0-beginning
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-cycle0-beginning)))

(tests
  (with (p/run (let [!F (atom (p/fn [] ::init))]
                 (let [F (p/watch !F)
                       x (! (new F))]
                   (reset! !F (p/fn [] x)))))
    % := ::init
    % := 0))
;✅❌ FAIL in () (photon_atom_cycle.cljc:7)
;expected: (= % 0)
;  actual: (not (= :hyperfiddle.rcf/timeout 0))


; Same bug
(tests
  (with (p/run (let [!F (atom (p/fn [] ::init)), F (p/watch !F)]
                 (! (new F))
                 (let [y 0]
                   (reset! !F (p/fn [] y)))))
    % := ::init
    % := 0))
;✅❌ FAIL in () (photon_atom_cycle.cljc:7)
;expected: (= % 0)
;actual: (not (= :hyperfiddle.rcf/timeout 0))


(tests
  (def !main (m/reactor
              (m/stream!
                (let [!F (atom (m/cp ::init))]
                  (let [<<x (m/signal! (m/watch !F))
                        <x (m/signal! (m/cp (! (m/?< (m/?< <<x)))))]
                    (m/latest {} <x (m/latest (partial reset! !F) (m/cp <x))))))))
  (def it (!main #(! [::success %]) #(! [::failure %])))
  % := ::init
  % := 0
  (it))
;✅❌ FAIL in () (photon_atom_cycle.cljc:32)
;expected: (= % 0)
;  actual: (not (= [:dustin.scratch/failure #error {
; :cause "Subscription failure : self subscription."
; :via
; [{:type java.lang.Error
;   :message "Subscription failure : self subscription."
;   :at [missionary.impl.Reactor subscribe "Reactor.java" 326]}] ...

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