(ns hyperfiddle.missionary-test
  (:require [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests tap % with]])
  (:import (missionary Cancelled)))


(tests
  "flow cancel before transfer"
  (def !x (atom 0))
  (def >x (m/watch !x))
  (def !it (>x (fn [] (tap ::notify))
               (fn [] (tap ::terminate))))
  % := ::notify
  (!it)
  @!it thrown? Cancelled
  % := ::terminate)

(tests
  "pentagram of death - via Kenny Tilton"
  (def !aa (atom 1))
  (def !a7 (atom 7))
  (with
    ((m/reactor
       (let [<aa  (m/signal! (m/watch !aa))
             <a7  (m/signal! (m/watch !a7))
             <a70 (m/signal! (m/latest (partial * 10) <a7))
             <bb  (m/signal! <aa)
             <cc  (m/signal! (m/latest (partial * 10) <aa))
             <dd  (m/signal! (m/cp
                               (try
                                 (if (even? (m/?< <bb))
                                   (* 10 (m/?< <cc)) 42)
                                 (catch Cancelled _))))
             <ee  (m/signal! (m/latest + <a70 <bb (m/latest (partial * 10000) <dd)))]
         (m/stream!
           (m/ap
             (m/amb=
               (tap {'aa (m/?< <aa)})
               (tap {'a7 (m/?< <a7)})
               (tap {'a70 (m/?< <a70)})
               (tap {'bb (m/?< <bb)})
               (tap {'cc (m/?< <cc)})
               (tap {'dd (m/?< <dd)})
               (tap {'ee (m/?< <ee)}))))))
     tap tap)
    % := {'ee 420071}
    % := {'dd 42}
    % := {'cc 10}
    % := {'bb 1}
    % := {'a70 70}
    % := {'a7 7}
    % := {'aa 1}

    (swap! !aa inc)
    % := {'aa 2}
    % := {'bb 2}
    % := {'cc 20}
    % := {'dd 200}
    % := {'ee 2000072}

    (swap! !aa inc)
    % := {'aa 3}
    % := {'bb 3}
    % := {'cc 30}
    % := {'dd 42}
    % := {'ee 420073}))
