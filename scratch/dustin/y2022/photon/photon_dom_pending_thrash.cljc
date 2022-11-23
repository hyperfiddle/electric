(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(defn observer [tap x]
  (fn mount [f]
    (tap [::mount x])
    (f nil)
    (fn unmount [] (tap [::unmount x]))))

;(tests
;  "transacting db does not thrash the app dom when pending"
;  (def !db (atom 0))
;  (def !y (atom false))
;
;  (p/defn MyDiv [x tap]
;    (new (m/observe (observer x tap))))
;
;  (p/defn App [db]
;    (MyDiv. db !)
;    (when (p/watch !y) (inc db)))
;
;  (with (p/run (try
;                 (let [tx' (App. (p/watch !db))]
;                   (when tx'
;                     (reset! !db tx')))))
;    % := [::mount 0]
;    (reset! !y true)
;    ; % := [:down 0]
;    % := ::rcf/timeout))