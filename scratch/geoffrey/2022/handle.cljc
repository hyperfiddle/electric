(ns geoffrey.handle
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests with tap %]])
  (:import [hyperfiddle.photon Pending]))


(p/def handlers {})
(p/defn Perform [ident values]
  (when-let [Handle (get handlers ident)]
    (Handle. values)))

(tests
  (with (p/run (tap (binding [handlers {::action (p/fn [[a b]] [a b])}]
                      (Perform. ::action [1 2])))))
  % := [1 2])


(defmacro with-handlers [handlers & body]
  `(binding [handlers (merge handlers ~handlers)]
     ~@body))

(tests
  (with (p/run (tap (with-handlers {::action (p/fn [[a b]] [a b])}
                      (Perform. ::action [1 2])))))
  % := [1 2])


(p/defn Input []
  (try (Perform. ::value "hello world")
       (tap "settled")
       (catch Pending _
         (tap "pending"))))

(tests
  (with (p/run (tap (with-handlers {::typed (p/fn [s] (tap ["typed" s]) (throw (Pending.)))}
                      (with-handlers {::value (p/fn [s] (Perform. ::typed s))}
                        (Input.))))))
  % := ["typed" "hello world"]
  % := "pending"
  % := "settled" ; FIXME not passing TODO simulate Pending state for one frame.
  )
