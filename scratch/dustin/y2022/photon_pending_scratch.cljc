(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending]
           [clojure.lang ExceptionInfo])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(tests
  (p/run
    (try
      (p/server
        (let [foo 1]
          (! foo)
          (! (p/client foo))))
      (catch Throwable e
        (! e))))
  (type %) := Pending
  % := 1
  (type %) := Pending
  % := 1)

(p/def foo)
(tests
  (with (p/run
          (try
            (p/server
              (binding [foo 1]
                (! foo)
                (! (p/client foo))))
            (catch Throwable e
              (! e))))
    (type %) := Pending
    % := 1
    ;(type %) := ExceptionInfo ; unbound var
    (type %) := Pending ; binding transfer
    % := 1
    ))
