(ns dustin.y2022.scratch
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.scratch))
  (:import [hyperfiddle.photon Pending]))


(defn slow-identity [x] (Thread/sleep 1) x)

(tests
  (with (p/run
          (try
            (when (p/for [x (range 2)]
                    (p/wrap (slow-identity x))
                    (tap nil))
              (tap ::finished))
            (catch Pending _)))
        % := nil
        % := nil
        % := ::finished))