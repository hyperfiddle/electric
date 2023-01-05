(ns dustin.y2023.pending-frame-nil-bug2
  #?(:cljs (:require-macros dustin.y2023.pending-frame-nil-bug2))
  (:import [hyperfiddle.photon Pending])
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-ui3 :as ui3]
    [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
    [missionary.core :as m]))

#?(:clj (defn slow-inc [x] (Thread/sleep 100) (inc x)))

(def !db (atom 1))

(p/defn Slow-tx! [x] #_(p/wrap) (swap! !db slow-inc))

(p/defn Demo []
  (p/server
    (let [v (p/watch !db)]
      (p/client
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (Slow-tx!. v')))))
        (ui3/long! v (p/fn [v'] (println 'b v') (p/server (when true (Slow-tx!. v')))))))))
