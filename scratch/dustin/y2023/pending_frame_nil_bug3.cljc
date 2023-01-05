(ns dustin.y2023.pending-frame-nil-bug3
  #?(:cljs (:require-macros dustin.y2023.pending-frame-nil-bug3))
  (:import [hyperfiddle.photon Pending])
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-ui3 :as ui3]))


(def !db (atom 1))

(p/defn Demo [] ; both 1. and 2. are needed to reproduce
  (p/server ; necessary
    (p/client
      (ui3/long! 11 (p/fn [v'] (println 'a v')))
      (ui3/long! 11 (p/fn [v'] (println 'a v') (p/server (new (p/fn [] v'))))))))
