(ns dustin.y2023.pending-frame-nil-bug1
  #?(:cljs (:require-macros dustin.y2023.pending-when-impulse-bug1))
  (:import [hyperfiddle.photon Pending])
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-ui3 :as ui3]
    [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
    [missionary.core :as m]))


(def !db (atom 1))

(p/defn Demo [] ; both 1. and 2. are needed to reproduce
  (p/server ; necessary
    (p/client
      (ui3/long! 11 (p/fn [v'] (println 'a v')))
      (ui3/long! 11 (p/fn [v'] (println 'a v') (p/server (new (p/fn [] v'))))))))


(p/defn Demo1 []
  ; 100% working (no nil)
  (p/client
    (let [v (p/server (p/watch !db))]
      (dom/pre (dom/text (pr-str v)))
      (ui3/long! v (p/fn [v'] (println 'a v') (p/server (reset! !db v'))))
      (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (reset! !db v'))))) ; when and v' are connected
      (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (swap! !db inc))))))))

(p/defn Demo2 [] ; both 1. and 2. are needed to reproduce
  (p/server
    (let [v (p/watch !db)] ; 1. let on server, body on client
      (p/client
        (dom/pre (dom/text (pr-str v)))
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (reset! !db v'))))
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (reset! !db (new (p/fn [] v')))))) ; new frame
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (new (p/fn [] (reset! !db v')))))) ; 2. new frame containing v'
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (reset! !db v'))))) ; 2. when and v' are connected
        (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (swap! !db inc)))))))))

;(p/defn Demo []
;  (Demo1.)
;  (Demo2.)
;  (Demo3.))

(comment
  #?(:clj (defn slow-inc [x] (Thread/sleep 100) (inc x)))
  (p/defn Slow-tx! [x] #_(p/wrap) (swap! !db slow-inc))
  (p/defn Demo-busted []
    (p/server
      (let [v (p/watch !db)]
        (p/client
          (dom/pre (dom/text (pr-str v)))
          (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (swap! !db inc)))))
          (ui3/long! v (p/fn [v'] (println 'a v') (p/server (when true (reset! !db v')))))
          (ui3/long! v (p/fn [v'] (println 'b v') (p/server (when true (Slow-tx!. v'))))))))))
