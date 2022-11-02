(ns user.tic-tac-toe
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.tic-tac-toe)))

(def !x #?(:clj (atom (vec (repeat 10 0))) :cljs nil))
(p/def x (p/server (p/watch !x)))
(defn update-board! [board pos] (update board pos #(case % 0 1, 1 2, 2 0)))

(p/defn button [offset]
  (ui/button {::ui/click-event (p/fn [_] (p/server (swap! !x update-board! offset)))}
     (p/server (case (nth x offset) 2 "x" 1 "o" 0 "-"))))

(p/defn App []
  (p/client
    (dom/h1 "Tic Tac Toe \uD83C\uDFAE")
    (dom/table
      (p/for [row [[0 1 2]
                   [3 4 5]
                   [6 7 8]]]
        (dom/tr
          (p/for [i row]
            (dom/td
              (button. i))))))))
