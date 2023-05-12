(ns wip.button-with-progress
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [missionary.core :as m]))

(e/defn Download! [!progress round]
  (reset! !progress (* 20 round))
  (when (< round 5)
    (let [ms (+ 100 (rand-int 500))]
      (case (new (e/task->cp (m/sleep ms ms)))
        (recur !progress (inc round))))))

(e/defn App []
  (let [!progress (atom 0), progress (e/watch !progress)]
    (e/client
      (dom/div (dom/style {:display "flex", :align-items "center", :gap "1rem"})
        (ui/button (e/fn [] (e/server (new Download! !progress 0)))
          (dom/text "Download"))
        (dom/div (dom/style {:border "1px solid gray", :border-radius "0.4rem"
                              :overflow "hidden", :width "5rem", :height "1rem"})
          (dom/div (dom/style {:width (str progress "%"), :height "100%", :background-color "green"})))))))
