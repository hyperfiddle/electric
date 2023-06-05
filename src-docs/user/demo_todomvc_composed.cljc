(ns user.demo-todomvc-composed
  (:require
    #?(:clj [datomic.api :as d])
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [hyperfiddle.electric-ui4 :as ui]
    [user.demo-todomvc :as todomvc]))

(def !n #?(:clj (atom 1)))

(e/defn PopoverCascaded [i F]
  (let [!focused (atom false) focused (e/watch !focused)]
    (dom/div (dom/props {:style {:position "absolute"
                                 :width "50vw"
                                 :left (str (* i 40) "px")
                                 :top (str (-> i (* 40) (+ 60)) "px")
                                 :z-index (+ i (if focused 1000 0))}})
      (dom/on "mouseenter" (e/fn [_] (reset! !focused true)))
      (dom/on "mouseleave" (e/fn [_] (reset! !focused false)))
      (F.))))

(e/defn TodoMVC-composed []
  (e/client
    (let [state (e/watch todomvc/!state)
          n (e/server (e/watch !n))]
      (e/server
        (binding [todomvc/db (e/watch todomvc/!db)
                  todomvc/transact! (partial d/transact todomvc/!conn)]
          (e/client
            (dom/link (dom/props {:rel :stylesheet, :href "/todomvc.css"}))
            (ui/range n (e/fn [v] (e/server (reset! !n v)))
              (dom/props {:min 1 :max 25 :step 1}))
            (dom/div (dom/props {:class "todomvc" :style {:position "relative"}})
              (dom/h1 (dom/text "TodoMVC"))

              (e/for [i (range n)] ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                (PopoverCascaded. i
                  (e/fn [] (todomvc/TodoMVC-UI. state)))))))))))
