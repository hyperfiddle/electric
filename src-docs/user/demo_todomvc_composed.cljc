(ns user.demo-todomvc-composed
  #?(:cljs (:require-macros user.demo-todomvc-composed))
  (:require
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as p]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [user.demo-5-todomvc :as todomvc]))

(def !n #?(:clj (atom 1)))

(p/defn App []
  (p/client
    (let [state (p/watch todomvc/!state)
          n (p/server (p/watch !n))]
      (p/server
        (binding [todomvc/db (p/watch todomvc/!conn)
                  todomvc/transact! (partial d/transact! todomvc/!conn)]
          (p/client
            (dom/link (dom/props {:rel :stylesheet, :href "/todomvc.css"}))
            (ui/range n (p/fn [v] (p/server (reset! !n v)))
              (dom/props {:min 1 :max 25 :step 1}))
            (dom/div (dom/props {:class "todomvc" :style {:position "relative"}})
              (dom/h1 (dom/text "TodoMVC"))
              (p/for [i (range n)]
                (let [!focused (atom false)
                      focused (p/watch !focused)]
                  (dom/div (dom/props {:style {:position "absolute"
                                                 :width "50vw"
                                                 :left (str (* i 40) "px")
                                                 :top (str (-> i (* 40) (+ 60)) "px")
                                                 :z-index (+ i (if focused 1000 0))}})
                    (dom/on "mouseenter" (p/fn [_] (reset! !focused true)))
                    (dom/on "mouseleave" (p/fn [_] (reset! !focused false)))
                    (todomvc/TodoApp. state)))))))))))
