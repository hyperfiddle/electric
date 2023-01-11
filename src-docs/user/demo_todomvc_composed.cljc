(ns user.demo-todomvc-composed
  (:require
   #?(:clj [datascript.core :as d])
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui4 :as ui4]
   [user.demo-5-todomvc :as todomvc])
  #?(:cljs (:require-macros user.demo-todomvc-composed)))

(def !n #?(:clj (atom 1)))

(p/defn App []
  (p/client
    (let [state (p/watch todomvc/!state)
          n (p/server (p/watch !n))]
      (p/server
        (binding [todomvc/db (p/watch todomvc/!conn)
                  todomvc/transact! (partial d/transact! todomvc/!conn)]
          (p/client
            (dom/link {:rel :stylesheet, :href "/todomvc.css"})
            (ui4/range n (p/fn [v] (p/server (reset! !n v)))
              (dom2/props {:min 1 :max 25 :step 1}))
            (dom/div {:class "todomvc" :style {:position "relative"}}
              (dom/h1 "TodoMVC")
              (p/for [i (range n)]
                (let [!focused (atom false)
                      focused (p/watch !focused)]
                  (dom2/div (dom2/props {:style {:position "absolute"
                                                 :width "50vw"
                                                 :left (str (* i 40) "px")
                                                 :top (str (-> i (* 40) (+ 60)) "px")
                                                 :z-index (+ i (if focused 1000 0))}})
                    (dom2/on "mouseenter" (p/fn [_] (reset! !focused true)))
                    (dom2/on "mouseleave" (p/fn [_] (reset! !focused false)))
                    (todomvc/TodoApp. state)))))))))))
