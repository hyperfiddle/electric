(ns user.demo-todomvc-composed
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.photon-dom :as dom]
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
            (ui/input {::dom/type "range" ::dom/min 1 ::dom/max 25 ::dom/step 1 ::ui/value n
                       ::ui/input-event (p/fn [e] (p/server (reset! !n (p/client (.. e -target -value)))))})
            (dom/div {:class "todomvc" :style {:position "relative"}}
              (dom/h1 "TodoMVC")
              (p/for [i (range n)]
                (let [!focused (atom false)
                      focused (p/watch !focused)]
                  (ui/element dom/div {::dom/style {:position "absolute"
                                                    :width "50vw"
                                                    :left (str (* i 40) "px")
                                                    :top (str (-> i (* 40) (+ 60)) "px")
                                                    :z-index (+ i (if focused 1000 0))}
                                       ::ui/mouseenter-event (p/fn [e] (reset! !focused true))
                                       ::ui/mouseleave-event (p/fn [e] (reset! !focused false))}
                    (todomvc/TodoApp. state)))))))))))
