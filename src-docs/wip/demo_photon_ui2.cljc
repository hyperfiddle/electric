(ns wip.demo-photon-ui2
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros wip.demo-photon-ui2)))

(p/defn ValueLog [v] (let [!log (atom []), log (p/watch !log)] (swap! !log conj v) (dom/div (str log))))

(defmacro snip [header elem]
  `(dom/div {:style {:width "200px"}}
     (dom/h2 (dom/code ~header))
     (ValueLog. ~elem)))

(p/defn App []
  (p/client
    (dom/h1 "Photon UI 2")
    (dom/div {:style {:display "flex" :flex-flow "row wrap" :width "100rem" :gap "20px"}}
      (snip "ui/select"  (ui/select [{:text ""} {:text "a"} {:text "b"}] "a"))
      (snip "ui/long"    (ui/long 0))
      (snip "ui/double"  (ui/double 1.123))
      (snip "ui/keyword" (ui/keyword :foo)))
    nil ;; upstream would interpret result of ui2/select
    ))
