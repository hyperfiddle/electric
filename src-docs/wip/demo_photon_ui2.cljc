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
    (dom/element "style" "input:invalid {background-color: #f88}")
    (dom/div {:style {:display "flex" :flex-flow "row wrap" :width "100rem" :gap "20px"}}
      (snip "ui/select"  (ui/select [{:text ""} {:text "a" :value :a} {:text "b" :value :b}] :a))
      (snip "ui/long"    (ui/long 0))
      (snip "ui/double"  (ui/double 1.123 {:step 0.001}))
      (snip "ui/keyword" (ui/keyword :foo))
      (snip "ui/symbol"  (ui/symbol 'foo))
      (snip "ui/uuid"    (ui/uuid #uuid "652af367-0262-4fc4-b0c8-f705142c26dd"))
      (snip "ui/edn"     (ui/edn {:hello "world"}))
      (snip "ui/date"    (ui/date "2022-11-30")))
    nil ;; upstream would interpret result of ui2/select
    ))
