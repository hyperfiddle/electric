(ns wip.demo-photon-ui2
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui2])
  #?(:cljs (:require-macros wip.demo-photon-ui2)))

(p/defn ValueLog [v] (let [!log (atom []), log (p/watch !log)] (swap! !log conj v) (dom/div (str log))))

(p/defn App []
  (p/client
    (dom/h1 "Photon UI 2")
    (ValueLog. (ui2/select [{:text ""} {:text "a"} {:text "b"}] "a"))
    nil ;; upstream would interpret result of ui2/select
    ))
