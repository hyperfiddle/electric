(ns wip.demo-photon-ui2
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui2])
  #?(:cljs (:require-macros wip.demo-photon-ui2)))

(p/defn App []
  (p/client
    (dom/h1 "Photon UI 2")
    (ui2/select [{:text "" :selected true} {:text "a"} {:text "b"}])
    nil ;; upstream would interpret result of ui2/select
    ))
