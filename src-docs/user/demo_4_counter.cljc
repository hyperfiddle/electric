(ns user.demo-4-counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-4-counter)))           ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Counter"))
    (let [!counter (atom 0)
          x        (p/watch !counter)]
      (ui/button {::ui/click-event (p/fn [_] (swap! !counter inc))}
                  (dom/text "click me"))
      (dom/div
       (dom/table
        (dom/thead
          (dom/td {:style {"width" "5em"}} (dom/text "count"))
          (dom/td {:style {"width" "10em"}} (dom/text "type")))
        (dom/tbody
         (dom/tr
          (dom/td (dom/text (str x)))
          (dom/td (dom/text (if (odd? x)
                              ~@(pr-str (type x))
                              (pr-str (type x))))))))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/node (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
