(ns user.demo-counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-counter)))             ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Counter"))

    (let [x (dom/button {:type "button"}
              (dom/text "click me")
              (new (dom/events "click" (map (constantly 1)) 0 +)))]
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
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
