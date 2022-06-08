(ns wip.click-counter
  (:require [hyperfiddle.photon-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.photon :as p])
  (:import (hyperfiddle.photon Pending Remote)))

(defn reduce-inc [r x] (if (some? x) (inc r) r))

(p/defn Click-counter []
  (dom/div
    (dom/span (dom/text "Counter"))
    (dom/input (dom/props {:type :button
                           :value "Click me!"}))
    {::n (->> (dom/events "click")
              (m/reductions reduce-inc 0)
              (new))}))

(p/defn Counting-component []
  (let [el (Click-counter.)]
    (dom/div
      (dom/text "count: ")
      (dom/text (::n (new (dom/signals el))))
      el)))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (dom/root (dom/by-id "root")
                   (Counting-component.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))