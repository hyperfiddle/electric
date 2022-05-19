(ns user.demo-healthcheck
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-healthcheck)))        ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Healthcheck"))

    (dom/p (dom/span (dom/text "millisecond time: "))
           (dom/span (dom/text dom/time)))

    (let [x (dom/button
              (dom/text "click me")
              (dom/set-attribute! dom/parent "type" "button")
              (new (->> (dom/events dom/parent "click")
                        (m/eduction (map (constantly 1)))
                        (m/reductions +))))]

      (dom/div
        (dom/table
          (dom/thead
            (dom/td (dom/style {"width" "5em"}) (dom/text "count"))
            (dom/td (dom/style {"width" "10em"}) (dom/text "type")))
          (dom/tbody
            (dom/tr
              (dom/td (dom/text (str x)))
              (dom/td (dom/text (if (odd? x)
                                  ~@(pr-str (type x))
                                  (pr-str (type x))))))))))))

(def main #?(:cljs (p/client (p/main
                               (binding [dom/parent (dom/by-id "root")]
                                 (try
                                   (App.)
                                   (catch Pending _)))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main)))
  )
