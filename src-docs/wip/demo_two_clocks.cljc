(ns wip.demo-two-clocks
  "network stress test"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-two-clocks)))          ; forces shadow hot reload to also reload JVM at the same time

(defn clock []
  (->> (m/ap
         (loop []
           (m/amb (m/? (m/sleep 10 1))                      ; deadlock in m/sleep causes this demo to freeze, fixme
                  (recur))))
       (m/reductions {} nil)
       (m/latest (fn [_]
                     #?(:clj  (System/currentTimeMillis)
                        :cljs (js/Date.now))))))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Two Clocks"))

    (let [c (new (clock))
          s ~@(new (clock))]

      (dom/p (dom/span (dom/text "client time: "))
             (dom/span (dom/text c)))

      (dom/p (dom/span (dom/text "server time: "))
             (dom/span (dom/text s)))

      (dom/p (dom/span (dom/text "latency: "))
             (dom/span (dom/text (- c s)))))))

(def main #?(:cljs (p/client (p/main
                               (binding [dom/parent (dom/by-id "root")]
                                 (try
                                   (App.)
                                   (catch Pending _)))))))

(comment
  ; List in user.cljc for cljs compiler
  #?(:clj (user/browser-main! `main))
  )
