(ns wip.dom-button
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.dom-button)))

(def !x #?(:clj (atom 0)))

(defmacro button [& body]
  `(new (let [step (dom/button
                     (dom/text "click me")
                     (dom/attribute "type" "button")
                     (new (->> (dom/events dom/parent "click")
                               (m/eduction (map (constantly 1)))
                               (m/reductions +)
                               (m/relieve {}))))]
          (z/do-step step ~@body))))

(p/defn App []
  (let [x ~@(p/watch !x)]
    (dom/div
      (dom/h1 (dom/text "Toggle Server"))

      (let [step (dom/button
                   (dom/text "click me")
                   (dom/attribute "type" "button")
                   (new (->> (dom/events dom/parent "click")
                             (m/eduction (map (constantly 1)))
                             (m/reductions +)
                             (m/relieve {})))

                   #_(new (->> (m/ap (let [e (dom/events dom/parent "click")]
                                       ~@(swap! !x inc)))
                               (m/relieve {})))

                   #_(->> (dom/events dom/parent "click")
                          (z/impulse x)))]

        (button (partial println ::clicked))
        #_(button (partial swap! !x inc))
        ;
        ;
        ;(do-step step (println ::clicked))
        ;~@(do-step step (swap! !x inc))
        ;
        ;(new (foreach-tick (p/fn [] step)
        ;                   (partial println ::clicked)))
        ;~@(new (foreach-tick (p/fn [] step)
        ;                     (partial swap! !x inc)))
        ;
        ;(when step
        ;  (println ::clicked)
        ;  ~@(swap! !x inc))
        )

      (dom/div (dom/text (if (odd? x)
                           ~@(pr-str "Server")
                           (pr-str "Client")))))))

(def main #?(:cljs (p/client (p/main
                               (binding [dom/parent (dom/by-id "root")]
                                 (try
                                   (App.)
                                   (catch Pending _)))))))

(comment
  (user/browser-main! `main)
  )

; Problem: How to transfer a click count to remote peer
; foreach click count, we want to perform an effect
; "click count" not just "click" because CT
; if something is skipped, you can still see it with a diff

