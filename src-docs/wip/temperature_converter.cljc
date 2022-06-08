(ns wip.temperature-converter
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m])
  #?(:cljs (:require-macros [wip.temperature-converter]))
  (:import (hyperfiddle.photon Pending Remote)))

(comment
  (rcf/enable!))

;; events looks like:
;; - [:db/add e a v]
;; - [:dom.input/focus true]
;; - [:browser/navigate url]
(defn interpret [event-tags f]
  (letfn [(matches? [event] (and (vector? event)
                              ((set event-tags) (first event))))]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (prn "=>" input)
         (if (matches? input)
           (do (f input) result)
           (xf result input)))))))

(tests
  "Unmatched events passes through"
  (sequence (interpret #{:dom.input/focus} (fn [_])) [[:browser/navigate "url"]])
  := '([:browser/navigate "url"]))

(tests
  "xf is called with matched events"
  (def !focus (atom false))
  (sequence (interpret #{:dom.input/focus} (fn [[_ focus?]] (reset! !focus focus?)))
    [[:browser/navigate "url"]
     [:dom.input/focus true]])
  := '([:browser/navigate "url"])
  (deref !focus) := true)

(defn set-switch! [!switch [event-tag open?]]
  (reset! !switch (boolean open?)))

(defn deref' [!atom & _] (deref !atom))

(defn dedupe-n "Like `dedupe` but deduplicates individual values of a sequential collection, position-wise."
  ([]
   (fn [rf]
     (let [pv (volatile! ::init)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv]
            (vreset! pv input)
            (if (= ::init prior)
              (rf result (seq input))
              (let [changed (->> (map vector input prior)
                                 (filter #(not= (first %) (second %)))
                                 (map first))]
                (if (seq changed)
                  (rf result changed)
                  result)))))))))
  ([coll] (sequence (dedupe-n) coll)))

(tests
  (dedupe-n [[1 "2"] [1 "3"] [2 "3"]]) 
  := '((1 "2") ("3") (2))
  )


(defmacro semicontroller ;; thyristor?
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)]
     (->> (p/fn [] (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input)))))
       (m/eduction (dedupe-n) cat (interpret #{~event-tag} (partial set-switch! !switch#)))
       (new))))

(tests

  (def !event (atom nil))
  (def !input (atom 0))
  (with (p/run (semicontroller :switch/set (new (m/watch !input))
                 (p/fn [open? input]
                   (! input)
                   [(p/watch !event)])))
    % := 0
    (swap! !input inc)
    % := 1
    (reset! !event [:switch/set false])
    (swap! !input inc)
    % := ::rcf/timeout
    (rcf/set-timeout! 1200) ;; rcf issue, extend timeout
    (reset! !event [:switch/set true])
    (swap! !input inc)
    % := 3))

(def !state (atom 0))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (dom/div
                     (dom/attribute "id" "main")
                     (let [output (semicontroller :dom.input/set-controlled! (p/watch !state)
                                    (p/fn [value]
                                      (dom/div
                                        (dom/input (dom/attribute "type" "text")
                                          (dom/style {:width "10em"
                                                      :outline "none"})
                                          (dom/property "value" value)
                                          [(new (->> (dom/focus-state dom/parent)
                                                     (m/eduction
                                                       (map not)
                                                       (map (partial conj [:dom.input/set-controlled!])))))
                                           (new (->> (dom/events dom/parent "input")
                                                     (m/eduction (map dom/target-value))
                                                     (m/reductions {} value)
                                                     (m/relieve {})))]))))]
                       (dom/p (dom/text "output:"))
                       (dom/p (dom/text output)))))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))