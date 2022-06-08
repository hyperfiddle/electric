(ns wip.semicontroller
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            [hyperfiddle.logger :as log])
  #?(:cljs (:require-macros [wip.semicontroller :refer [interpreter semicontroller]]))
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

;; TODO defeated by z/instant?
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
  := '((1 "2") ("3") (2)))

(defn check-interpretable! [x]
  (if-not (vector? x)
    (do (log/error "Semicontroller expects a vector of events. Received:" (pr-str x))
      nil)
    x))

(defmacro interpreter [event-tags f & body]
  `(->> (p/fn [] ~@body)
        (m/eduction (dedupe-n) cat (interpret ~event-tags ~f))
        (m/reductions {} nil)
        (m/relieve {})
        (new)))

(defmacro semicontroller
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)]
     (->> (p/fn [] (check-interpretable! (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input))))))
          (m/eduction (dedupe-n) cat (interpret #{~event-tag} (partial set-switch! !switch#)))
          (m/reductions {} nil)
          (new))))

(tests
  (def !event (atom nil))
  (def !input (atom 0))
  (with (p/run (semicontroller :switch/set (new (m/watch !input))
                 (p/fn [input]
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

;; TODO move to photon test suite
#_(tests
  (defn boom! [x] (throw (ex-info "boom" {})) x)
  (with (p/run (! (try (new (m/eduction (map boom!) (p/fn [] 1)))
                    (catch Throwable t
                      :boom))))
    % := :boom ;; fails with ::rcf/timeout
    ))