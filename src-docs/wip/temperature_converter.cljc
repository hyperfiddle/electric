(ns wip.temperature-converter
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
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
  := '((1 "2") ("3") (2)))

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

#_(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (dom/div
                     (dom/attribute "id" "main")
                     (let [{:keys [input/value]}
                           (semicontroller :dom.input/set-controlled! (p/watch !state)
                             (p/fn [value]
                               (dom/div
                                 (dom/input (dom/attribute "type" "text")
                                   [::noop (dom/style {:width "10em"
                                                       :outline "none"})]
                                   [::noop (dom/property "value" value)]
                                   [:dom.input/set-controlled! (not (new (dom/focus-state dom/parent)))]
                                   [:input/value (new (->> (dom/events dom/parent "input")
                                                        (m/eduction (map dom/target-value))
                                                        (m/reductions {} value)
                                                        (m/relieve {})))]))))]
                       (dom/p (dom/text "output:"))
                       (dom/p (dom/text value)))))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))

(comment
  (let [bus (p/for [e [1 2 3]]
              (interpret
                [:dom/div
                 [:dom/on-click]
                 [:dom/input
                  [:dom/on-input]
                  [:dom/on-focus]]
                 [:dom/text "hello"]]))]
    (interpret [:dom/text (apply str (q bus :dom.input/value))])))

(comment
  (def !click-count (atom 0))

  (p/defn Counting-component []
    (dom/div
      (dom/text "The atom ")
      (dom/code "click-count")
      (dom/text " has value: ")
      (dom/text (p/watch !click-count))
      (dom/text ". ")
    ; counter must render at this location, so cannot lift it into higher scope to access
    ; the count signal, therefore use state to "loop" by side effect in React style
      (->> (Click-counter.)
        (reset! !click-count))))

  (p/defn Click-counter []
    (dom/input
      (dom/attribute "type" "button")
      (dom/attribute "value" "Click me!")
      (->> (dom/events dom/parent dom/click-event)
        #_(m/reductions (fn [r event] (inc r)) 0)          ; no cc/fn in Photon yet
        (m/eduction (map (constantly 1)))
        (m/reductions + 0)
        (m/relieve {})
        new))))

(comment
  (p/defn Counting-component []
    (interpret
      [:div
       [:text "The atom"]
       [:code [:text "click-count"]]
       [:text " has value: "]
       (let [bus (Click-counter.)]
         [[:text (q bus :click/count)]
          (if (odd? x)
            [:text "odd"]
            [:text "even"])
          [:text ". "]
          (q bus :input)])]))

  (p/defn Click-counter []
    (interpret #{:dom/on-click} (fn [state event] [(inc state) [:click/count (inc state)]]) 0
      [:input
       [:dom.attribute/type "button"]
       [:dom.attribute/value "Click me!"]
       [:dom/on-click]])))

(comment
  (dom/div
    (dom/attribute "id" "main")
    (let [{:keys [input/value]}
          (semicontroller :dom.input/set-controlled! (p/watch !state)
            (p/fn [value]
              (dom/div
                (dom/input (dom/attribute "type" "text")
                  (dom/style {:width "10em"
                              :outline "none"})
                  (dom/property "value" value)
                  :dom.input/set-controlled! (not (new (dom/focus-state dom/parent)))
                  :input/value (new (->> (dom/events "input")
                                      (m/eduction (map dom/target-value))
                                      (m/reductions {} value)
                                      (m/relieve {})))))))]
      (dom/p (dom/text "output:"))
      (dom/p (dom/text value)))))

#_(defmacro semicontroller ;; thyristor?
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)
         el# (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input))))]
     (when-let [event# (get el# ~event-tag)]
       (set-switch! event#))
     el#))

(comment
  (p/defn Counting-component []
    (let [el (Click-counter. (p/watch !label))]
      (dom/div
        (dom/text "count: ")
        (dom/text (signal el ::n))
        el)))

  (p/defn Click-counter [label]
    (dom/div
      (dom/span (dom/text label))
      (dom/input (dom/attribute "type" "button"))
      ::n (->> (dom/events "click" dom/time)
               (m/reductions (fn [r _] (inc r)))
               (new)))))

(defn reduce-inc [r _] (inc r))

(p/defn Click-counter []
  (dom/div
    (dom/span (dom/text "Counter"))
    (dom/input (dom/props {:type :button
                           :value "Click me!"}))
    {::n (->> (dom/events "click")
              (m/reductions reduce-inc 0)
              (m/relieve {})
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
                   (Counting-component.)
                   )
                 (catch Pending _)
                 (catch Remote _))))))