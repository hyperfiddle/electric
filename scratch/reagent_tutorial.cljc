(ns wip.reagent-tutorial
  "wip, untested. Reagent tutorial translated to Photon from https://reagent-project.github.io/"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))


; Example 1 — Static component

(p/defn Simple-component []
  (dom/div
    (dom/p (dom/text "I am a component"))
    (dom/p {:class "someclass"}
      (dom/text "I have") (dom/strong "bold")
      (dom/span {:style {:color "red"}} (dom/text " and red "))
      (dom/text "text."))))

; Example 2 — Composition

(p/defn Simple-parent []
  (dom/div
    (dom/p (dom/text "I include simple-component"))
    (Simple-component.)))

; Example 3 — Passing arguments

(p/defn Hello-component [name]
  (dom/p (dom/text "Hello, ") (dom/text name) (dom/text "!")))

(p/defn Say-hello []
  (Hello-component. "world"))

; Example 4 — for

(p/defn Lister [items]
  (dom/ul
    (p/for-by identity [item items]
      (dom/li (dom/text "Item ") item))))

(p/defn Lister-user []
  (dom/div
    (dom/text "Here is a list:")
    (Lister. (range 3))))

; Example 5 — Basic reactivity

(p/defn Click-counter []
  (dom/input {:type  "button"
              :value "Click me!"}
    (dom/events "click" (map (constantly 1)) 0 +)))

(def !click-count (atom 0))

(p/defn Counting-component []
  (dom/div
    (dom/text "The atom ")
    (dom/code (dom/text "click-count"))
    (dom/text " has value: ")
    (dom/text (p/watch !click-count))
    (dom/text ". ")
    ; counter must render at this location, so cannot lift it into higher scope to access
    ; the count signal, therefore use state to "loop" by side effect in React style
    (->> (Click-counter.)
         (reset! !click-count))))

; Example 6 — Effect loop

(p/defn Timer-component-as-effect-loop []
  ; Honor the intent of the Reagent tutorial, which is effect loops
  (let [!count (atom 0)
        count  (p/watch !count)]
    (do count (js/setTimeout (partial swap! !count inc)) nil) ; Reset the timeout through causal dependency on `count`
    (dom/div (dom/text (str "Seconds Elapsed: " count)))))

(p/defn Timer-component []
  ; Idiomatic timer
  (let [>tick (m/observe (fn mount [send!]
                           (let [k (js/setInterval (partial send! 1) 1000)]
                             (fn unmount [] (js/clearInterval k)))))]
    (dom/div
      (dom/text "Seconds Elapsed:")
      (new (->> (m/reductions + 0 >tick) (m/relieve {}))))))

; Example 7 — controlled input

(p/defn ControlledInput [state]
  (dom/input {:type "text"
              :value state}                           ; todo need better controlled input OOTB
             ;; listen to input events, get target value, initial value is `""`
             (dom/events "input" (map (dom/oget :target :value)) "")
             ))

(p/defn Shared-state []
  (let [!state (atom "foo")]
    (dom/div
      (dom/p (dom/text (str "The value is now: " (p/watch !state))))
      (dom/p (dom/text "Change it here: ") (->> (ControlledInput. state)
                                                (reset! !state))))))

; Example 9 — BMI calculator

(p/defn slider [value min max]                              ; controlled
  (dom/input {:type  "range"
              :value value
              :min   min
              :max   max
              :style {:width "100%"}}
             (dom/events "input" (map (dom/oget :target :value)) (p/current value))))

(comment
  ; This doesn't work - no p/rec for a while
  ; there are other ways to do this, todo
  (p/defn bmi-component []
    (dom/div
      (let [height (Slider. 170 100 220)]
        ; Looped interdependent bindings
        (p/rec [weight (Slider. (* (or bmi 50) height height) 30 150)
                bmi (Slider. (/ (or weight 70) (* height height)) 10 50)]
               (dom/p (dom/text "BMI: ")
                      (dom/text bmi)))))))

; Example 10 — Clock

(defn system-time-ms [_] (js/Date.now))

(def <time (->> (m/observe (fn mount [send!]
                             (let [k (js/setInterval (partial send! 1) 1000)]
                               (fn unmount [] (js/clearInterval k)))))
                (m/latest system-time-ms)))

(p/def color (p/watch color))

(p/defn Clock [color]
  (let [time-str (-> (new <time) .toTimeString (clojure.string/split " ") first)]
    (dom/div {:class "example-clock"
              :style {"color" color}}
      time-str)))

(p/defn ColorInput [value]
  (dom/div {:class "color-input"}
    (dom/text "Time color: ")
    (dom/input {:type "text"
                :value value}
               (dom/events "input" (map (dom/oget :target :value)) (p/current value)))))

(p/defn ClockExample []
  (let [!color (atom nil)
        color (p/watch !color)]
    (dom/div
      (dom/h1 (dom/text "Hello world, it is now"))
      (Clock. color)
      (->> (ColorInput. "#f34")
           (reset! !color)))))

; Example 11 — TodoMVC
