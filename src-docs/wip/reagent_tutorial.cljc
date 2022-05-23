(ns wip.reagent-tutorial
  "wip, untested. Reagent tutorial translated to Photon from https://reagent-project.github.io/"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))


; Example 1 — Static component

(p/defn Simple-component []
  (dom/div
    (dom/p (dom/text "I am a component"))
    (dom/p
      (dom/attribute "class" "someclass")
      (dom/text "I have") (dom/strong "bold")
      (dom/span (dom/style {:color "red"}) (dom/text " and red "))
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
    (dom/for-by identity [item items]
      (dom/li (dom/text "Item ") item))))

(p/defn Lister-user []
  (dom/div
    (dom/text "Here is a list:")
    (Lister. (range 3))))

; Example 5 — Basic reactivity

(p/defn Click-counter []
  (dom/input
    (dom/attribute "type" "button")
    (dom/attribute "value" "Click me!")
    (->> (dom/events dom/parent dom/click-event)
         #_(m/reductions (fn [r event] (inc r)) 0)          ; no cc/fn in Photon yet
         (m/eduction (map (constantly 1)))
         (m/reductions + 0)
         (m/relieve {})
         new)))

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
  (dom/input
    (dom/attribute "type" "text")
    (dom/attribute "value" state)                           ; todo need better controlled input OOTB
    (->> (dom/events dom/parent dom/input-event)
         (m/eduction (map dom/target-value))
         (m/relieve {})
         new)))

(p/defn Shared-state []
  (let [!state (atom "foo")]
    (dom/div
      (dom/p (dom/text (str "The value is now: " (p/watch !state))))
      (dom/p (dom/text "Change it here: ") (->> (ControlledInput. state)
                                                (reset! !state))))))

; Example 9 — BMI calculator

(p/defn slider [value min max]
  (dom/input
    (dom/attribute "type" "range")
    (dom/attribute "value" value)
    (dom/attribute "min" min)
    (dom/attribute "max" max)
    (dom/style {:width "100%"})
    (->> (dom/events dom/parent dom/input-event)
         (m/eduction (map dom/target-value))
         (m/relieve {})
         new)))

(p/defn bmi-component []
  (dom/div
    (let [height (Slider. 170 100 220)]
      ; todo
      (p/rec [weight (Slider. (* (or bmi 50) height height) 30 150)
              bmi (Slider. (/ (or weight 70) (* height height)) 10 50)]
             (dom/p (dom/text "BMI: ")
                    (dom/text bmi))))))

; Example 10 — Clock

(defn system-time-ms [_] (js/Date.now))

(def <time (->> (m/observe (fn mount [send!]
                             (let [k (js/setInterval (partial send! 1) 1000)]
                               (fn unmount [] (js/clearInterval k)))))
                (m/latest system-time-ms)))

(p/def color (p/watch color))

(p/defn Clock [color]
  (let [time-str (-> (new <time) .toTimeString (clojure.string/split " ") first)]
    (dom/div
      (dom/class "example-clock")
      (dom/style {"color" color})
      time-str)))

(p/defn ColorInput []
  (dom/div
    (dom/class "color-input")
    (dom/text "Time color: ")
    (dom/input
      (dom/attribute "type" "text")
      (dom/attribute "value" "#f34")                        ; initial value
      (->> (dom/events dom/parent dom/input-event)
           (m/eduction (map dom/target-value))
           (m/relieve {})))))

(p/defn ClockExample []
  (let [!color (atom nil)
        color (p/watch !color)]
    (dom/div
      (dom/h1 (dom/text "Hello world, it is now"))
      (Clock. color)
      (->> (ColorInput.)
           (reset! !color)))))

; Example 11 — TodoMVC
