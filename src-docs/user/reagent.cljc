;;;; Reagent examples translated to Photon
;;;; https://reagent-project.github.io/

(ns user.reagent
  (:require [hyperfiddle.photon :as r]
            [hyperfiddle.photon-dom :as d]
            [missionary.core :as m]
            [reagent.core :as reagent]))

;;; Example 1 — Static component

;; Reagent
(defn simple-component []
  [:div
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

;; Photon
(r/defn simple-component []
  (dom/div
   (dom/p (dom/text "I am a component"))
   (dom/p (dom/attribute "class" "someclass")
        (dom/text "I have") (dom/strong "bold")
        (dom/span (dom/style {:color "red"}) (dom/text " and red "))
        (dom/text "text."))))

;; Questions about example 1
;;
;; - Props
;;   - [ ] should props be a flow?
;;     - [ ] if yes, could they nest?
;; - Text
;;   - [ ] should we use `(dom/text "foo")`
;;   - [ ] or should we support text litterals? (dom/div "foo")

;;; Example 2 — Composition

;; Reagent
(defn simple-parent []
  [:div
   [:p "I include simple-component."]
   [simple-component]])

;; Photon
(r/defn simple-parent []
  (dom/div
   (dom/p (dom/text "I include simple-component"))
   (r/$ simple-component)))

;;; Example 3 — Passing arguments

;; Reagent
(defn hello-component [name]
  [:p "Hello, " name "!"])

(defn say-hello []
  [hello-component "world"])

;; Photon
(r/defn hello-component [name]
  (dom/p (dom/text "Hello, ")
       (dom/text name)
       (dom/text "!")))

(r/defn say-hello []
  (r/$ hello-component "world"))

;;; Example 4 — for

;; Reagent
(defn lister [items]
  [:ul
   (for [item items]
     ^{:key item} [:li "Item " item])])

(defn lister-user []
  [:div
   "Here is a list:"
   [lister (range 3)]])

;; Photon

(r/defn lister [items]
  (dom/ul
   (r/for [item items]                            ; TODO ^:key
     (dom/li (dom/text "Item ") item))))

(r/defn lister-user []
  (dom/div
   (dom/text "Here is a list:")
   (r/$ lister (range 3))))

;;; Example 5 — Basic reactivity

;; Reagent

(def click-count (reagent/atom 0))

(defn counting-component []
  [:div
   "The atom " [:code "click-count"] " has value: "
   @click-count ". "
   [:input {:type "button" :value "Click me!"
            :on-click #(swap! click-count inc)}]])

;; Photon

(def click-count (atom 0))

(r/defn click-counter []
  ~(->> (dom/element :input
                   (dom/attribute "type" "button")
                   (dom/attribute "value" "Click me!")
                   (dom/events dom/parent dom/click-event))
        (m/reductions (fn [r _] (inc r)) 0)))

(r/defn counting-component []
  (dom/div
   (dom/text "The atom ")
   (dom/code "click-count")
   (dom/text " has value: ")
   (dom/text ~(m/watch click-count))
   (dom/text ". ")
   (reset! click-count (r/$ click-counter))))

;;; Example 6 — Effect loop

;; Reagent
(defn timer-component []
  (let [seconds-elapsed (r/atom 0)]
    (fn []
      (js/setTimeout #(swap! seconds-elapsed inc) 1000)
      [:div
       "Seconds Elapsed: " @seconds-elapsed])))

;; Photon

(def second-elapsed
  (m/ap (loop [x 0]
          (m/? (m/sleep 1000))
          (m/amb=
           x
           (recur (inc x))))))

(r/defn timer-component []
  (dom/div "Seconds Elapsed:" ~second-elapsed))

;; NOTE This is a different paradigm
;; NOTE these examples are not good enough to show the added value.

;;; Example 7 — Passing flows to children

;; Reagent
(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn shared-state []
  (let [val (r/atom "foo")]
    (fn []
      [:div
       [:p "The value is now: " @val]
       [:p "Change it here: " [atom-input val]]])))

;; Photon
(r/defn atom-input [val]
  (dom/input {:type "text"} ~value
           (->> ~(m/relieve {} (events dom/parent input-event))
                dom/event-target
                dom/get-value
                (reset! val))))

(r/defn shared-state []
  (let [val (atom "foo")]
    (dom/div
     (dom/p (dom/text "The value is now: ") (dom/text ~(m/watch val)))
     (dom/p (dom/text "Change it here: ")   (atom-input val)))))


;;; Example 8 — Mounting entrypoint

;; Reagent
(defn hello []
  [:p "Hello"])

(defn render-hello []
  (rdom/render
   [hello]
   (.-body js/document)))

;; Photon

(r/def hello []
  (dom/p (dom/text "Hello")))

(defn render-hello []
  (r/run
    (r/binding [d/parent (.-body js/document)]
      (r/$ hello))))

;;; Example 9 — BMI calculator

(r/defn slider [value min max]
  (d/input (d/attribute "type" "range")
           (d/attribute "value" value)
           (d/attribute "min" min)
           (d/attribute "max" max)
           (d/style {:width "100%"})
           (->> ~(m/relieve {} (events dom/parent input-event))
                dom/event-target
                dom/get-value)))

(r/defn bmi-component []
  (d/div
   (let [height (r/$ slider 170 100 220)]
     (r/letrec [weight (r/$ slider (* bmi height height)        30 150)
                bmi    (r/$ slider (/ weight (* height height)) 10 50)]
        (d/p (d/text "BMI: ")
             (d/text bmi))))))

;;; Example 10 — Clock

;;; Example 11 — TodoMVC
