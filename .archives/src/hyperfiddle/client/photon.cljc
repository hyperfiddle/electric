(ns hyperfiddle.client.photon
  (:require [hyperfiddle.photon-dom :as dom]
            [hfdl.lang :as photon]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.client.photon :refer [simple-component simple-parent
                                                               hello-component say-hello
                                                               lister lister-user
                                                               click-counter counting-component
                                                               timer-component
                                                               shared-state input]])))

(def body #?(:cljs (.-body js/document)))

(photon/defn simple-component []
  (dom/div
   (dom/p (dom/text "I am a component"))
   (dom/p (dom/attribute "class" "someclass")
          (dom/text "I have ") (dom/strong (dom/text "bold"))
          (dom/span (dom/style {:color "red"}) (dom/text " and red "))
          (dom/text "text."))))

(photon/defn simple-parent []
  (dom/div
   (dom/p (dom/text "I include simple-component"))
   (photon/$ simple-component)))

(photon/defn hello-component [name]
  (dom/p (dom/text "Hello, ")
         (dom/text name)
         (dom/text "!")))

(photon/defn say-hello []
  (photon/$ hello-component "world"))

(photon/defn lister [items]
  (dom/ul
   (photon/for [item items]
     (dom/li (dom/text "Item ") (dom/text item)))))

(photon/defn lister-user []
  (dom/div
   (dom/text "Here is a list:")
   (photon/$ lister (range 3))))

;; Example 5

(def click-reducer (fn [r _] (inc r)))

(photon/defn click-counter []
  ~(->> (dom/element :input
                     (dom/attribute "type" "button")
                     (dom/attribute "value" "Click me!")
                     (dom/events dom/parent dom/click-event))
        (m/reductions click-reducer 0)))

(photon/defn counting-component []
  (let [!ref (atom 0)]
    (dom/div
     (dom/text "The atom ")
     (dom/code "click-count")
     (dom/text " has value: ")
     (dom/text ~(m/watch !ref))
     (dom/text ". ")
     (reset! !ref (photon/$ click-counter)))))

;; Example 6

(def second-elapsed
  (m/ap (loop [x 0]
          (m/? (m/sleep 1000))
          (m/amb=
           x
           (recur (inc x))))))

(photon/defn timer-component []
  (dom/div (dom/text "Seconds Elapsed:") (dom/text ~second-elapsed)))

;; Example 7

(photon/defn input [val]
  (dom/input (dom/set-attribute! dom/parent "type" "text")
             (dom/set-attribute! dom/parent "value" val)
             (->> ~(m/relieve {} (dom/events dom/parent dom/input-event))
                  dom/log
                  dom/event-target
                  dom/get-value)))

(photon/defn shared-state []
  (let [!val (atom "foo")]
    (dom/div
     (dom/p (dom/text "The value is now: ") (dom/text ~(m/watch !val)))
     (dom/p (dom/text "Change it here: ")  (reset! !val (photon/$ input ~(m/watch !val)))))))

;; EXAMPLE 8 (entrypoint)

(def main
  (photon/local1
    (photon/binding [dom/parent body]
      (photon/$ shared-state))))
