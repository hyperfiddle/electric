(ns hyperfiddle.client.examples.reagent1
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.reagent1 :refer [simple-component]])))

(photon/defn simple-component []
  (dom/div
   (dom/p (dom/text "I am a component"))
   (dom/p (dom/attribute "class" "someclass")
          (dom/text "I have ") (dom/strong (dom/text "bold"))
          (dom/span (dom/style {:color "red"}) (dom/text " and red "))
          (dom/text "text."))))

(def root #?(:cljs (js/document.getElementById "root")))

(defn reagent1 []
  (photon/run
    (photon/binding [dom/parent (dom/by-id "reagent1")]
      (photon/$ simple-component))))

#?(:cljs (reagent1))
