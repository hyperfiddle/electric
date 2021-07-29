(ns hyperfiddle.client.photon
  (:require [hyperfiddle.photon-dom :as dom]
            [hfdl.lang :as photon]))

(def body (.-body js/document))

(photon/run
  (photon/binding [dom/parent body]
    (dom/element :div)))