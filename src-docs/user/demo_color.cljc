(ns user.demo-color
  #?(:cljs (:require-macros user.demo-color))
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [contrib.color :as c]))

;; Goal is to show:
;; - fine grained reactivity on CSS properties
;; - Non trivial DOM api usage (canvas)

(def CANVAS-WIDTH 360)                  ; px
(def CANVAS-HEIGHT 100)                 ; px

(defn format-rgb [[r g b]] (str "rgb("r","g","b")"))

#?(:cljs
   (defn draw! [^js canvas colorf]
     (let [ctx (.getContext canvas "2d")]
       (loop [angle 0]
         (set! (.-strokeStyle ctx) (colorf angle))
         (.beginPath ctx)
         (.moveTo ctx angle 0)
         (.lineTo ctx angle CANVAS-HEIGHT)
         (.closePath ctx)
         (.stroke ctx)
         (when (< angle 360)
           (recur (inc angle)))))))

#?(:cljs
   (defn draw-gradient! [canvas hue colorf]
     (draw! canvas (fn [angle] (format-rgb (if (= angle hue) [255 255 255] (colorf angle)))))))

(defn saturation->chroma [saturation] (* 0.158 (/ saturation 100)))

(p/defn App []
  (p/client
    (let [!lightness  (atom 70)
          lightness   (p/watch !lightness)
          !saturation (atom 80)
          saturation  (p/watch !saturation)
          !hue        (atom 180)
          hue         (p/watch !hue)]
      (dom/div (dom/props {:style {:display               :grid
                                   :grid-template-columns "auto 1fr auto"
                                   :gap                   "0 1rem"
                                   :align-items           :center
                                   :justify-items         :stretch
                                   :max-width             "600px"}})
        (dom/p (dom/text "Lightness"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   100
                               :step  1
                               :value lightness})
          (dom/event "input" (fn [^js e] (reset! !lightness (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text lightness "%"))

        (dom/p (dom/text "Saturation"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   100
                               :step  1
                               :value saturation})
          (dom/event "input" (fn [^js e] (reset! !saturation (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text saturation "%"))


        (dom/p (dom/text "Hue"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   360
                               :step  1
                               :value hue})
          (dom/event "input" (fn [^js e] (reset! !hue (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text hue "°"))


        (dom/p (dom/text "HSL"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node hue (fn [hue] (c/hsl->rgb [hue saturation lightness])))
          )
        (dom/div (dom/props {:style {:background-color (format-rgb (c/hsl->rgb [hue saturation lightness]))
                                     :width            "100px"
                                     :height           "100%"
                                     }}))

        (dom/p (dom/text "OKLCH"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node hue (fn [hue] (c/oklch->rgb [lightness (saturation->chroma saturation) hue]))))
        (dom/div (dom/props {:style {:background-color (format-rgb (c/oklch->rgb [lightness (saturation->chroma saturation) hue]))
                                     :width            "100px"
                                     :height           "100%"}}))

        (dom/p (dom/text "HSLuv"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node hue (fn [hue] (c/hsluv->rgb [hue saturation lightness]))))
        (dom/div (dom/props {:style {:background-color (format-rgb (c/hsluv->rgb [hue saturation lightness]))
                                     :width            "100px"
                                     :height           "100%"}}))
        ))))
