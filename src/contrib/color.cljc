(ns contrib.color
  (:require [contrib.hsluv :as hsluv]))

(def PHI 0.618033988749895)  ; Golden ratio
(def SEED-ANGLE (/ 125 360)) ; Rotate the chromatic circle like a pointer knob. 1/2 means half a turn.

(defn hsluv->rgb [h s l]
  (mapv #(int (* 255 %)) (hsluv/hsluv->rgb [h s l])))

(defn css-rgb-str [r g b] (str "rgb("r","g","b")"))

(defn color "Hash a value into an harmonious color. Contrast is consistent. Plays well with text and colored backgrounds.
  https://www.hsluv.org/"
  ([x] (color x SEED-ANGLE 50 70 #_85))
  ([x seed-angle saturation lightness]
   (apply css-rgb-str
     (if (nil? x)
       (hsluv->rgb 0 0 80)
       (hsluv->rgb (* 360 (mod (+ seed-angle (* (hash x) PHI)) 1)) ; Hue
         saturation lightness)))))
