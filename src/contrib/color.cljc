(ns contrib.color
  ;; We could drop the dependency by porting HSLuv to cljc. Mechanical translation. ~2 days of work incl. tests.
  #?(:clj (:import (org.hsluv HUSLColorConverter))
     :cljs (:require ["./hsluv" :refer [Hsluv]])))

(def PHI 0.618033988749895)  ; Golden ratio
(def SEED-ANGLE (/ 125 360)) ; Rotate the chromatic circle like a pointer knob. 1/2 means half a turn.

(defn hsluv->rgb [h s l]
  (mapv #(int (* 255 %))
     #?(:clj (HUSLColorConverter/hsluvToRgb (double-array [h s l]))
        :cljs (let [conv (new Hsluv)]
                (set! (.-hsluv_h conv) h)
                (set! (.-hsluv_s conv) s)
                (set! (.-hsluv_l conv) l)
                (.hsluvToRgb conv)
                [(.-rgb_r conv)
                 (.-rgb_g conv)
                 (.-rgb_b conv)]))))

(defn css-rgb-str [r g b] (str "rgb("r","g","b")"))

(defn color "Hash a value into an harmonious color. Contrast is consistent. Plays well with text and colored backgrounds.
  https://www.hsluv.org/"
  ([x] (color x SEED-ANGLE 50 #_70 85))
  ([x seed-angle saturation lightness]
   (apply css-rgb-str
     (if (nil? x)
       (hsluv->rgb 0 0 80)
       (hsluv->rgb (* 360 (mod (+ seed-angle (* (hash x) PHI)) 1)) ; Hue
         saturation lightness)))))
