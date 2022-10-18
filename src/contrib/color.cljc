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

(defn ^:deprecated hsv->rgb [h s v]
  (let [h-i     (int (* h 6))
        f       (- (* h 6) h-i)
        p       (* v (- 1 s))
        q       (* v ( - 1 (* f s)))
        t       (* v (- 1 (* (- 1 f) s)))
        [r g b] (case h-i
                  0 [v t p]
                  1 [q v p]
                  2 [p v t]
                  3 [p q v]
                  4 [t p v]
                  5 [v p q])]
    [(int (* r 256))
     (int (* g 256))
     (int (* b 256))]))

(comment
  (hsv->rgb (/ 60 360) 1 0.9) := [230 230 0]
  )

(defn ^:deprecated color-hsv
  "Deprecated, please use `color`. Produces inconsistent contrast. Percieved colors are not linear to the human eye.
   Nearby colored area don’t mix well. Text might be unreadable.
  See https://www.hsluv.org/comparison/

  Hash a value into an harmonious color.
  http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
  https://webcache.googleusercontent.com/search?q=cache:qmCbllpQTP8J:https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/+&cd=1&hl=fr&ct=clnk&gl=fr"
  ([x] (color-hsv x
         0.6 #_"Too bright hurts the eyes"
         0.9 #_"Medium gray (50) can be read on white and black backgrounds"))
  ([x s v]
   (if (nil? x)
     "#ccc"
     (let [
           seed 0.3100632204946232                          ; DG: i liked these colors, GG: I don’t like the one for "$" (default)
           hue  (mod (+ seed (* (hash x) PHI)) 1)]
       (apply css-rgb-str (hsv->rgb hue s v))))))
