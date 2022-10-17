(ns hyperfiddle.ui.color
  ;; FIXME move to appropriate namespace
  ;; TODO restore HSLuv
  #?(:clj (:import (org.hsluv HUSLColorConverter))))

(defn hsluv->rgb [h s l]
  #?(:clj (mapv #(int (* 255 %))
                (HUSLColorConverter/hsluvToRgb (double-array [h s l])))))

(defn hsv->rgb [h s v]
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

(defn rgb [r g b] (str "rgb("r","g","b")"))

(defn color "Hash a value into an harmonious color. https://www.hsluv.org/"
  ; Server only !!!
  ([x] (color x 50 #_70 85))
  ([x saturation lightness]
   (let [golden-ratio 0.618033988749895
         seed (/ 125 360)]                                  ;; Rotate the chromatic circle like a pointer knob. 1/2 means half a turn.
     (apply rgb
            (if (nil? x)
              (hsluv->rgb 0 0 80)
              (hsluv->rgb (* 360 (mod (+ seed (* (hash x) golden-ratio)) 1)) ; Hue
                          saturation lightness))))))
