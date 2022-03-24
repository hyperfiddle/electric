(ns hyperfiddle.color
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

(defn rgb [r g b] (str "rgb("r","g","b")"))

(defn color
  "Hash a value into an harmonious color.
  See `http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/`,
  `https://webcache.googleusercontent.com/search?q=cache:qmCbllpQTP8J:https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/+&cd=1&hl=fr&ct=clnk&gl=fr`

  https://www.hsluv.org/
  "
  [x]
  (let [golden-ratio 0.618033988749895
        seed         (/ 125 360) ;; Rotate the chromatic circle like a pointer knob. 1/2 means half a turn.
        ]
    (apply rgb
           (if (nil? x)
             (hsluv->rgb 0 0 80)
             (hsluv->rgb (* 360 (mod (+ seed (* (hash x) golden-ratio)) 1)) ; Hue
                         50             ; Saturation
                         70)            ; Lightness
             ))))
