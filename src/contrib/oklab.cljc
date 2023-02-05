(ns contrib.oklab
  "A perceptual color space.
  https://bottosson.github.io/posts/oklab/

  Specified by CSS Color Module Level 4. Available natively in Safari.
  "
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn deg->rad [alpha] (/ (* alpha Math/PI) 180))

(defn oklch->oklab [[l c h]]
  [l
   (* c (Math/cos (deg->rad h)))
   (* c (Math/sin (deg->rad h)))])

(defn oklab->lrgb [[l a b]]
  ;; https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab
  ;; https://github.com/Evercoder/culori/blob/4ac59027cedc102905233424b84398b3fae0234c/src/oklab/convertOklabToLrgb.js#L1
  (let [L (+ (* l 0.99999999845051981432) (* 0.39633779217376785678 a) (* 0.21580375806075880339 b))
        M (- (* l 1.0000000088817607767) (* 0.1055613423236563494 a) (* 0.063854174771705903402 b))
        S (- (* l 1.0000000546724109177) (* 0.089484182094965759684 a) (* 1.2914855378640917399 b))

        L (Math/pow L 3)
        M (Math/pow M 3)
        S (Math/pow S 3)]

    [(+ (- (* 4.076741661347994 L) (* 3.307711590408193 M)) (* 0.230969928729428 S))
     (- (+ (* -1.2684380040921763 L) (* 2.6097574006633715 M)) (* 0.3413193963102197 S))
     (+ (- (* -0.004196086541837188 L) (* 0.7034186144594493 M)) (* 1.7076147009309444 S))]))

(defn sign [x] (cond (< x 0) -1
                     (> x 0) 1
                     :else   0))

(defn lrgb->rgb [[r g b]]
  ;; https://github.com/Evercoder/culori/blob/4ac59027cedc102905233424b84398b3fae0234c/src/lrgb/convertLrgbToRgb.js#L9
  (let [f (fn [c]
            (let [abs (Math/abs c)]
              (if (> abs 0.0031308)
                (let [s (if (zero? (sign c)) 1 (sign c))]
                  (* s (- (* 1.055 (Math/pow abs (/ 1 2.4))) 0.055)))
                (* c 12.92))))]
    [(f r) (f g) (f b)]))

(defn oklab->rgb [lab] (lrgb->rgb (oklab->lrgb lab)))
(defn oklch->rgb [lch] (oklab->rgb (oklch->oklab lch)))

(tests
  (oklch->oklab [29.2345 44.2 27])    := [29.2345 39.382488369125866 20.066380088487968]
  (oklch->oklab [52.2345 72.2 56.2])  := [52.2345 40.164543439122006 59.99707868160651]
  (oklch->oklab [60.2345 59.2 95.2])  := [60.2345 -5.365448747708576 58.956356398065445]
  (oklch->oklab [62.2345 59.2 126.2]) := [62.2345 -34.963855523099575 47.77205047891309]
  (oklch->oklab [67.2345 42.5 258.2]) := [67.2345 -8.691082203276117 -41.60186402237161]
  (oklch->oklab [29.69 45.553 327.1]) := [29.69 38.24720368913612 -24.7432257186029]
  )

(tests
 (oklab->rgb [0 0 0])          := [0.0 0.0 0.0]
 (oklab->rgb [0.18 0.18 0.18]) := [0.3291136203007484 -0.16074507515968153 -0.03880923470932488]
 (oklab->rgb [0.40 0.50 0.60]) := [1.1788092697656853 -0.6549919000430852 -0.42162843334771566]
 (oklab->rgb [1 1 1])          := [3.2211899649043 -1.7851374303318226 -0.7451880544335563]
 )
