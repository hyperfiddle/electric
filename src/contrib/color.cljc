(ns contrib.color
  (:require [contrib.hsluv :as hsluv]
            [contrib.oklab :as oklab]
            [hyperfiddle.rcf :refer [tests]]))

(def PHI 0.618033988749895)  ; Golden ratio
(def SEED-ANGLE (/ 125 360)) ; Rotate the chromatic circle like a pointer knob. 1/2 means half a turn.

(defn clamp [low up x] (min (max low x) up))

(tests
  (clamp 5 10 4)  := 5
  (clamp 5 10 11) := 10
  (clamp 5 10 6)  := 6
  )

(defn rgbint [x] (clamp 0 255 (int (* x 255))))

(defn hsluv->rgb [[h s l]]
  (mapv rgbint (hsluv/hsluv->rgb [h s l])))

(defn css-rgb-str [r g b] (str "rgb("r","g","b")"))

(defn color "Hash a value into an harmonious color. Contrast is consistent. Plays well with text and colored backgrounds.
  https://www.hsluv.org/"
  ([x] (color x SEED-ANGLE 50 70 #_85))
  ([x seed-angle saturation lightness]
   (apply css-rgb-str
     (if (nil? x)
       (hsluv->rgb [0 0 80])
       (hsluv->rgb [(* 360 (mod (+ seed-angle (* (hash x) PHI)) 1)) ; Hue
                    saturation lightness])))))

;;; Misc

(defn hsl->rgb* [[H S L]]
  (let [S          (double (/ S 100))            ; scale to [0..1]
        L          (double (/ L 100))
        C          (* (- 1 (Math/abs (- (* L 2) 1))) S)
        X          (* C (- 1 (Math/abs (- (mod (/ H 60) 2) 1))))
        m          (- L (/ C 2))
        [R' G' B'] (cond
                     (< H 60)  [C X 0]
                     (< H 120) [X C 0]
                     (< H 180) [0 C X]
                     (< H 240) [0 X C]
                     (< H 300) [X 0 C]
                     (< H 360) [C 0 X])]
    [(* (+ R' m) 255)
     (* (+ G' m) 255)
     (* (+ B' m) 255)]))

(defn hsl->rgb [hsl]
  (let [[R G B] (hsl->rgb* hsl)]
    [(Math/round ^float R)
     (Math/round ^float G)
     (Math/round ^float B)]))

(tests
  (hsl->rgb [0   0   0])   := [0 0 0]       ; black
  (hsl->rgb [0   0   100]) := [255 255 255] ; white
  (hsl->rgb [0   100 50])  := [255 0 0]     ; red
  (hsl->rgb [120 100 50])  := [0 255 0]     ; lime
  (hsl->rgb [240 100 50])  := [0 0 255]     ; blue
  (hsl->rgb [60  100 50])  := [255 255 0]   ; yellow
  (hsl->rgb [180 100 50])  := [0 255 255]   ; cyan
  (hsl->rgb [300 100 50])  := [255 0 255]   ; magenta
  (hsl->rgb [0   0   75])  := [191 191 191] ; silver
  (hsl->rgb [0   0   50])  := [128 128 128] ; gray
  (hsl->rgb [0   100 25])  := [128 0 0]     ; maroon
  (hsl->rgb [60  100 25])  := [128 128 0]   ; olive
  (hsl->rgb [120 100 25])  := [0 128 0]     ; green
  (hsl->rgb [300 100 25])  := [128 0 128]   ; purple
  (hsl->rgb [180 100 25])  := [0 128 128]   ; teal
  (hsl->rgb [240 100 25])  := [0 0 128]     ; navy
  )


;; OKLCH

(defn oklch->rgb [[l c h]] (mapv rgbint (oklab/oklch->rgb [(/ l 100) c h])))

(comment
  ;; XYZ
  ;; Untested
  (def ^:const Mxyz [[0.4124 0.3576 0.1805]
                     [0.2126 0.7152 0.0722]
                     [0.0193 0.1192 0.9505]])

  (defn scale-rgb [C]
    (let [var_C (/ C 255)]
      (* (if (> var_C 0.04045)
           (Math/pow (/ (+ var_C 0.055) 1.055) 2.4)
           (/ var_C 12.92))
        100)))

  (let [[[xyz1a xyz1b xyz1c]
         [xyz2a xyz2b xyz2c]
         [xyz3a xyz3b xyz3c]] Mxyz]
    (defn rgb->xyz [[R G B]]
      (let [var_R (scale-rgb R)
            var_G (scale-rgb G)
            var_B (scale-rgb B)

            X (+ (* var_R xyz1a) (* var_G xyz1b) (* var_B xyz1c))
            Y (+ (* var_R xyz2a) (* var_G xyz2b) (* var_B xyz2c))
            Z (+ (* var_R xyz3a) (* var_G xyz3b) (* var_B xyz3c))
            ]
        [X Y Z]))))
