;; Partial port of HSLuv’s reference implementation written by Alexei Boronine
;; under MIT license. https://www.hsluv.org/

;; Supported features:
;; - [X] hsluvToRgb
;; - [ ] hpluvToRgb
;; - [ ] rgbToHsluv
;; - [ ] rgbToHpluv

(ns contrib.hsluv
  "HSLuv is a human-friendly alternative to HSL. HSLuv colors have an almost
  uniform hue and consistant contrasts. Text will stay readable no matter the
  background color.")

(comment (set! *warn-on-reflection* true))

(def MIN_F 0.00000001)
(def MAX_F 99.9999999)

(def M [[  3.240969941904521, -1.537383177570093, -0.498610760293    ]
        [ -0.96924363628087,   1.87596750150772,   0.041555057407175 ]
        [  0.055630079696993, -0.20397695888897,   1.056971514242878 ]])

(def REF_Y 1.0)
(def REF_U 0.19783000664283)
(def REF_V 0.46831999493879)

(def KAPPA 903.2962962)
(def EPSILON 0.0088564516)

(defn dot-product [[a0 a1 a2] [b0 b1 b2]]
  (+ (* a0 b0)
     (* a1 b1)
     (* a2 b2)))

(defn from-linear [c]
  (if (<= c 0.0031308)
    (* 12.92 c)
    (- (* 1.055 (Math/pow c (/ 1 2.4))) 0.055)))

(defn lch->luv [[l c h]]
  (let [h-rad (* (/ h 180) Math/PI)]
    [l (* (Math/cos h-rad) c) (* (Math/sin h-rad) c)]))

(defn xyz->rgb [xyz]
  (let [[m1 m2 m3] M]
    [(from-linear (dot-product m1 xyz))
     (from-linear (dot-product m2 xyz))
     (from-linear (dot-product m3 xyz))]))

(defn l-to-y [L]
  (if (<= L 8)
    (/ (* REF_Y L) KAPPA)
    (* REF_Y (Math/pow (/ (+ L 16) 116) 3))))

(defn luv->xyz
  [[l u v]]
  (if (= l 0)
    [0 0 0]
    (let [varU (+ (/ u (* 13 l)) REF_U)
          varV (+ (/ v (* 13 l)) REF_V)
          y    (l-to-y l)
          x    (- 0 (/ (* (* 9 y) varU) (- (* (- varU 4) varV) (* varU varV))))
          z    (/ (- (- (* 9 y) (* (* 15 varV) y)) (* varV x)) (* 3 varV))]
      [x y z])))

(defn bounding-lines [l]
  (let [sub1            (/ (Math/pow (+ l 16) 3) 1560896)
        sub2            (if (> sub1 EPSILON) sub1 (/ l KAPPA))
        [[mr0 mr1 mr2]
         [mg0 mg1 mg2]
         [mb0 mb1 mb2]] M

        s1r (* sub2 (- (* 284517 mr0) (* 94839 mr2)))
        s2r (* sub2 (+ (* 838422 mr2) (* 769860 mr1) (* 731718 mr0)))
        s3r (* sub2 (- (* 632260 mr2) (* 126452 mr1)))

        s1g (* sub2 (- (* 284517 mg0) (* 94839 mg2)))
        s2g (* sub2 (+ (* 838422 mg2) (* 769860 mg1) (* 731718 mg0)))
        s3g (* sub2 (- (* 632260 mg2) (* 126452 mg1)))

        s1b (* sub2 (- (* 284517 mb0) (* 94839 mb2)))
        s2b (* sub2 (+ (* 838422 mb2) (* 769860 mb1) (* 731718 mb0)))
        s3b (* sub2 (- (* 632260 mb2) (* 126452 mb1)))

        r0s (/ s1r s3r)
        r0i (/ (* s2r l) s3r)
        r1s (/ s1r (+ s3r 126452))
        r1i (/ (* (- s2r 769860) l) (+ s3r 126452))

        g0s (/ s1g s3g)
        g0i (/ (* s2g l) s3g)
        g1s (/ s1g (+ s3g 126452))
        g1i (/ (* (- s2g 769860) l) (+ s3g 126452))

        b0s (/ s1b s3b)
        b0i (/ (* s2b l) s3b)
        b1s (/ s1b (+ s3b 126452))
        b1i (/ (* (- s2b 769860) l) (+ s3b 126452))
        ]
    [[r0s r0i]
     [r1s r1i]
     [g0s g0i]
     [g1s g1i]
     [b0s b0i]
     [b1s b1i]]))

(defn distance-from-origin-angle [slope intercept angle]
  (let [d (/ intercept (- (Math/sin angle) (* slope (Math/cos angle))))]
    (if (< d 0) ##Inf d)))

(defn max-chroma-hsluv [l h]
  (let [hue-rad     (* (/ h 360) Math/PI 2)
        [[r0s r0i]
         [r1s r1i]
         [g0s g0i]
         [g1s g1i]
         [b0s b0i]
         [b1s b1i]] (bounding-lines l)]
    (min
      (distance-from-origin-angle r0s r0i hue-rad)
      (distance-from-origin-angle r1s r1i hue-rad)
      (distance-from-origin-angle g0s g0i hue-rad)
      (distance-from-origin-angle g1s g1i hue-rad)
      (distance-from-origin-angle b0s b0i hue-rad)
      (distance-from-origin-angle b1s b1i hue-rad))))

(defn hsluv->lch [[h s l]]
  (if (> l MAX_F)
    [100 0 h]
    (if (< l MIN_F)
      [0 0 h]
      [l, (* (/ (max-chroma-hsluv l h) 100) s), h])))

(def hsluv->rgb "`hsluv->rgb` convert HSLuv components to RGB" (comp xyz->rgb luv->xyz lch->luv hsluv->lch))

(comment
  (hsluv->rgb [28.140599727630615 50 85])
  )
