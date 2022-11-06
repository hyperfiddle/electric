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
  background color."
  #?(:cljs (:require-macros [contrib.hsluv :refer [destructure-array set-array]])))

(comment (set! *warn-on-reflection* true))

(defmacro destructure-array [[bindings array] & body]
  `(let [~@(mapcat (fn [sym idx] [sym `(aget ~array (int ~idx))]) bindings (range))]
     ~@body))

(defmacro set-array [arr & syms] `(doto ~arr ~@(map-indexed (fn [idx sym] `(aset (int ~idx) ~sym)) syms)))

(def ^:const MIN_F 0.00000001)
(def ^:const MAX_F 99.9999999)

(def ^:const M
  [[  3.240969941904521, -1.537383177570093, -0.498610760293    ]
   [ -0.96924363628087,   1.87596750150772,   0.041555057407175 ]
   [  0.055630079696993, -0.20397695888897,   1.056971514242878 ]])

(def ^:const REF_Y 1.0)
(def ^:const REF_U 0.19783000664283)
(def ^:const REF_V 0.46831999493879)

(def ^:const KAPPA 903.2962962)
(def ^:const EPSILON 0.0088564516)

(defn dot-product [a0 a1 a2, b0 b1 b2]
  (+ (* a0 b0)
     (* a1 b1)
     (* a2 b2)))

(defn from-linear ^double [c]
  (double
    (if (<= c 0.0031308)
      (* 12.92 c)
      (- (* 1.055 (Math/pow c (/ 1 2.4))) 0.055))))

(defn lch->luv ^doubles [^doubles lch]
  (destructure-array [[l c h] lch]
    (let [h-rad (* (/ h 180) Math/PI)]
      (set-array lch l (* (Math/cos h-rad) c) (* (Math/sin h-rad) c)))))

(let [[[m1a m1b m1c] [m2a m2b m2c] [m3a m3b m3c]] M]
  (defn xyz->rgb ^doubles [^doubles xyz]
    (destructure-array [[x y z] xyz]
      (set-array xyz
        (from-linear (dot-product m1a m1b m1c x y z))
        (from-linear (dot-product m2a m2b m2c x y z))
        (from-linear (dot-product m3a m3b m3c x y z))))))

(defn l-to-y [L]
  (if (<= L 8)
    (/ (* REF_Y L) KAPPA)
    (* REF_Y (Math/pow (/ (+ L 16) 116) 3))))

(defn luv->xyz ^doubles [^doubles luv]
  (destructure-array [[l u v] luv]
    (if (zero? l)
      (set-array luv 0.0 0.0 0.0)
      (let [varU (+ (/ u (* 13 l)) REF_U)
            varV (+ (/ v (* 13 l)) REF_V)
            y    (l-to-y l)
            x    (- 0 (/ (* (* 9 y) varU) (- (* (- varU 4) varV) (* varU varV))))
            z    (/ (- (- (* 9 y) (* (* 15 varV) y)) (* varV x)) (* 3 varV))]
        (set-array luv (double x) (double y) (double z))))))

(let [[[mr0 mr1 mr2]
       [mg0 mg1 mg2]
       [mb0 mb1 mb2]] M
      ra              (- (* 284517 mr0) (* 94839 mr2))
      rb              (+ (* 838422 mr2) (* 769860 mr1) (* 731718 mr0))
      rc              (- (* 632260 mr2) (* 126452 mr1))

      ga              (- (* 284517 mg0) (* 94839 mg2))
      gb              (+ (* 838422 mg2) (* 769860 mg1) (* 731718 mg0))
      gc              (- (* 632260 mg2) (* 126452 mg1))

      ba              (- (* 284517 mb0) (* 94839 mb2))
      bb              (+ (* 838422 mb2) (* 769860 mb1) (* 731718 mb0))
      bc              (- (* 632260 mb2) (* 126452 mb1))]

  (defn bounding-lines [l]
    (let [sub1 (/ (Math/pow (+ l 16) 3) 1560896)
          sub2 (if (> sub1 EPSILON) sub1 (/ l KAPPA))

          s1r (* sub2 ra)
          s2r (* sub2 rb)
          s3r (* sub2 rc)

          s1g (* sub2 ga)
          s2g (* sub2 gb)
          s3g (* sub2 gc)

          s1b (* sub2 ba)
          s2b (* sub2 bb)
          s3b (* sub2 bc)

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
      [r0s r0i
       r1s r1i
       g0s g0i
       g1s g1i
       b0s b0i
       b1s b1i])))

(defn distance-from-origin-angle [slope intercept angle]
  (let [d (/ intercept (- (Math/sin angle) (* slope (Math/cos angle))))]
    (if (< d 0) ##Inf d)))

(defn max-chroma-hsluv [l h]
  (let [hue-rad     (* (/ h 360) Math/PI 2)
        [r0s r0i
         r1s r1i
         g0s g0i
         g1s g1i
         b0s b0i
         b1s b1i] (bounding-lines l)]
    (min
      (distance-from-origin-angle r0s r0i hue-rad)
      (distance-from-origin-angle r1s r1i hue-rad)
      (distance-from-origin-angle g0s g0i hue-rad)
      (distance-from-origin-angle g1s g1i hue-rad)
      (distance-from-origin-angle b0s b0i hue-rad)
      (distance-from-origin-angle b1s b1i hue-rad))))

(defn hsluv->lch [^doubles hsl]
  (destructure-array [[h s l] hsl]
    (if (> l MAX_F)
      [100 0 h]
      (if (< l MIN_F)
        [0 0 h]
        (set-array hsl l, (* (/ (max-chroma-hsluv l h) 100) s), h)))))

(defn hsluv->rgb "`hsluv->rgb` convert HSLuv components to RGB"
  [hsl]
  (vec (-> (double-array hsl) hsluv->lch lch->luv luv->xyz xyz->rgb)))

(comment
  (hsluv->rgb [28.140599727630615 50 85])
   := [0.9227355039080426 0.8083448671138809 0.7774746499934942]
  )


(comment
  (require '[criterium.core :as perf])
  (require '[clj-async-profiler.core :as prof])

  (prof/serve-files 8080)
  (perf/quick-bench (hsluv->rgb [28.140599727630615 50 85]))
  (set! *warn-on-reflection* true)
  )
