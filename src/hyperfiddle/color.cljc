(ns hyperfiddle.color)

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

(defn rgb [h s v]
  (let [[r g b] (hsv->rgb h s v)]
    (str "rgb("r","g","b")")))

(defn color
  "Hash a value into an harmonious color.
  See `http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/`,
  `https://webcache.googleusercontent.com/search?q=cache:qmCbllpQTP8J:https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/+&cd=1&hl=fr&ct=clnk&gl=fr`"
  [x]
  (let [golden-ratio 0.618033988749895
        seed         0.3100632204946232 ; DG: i liked these colors, GG: I don’t like the one for "$" (default)
        ]
    (if (nil? x)
      "#ccc"
      (rgb (mod (+ seed (* (hash x) golden-ratio)) 1)
           0.6  #_"Too bright hurts the eyes"
           0.9) #_"Medium gray (50) can be read on white and black backgrounds")))


