(ns dustin.y2022.color-hsv-bad)

(defn color-hsv
  "Hash a value into an harmonious color.
   http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
   https://webcache.googleusercontent.com/search?q=cache:qmCbllpQTP8J:https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/+&cd=1&hl=fr&ct=clnk&gl=fr"
  ([x] (color-hsv x
                  0.6 #_"Too bright hurts the eyes"
                  0.9 #_"Medium gray (50) can be read on white and black backgrounds"))
  ([x s v]
   (if (nil? x)
     "#ccc"
     (let [golden-ratio 0.618033988749895
           seed 0.3100632204946232                          ; DG: i liked these colors, GG: I don’t like the one for "$" (default)
           hue (mod (+ seed (* (hash x) golden-ratio)) 1)]
       (apply rgb (hsv->rgb hue s v))))))

(comment
  (color-hsv 1))
