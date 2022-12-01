(ns dustin.y2022.photon-loop)


(tests
  (p/defn F [n] (dom/div (str n)))
  (with (p/run (loop [n 0]
                 (if (< n 5)
                   (recur (F. n))
                   n)))))
