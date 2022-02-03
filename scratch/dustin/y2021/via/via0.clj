(ns dustin.via0)

(comment
  (via (Identity)
    (let [a 1
          b (+ a 2)]
      (inc (inc b))))
  := 5

  (via (Trace)
    (let [a 1
          b (+ a 2)]
      (inc (inc b))))
  := 5
  *this := [{a 1} {(+ a 2) 3} {b 3} {(inc 3) 4} {(inc 4) 5}]

  (via (Stream)
    (let [a 1
          b (+ a 2)]                                         ; (fapply (pure +) a (pure 2)) - be smarter here
      (inc (inc b))))
  := (Stream 5)

  (via (TracingStream)
    (let [a 1
          b (+ a 2)]
      (inc (inc b))))
  := (Stream 5)
  *this := [{a 1}
            {(+ a 2) 3}
            {b 3}
            {(inc 3) 4}
            {(inc 4) 5}]

  )