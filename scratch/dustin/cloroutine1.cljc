(ns dustin.cloroutine1
  (:require
    [cloroutine.core :refer [cr]]
    [cloroutine.impl :as i]
    [minitest :refer [tests]]))

; https://github.com/leonoel/cloroutine/blob/master/doc/01-generators.md

(def ^:dynamic *tail*)

(defn gen-seq [gen]
  (binding [*tail* (lazy-seq (gen-seq gen))]
    (gen)))

(defn yield [x] (cons x *tail*))

(defn no-op [])

(defmacro generator [& body]
  `(gen-seq (cr {yield no-op} ~@body nil)))

(tests

  (take 3
    (generator
      (yield :a)
      (yield :b)
      (yield :c)))
  := [:a :b :c]

  (take 5
    (generator
      (yield :a)
      (yield :b)
      (yield :c)))
  := [:a :b :c]
  )

(defn my-repeat [x]
  (generator
    (loop []
      (yield x)
      (recur))))

(tests
  (take 3 (my-repeat :a))
  := [:a :a :a])

(defn my-iterate [f seed]
  (generator
    (loop [x seed]
      (yield x)
      (recur (f x)))))

(tests
  (take 3 (my-iterate inc 0))
  := [0 1 2])

(tests
  "how does it work"
  (macroexpand '(generator (yield :a)))
  := (dustin.cloroutine1/gen-seq
       (cloroutine.core/cr
         #:dustin.cloroutine1{yield dustin.cloroutine1/no-op}
         (yield :a) nil))
  )

(defn ping [x] x)
(defn pong [] :pong)

(def c (cr {ping pong}
         (ping :a)
         (ping :b)
         (ping :c)
         nil))
(c)

(macroexpand-1 '(cr {ping pong} (ping :a) nil))

:= '(letfn [(cr17003-block-0 [cr17003-state]
              (i/safe
                [cr17003-exception
                 (let []
                   (let [cr17003-place-0 (i/hint clojure.lang.Keyword clojure.lang.Keyword :a)]
                     (do
                       (aset cr17003-state 0 cr17003-block-1)
                       (dustin.cloroutine1/ping cr17003-place-0))))]
                (do
                  (aset cr17003-state 0 nil)
                  (throw cr17003-exception))))
            (cr17003-block-1 [cr17003-state]
              (i/safe [cr17003-exception
                       (let []
                         (let [cr17003-place-1 (pong)
                               cr17003-place-2 (i/hint nil nil nil)]
                           (do
                             (aset cr17003-state 0 nil)
                             cr17003-place-2)))]
                (do
                  (aset cr17003-state 0 nil)
                  (throw cr17003-exception))))]
      (i/coroutine
        (doto (object-array 1)
          (aset 0 cr17003-block-0))))


(defmacro via [& body] `(cr {unquote no-op} ~@body))

(via (vector ~1))