(ns hyperfiddle.electric-de-test
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap with %]]
            [hyperfiddle.electric-local-def-de :as l]))

;; fails to compile, rcf rewrites `tap` to `RCF__tap` and electric compiler fails to resolve that
;; (tests "hello world"
;;   (with ((l/single {} (tap "hello world")) tap tap)
;;     % := "hello world"))

(def hello (l/single {} (tap "hello world")))
(tests "hello world"
  (with (hello tap tap)
    % := "hello world"                  ; returns a diff {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 "hello world"}, :freeze #{0}}
    ))
