(ns main-test
  (:require-macros [minitest :refer [tests]])
  (:require
    ; runs the tests
    #_[dustin.hfql19]
    [minitest]))

(tests
  1 => 1
  1 => 2
  )

(println "done")
