(ns hyperfiddle.executor
  (:require [hyperfiddle.fabric :as f :refer [defnode]]
            [hyperfiddle.trace :as trace]
            [minitest :refer [tests]]))

(tests
 !! (do
      (f/set-executor! f/compute-executor)
      (defnode >a (f/input))
      (defnode >b (f/fmap inc >a))
      (defnode >c (f/on >b prn))
      (f/put >a 1))
 (trace/trace >c) => '([>a 1] [>b 2] [>c 2])

 !! (def trace (into {} '([>a 1] [>b 2] [>c 2])))

 !! (f/set-executor! (f/cache-or-compute-executor (constantly trace)))
 !! (f/put >a 1)
 !! (f/replay >a)
 !! (f/put >a 100)
 (trace/trace >c) => '([>a 1] [>b 2] [>c 2])

 )
