(ns hyperfiddle.electric.impl.runtime-test
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [hyperfiddle.electric-de :as e]
            #?(:clj [hyperfiddle.electric.impl.lang-de2 :as l])
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.rcf :as rcf :refer [tests %]]))

(defn on-diff! [cb incseq]
  ((m/reduce (fn [_ d] (cb d) nil) nil incseq)
   cb (fn [e] #?(:clj (.printStackTrace ^Throwable e)
                 :cljs (.error js/console e)))))

(defmacro root-frame [form]
  `(r/root-frame {::Main ~(l/compile ::Main form
                            (assoc (l/normalize-env &env)
                              ::l/peers {:client :clj, :server :clj}))}
     ::Main))

(tests
  (on-diff! rcf/tap (root-frame "hello electric"))
  % := {:grow 1, :degree 1, :shrink 0, :permutation {}, :change {0 "hello electric"}, :freeze #{0}}
  % := nil)

(tests
  (def !x (atom :foo))
  (on-diff! rcf/tap (root-frame (e/watch !x)))
  % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 :foo}, :freeze #{}}
  (reset! !x :bar)
  % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 :bar}, :freeze #{}})

(tests
  (def !x (atom false))
  (on-diff! rcf/tap
    (root-frame (if (e/watch !x) "foo" "bar")))
  % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 "bar"}, :freeze #{0}}
  (swap! !x not)
  % := {:degree 2, :permutation {0 1, 1 0}, :grow 1, :shrink 1, :change {0 "foo"}, :freeze #{0}})

(tests
  (def !bar (atom :bar))
  (on-diff! rcf/tap
    (root-frame (e/amb :foo (e/watch !bar) :baz)))
  % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 :foo, 1 :bar, 2 :baz}, :freeze #{0 2}}
  (reset! !bar :BAR)
  % := {:degree 3, :permutation {}, :grow 0, :shrink 0, :change {1 :BAR}, :freeze #{}})

(tests
  (def !xs (atom [0 1 2]))
  (on-diff! rcf/tap (root-frame (e/diff-by identity (e/watch !xs))))
  % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 0, 1 1, 2 2}, :freeze #{}}
  (swap! !xs conj 3)
  % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :change {3 3}, :freeze #{}})

(tests
  (def !xs (atom [0 1 2]))
  (on-diff! rcf/tap (root-frame (e/cursor [x (e/diff-by identity (e/watch !xs))] (+ x x))))
  % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 0, 1 2, 2 4}, :freeze #{}}
  (swap! !xs conj 3)
  % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :change {3 6}, :freeze #{}})

(tests
  (def !n (atom 20))
  (def !fizz (atom "Fizz"))
  (def !buzz (atom "Buzz"))
  (on-diff! rcf/tap (root-frame (e/server (let [fizz (e/watch !fizz) ; i/fixed + m/watch + e/join
                                                buzz (e/watch !buzz)
                                                is (e/diff-by identity (range 1 (inc (e/watch !n))))] ; variable in time and space
                                            (e/cursor [i is]
                                              [i (cond
                                                   (zero? (mod i (* 3 5))) (str fizz buzz)
                                                   (zero? (mod i 3)) fizz
                                                   (zero? (mod i 5)) buzz
                                                   :else i)])))))
  % := {:degree 20, :permutation {}, :grow 20, :shrink 0, :change {0 [1 1], 7 [8 8], 1 [2 2], 4 [5 "Buzz"], 15 [16 16], 13 [14 14], 6 [7 7], 17 [18 "Fizz"], 3 [4 4], 12 [13 13], 2 [3 "Fizz"], 19 [20 "Buzz"], 11 [12 "Fizz"], 9 [10 "Buzz"], 5 [6 "Fizz"], 14 [15 "FizzBuzz"], 16 [17 17], 10 [11 11], 18 [19 19], 8 [9 "Fizz"]}, :freeze #{}}
  (swap! !n inc)
  % := {:degree 21, :permutation {}, :grow 1, :shrink 0, :change {20 [21 "Fizz"]}, :freeze #{}}
  (reset! !fizz "Fuzz")
  % := {:degree 21, :permutation {}, :grow 0, :shrink 0, :change {20 [21 "Fuzz"], 2 [3 "Fuzz"], 5 [6 "Fuzz"], 8 [9 "Fuzz"], 11 [12 "Fuzz"], 14 [15 "FuzzBuzz"], 17 [18 "Fuzz"]}, :freeze #{}})

(tests
  (def !animals
    (atom [{:name "betsy" :owner "brian" :kind "cow"}
           {:name "jake" :owner "brian" :kind "horse"}
           {:name "josie" :owner "dawn" :kind "cow"}]))
  (def !personalities
    (atom [{:kind "cow" :personality "stoic"}
           {:kind "horse" :personality "skittish"}]))
  (on-diff! rcf/tap
    (root-frame
      (let [ks #{:kind}]
        (e/cursor [animal (e/diff-by identity (e/watch !animals))
                   personality (e/diff-by identity (e/watch !personalities))]
          (if (= (select-keys animal ks) (select-keys personality ks))
            (merge animal personality) (e/amb))))))
  % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :freeze #{},
        :change {0 {:name "betsy", :owner "brian", :kind "cow", :personality "stoic"},
                 1 {:name "jake", :owner "brian", :kind "horse", :personality "skittish"},
                 2 {:name "josie", :owner "dawn", :kind "cow", :personality "stoic"}}}
  (swap! !animals conj {:name "bob" :owner "jack" :kind "horse"})
  % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :freeze #{},
        :change {3 {:name "bob", :owner "jack", :kind "horse", :personality "skittish"}}}
  (swap! !animals pop)
  % := {:degree 4, :permutation {}, :grow 0, :shrink 1, :change {}, :freeze #{}})

(tests
  (def !x (atom "hello"))
  (def !y (atom "electric"))
  (on-diff! rcf/tap
    (root-frame (e/as-vec (e/amb (e/watch !x) (e/watch !y)))))
  % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 ["hello" "electric"]}, :freeze #{}}
  (reset! !y "world")
  % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 ["hello" "world"]}, :freeze #{}})

(tests
  (def !n (atom 3))
  (on-diff! rcf/tap
    (root-frame (e/for-by identity [x (range (e/watch !n))
                                    y (range x)]
                  [x y])))
  % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 [[1 0] [2 0] [2 1]]}, :freeze #{}}
  (swap! !n inc)
  % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 [[1 0] [2 0] [2 1] [3 0] [3 1] [3 2]]}, :freeze #{}})