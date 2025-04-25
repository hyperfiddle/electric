(ns hyperfiddle.electric.impl.runtime3-test
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric.impl.lang3 :as l]
            [hyperfiddle.electric.impl.runtime3 :as r]
            [hyperfiddle.rcf :as rcf :refer [tests %]])
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.runtime3-test :refer [root]])))

(defn on-diff! [cb incseq]
  ((m/reduce (fn [_ d] (cb d) nil) nil incseq)
   cb (fn [e] #?(:clj (.printStackTrace ^Throwable e)
                 :cljs (.error js/console e)))))

(defmacro root [site form]
  `(r/make-root ~site
     {::Main ~(l/compile ::Main form
                (assoc (l/normalize-env &env)
                  ::l/peers {:client :clj, :server :clj}))}
     ::Main nil))

(tests
  (on-diff! rcf/tap (r/frame-result (root :client "hello electric")))
  % := {:grow 1, :degree 1, :shrink 0, :permutation {}, :change {0 "hello electric"}, :freeze #{0}}
  % := nil)

#?(:clj ; FIXME fails in cljs
   (tests
     (def !x (atom :foo))
     (on-diff! rcf/tap (r/frame-result (root :client (e/watch !x))))
     % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 :foo}, :freeze #{}}
     (reset! !x :bar)
     % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 :bar}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !x (atom false))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (if (e/watch !x) "foo" "bar"))))
     % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 "bar"}, :freeze #{0}}
     (swap! !x not)
     % := {:degree 2, :permutation {0 1, 1 0}, :grow 1, :shrink 1, :change {0 "foo"}, :freeze #{0}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !bar (atom :bar))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/amb :foo (e/watch !bar) :baz))))
     % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 :foo, 1 :bar, 2 :baz}, :freeze #{0 2}}
     (reset! !bar :BAR)
     % := {:degree 3, :permutation {}, :grow 0, :shrink 0, :change {1 :BAR}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !xs (atom [0 1 2]))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/diff-by identity (e/watch !xs)))))
     % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 0, 1 1, 2 2}, :freeze #{}}
     (swap! !xs conj 3)
     % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :change {3 3}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !xs (atom [0 1 2]))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/cursor [x (e/diff-by identity (e/watch !xs))] (+ x x)))))
     % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :change {0 0, 1 2, 2 4}, :freeze #{}}
     (swap! !xs conj 3)
     % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :change {3 6}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !n (atom 20))
     (def !fizz (atom "Fizz"))
     (def !buzz (atom "Buzz"))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/client
             (let [fizz (e/watch !fizz) ; i/fixed + m/watch + e/join
                   buzz (e/watch !buzz)
                   is (e/diff-by identity (range 1 (inc (e/watch !n))))] ; variable in time and space
               (e/cursor [i is]
                 [i (cond
                      (zero? (mod i (* 3 5))) (str fizz buzz)
                      (zero? (mod i 3)) fizz
                      (zero? (mod i 5)) buzz
                      :else i)]))))))
     % := {:degree 20, :permutation {}, :grow 20, :shrink 0, :change {0 [1 1], 7 [8 8], 1 [2 2], 4 [5 "Buzz"], 15 [16 16], 13 [14 14], 6 [7 7], 17 [18 "Fizz"], 3 [4 4], 12 [13 13], 2 [3 "Fizz"], 19 [20 "Buzz"], 11 [12 "Fizz"], 9 [10 "Buzz"], 5 [6 "Fizz"], 14 [15 "FizzBuzz"], 16 [17 17], 10 [11 11], 18 [19 19], 8 [9 "Fizz"]}, :freeze #{}}
     (swap! !n inc)
     % := {:degree 21, :permutation {}, :grow 1, :shrink 0, :change {20 [21 "Fizz"]}, :freeze #{}}
     (reset! !fizz "Fuzz")
     % := {:degree 21, :permutation {}, :grow 0, :shrink 0, :change {20 [21 "Fuzz"], 2 [3 "Fuzz"], 5 [6 "Fuzz"], 8 [9 "Fuzz"], 11 [12 "Fuzz"], 14 [15 "FuzzBuzz"], 17 [18 "Fuzz"]}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !animals
       (atom [{:name "betsy" :owner "brian" :kind "cow"}
              {:name "jake" :owner "brian" :kind "horse"}
              {:name "josie" :owner "dawn" :kind "cow"}]))
     (def !personalities
       (atom [{:kind "cow" :personality "stoic"}
              {:kind "horse" :personality "skittish"}]))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (let [ks #{:kind}]
             (e/cursor [animal (e/diff-by identity (e/watch !animals))
                        personality (e/diff-by identity (e/watch !personalities))]
               (if (= (select-keys animal ks) (select-keys personality ks))
                 (merge animal personality) (e/amb)))))))
     % := {:degree 3, :permutation {}, :grow 3, :shrink 0, :freeze #{},
           :change {0 {:name "betsy", :owner "brian", :kind "cow", :personality "stoic"},
                    1 {:name "jake", :owner "brian", :kind "horse", :personality "skittish"},
                    2 {:name "josie", :owner "dawn", :kind "cow", :personality "stoic"}}}
     (swap! !animals conj {:name "bob" :owner "jack" :kind "horse"})
     % := {:degree 4, :permutation {}, :grow 1, :shrink 0, :freeze #{},
           :change {3 {:name "bob", :owner "jack", :kind "horse", :personality "skittish"}}}
     (swap! !animals pop)
     % := {:degree 4, :permutation {}, :grow 0, :shrink 1, :change {}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !x (atom "hello"))
     (def !y (atom "electric"))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/as-vec (e/amb (e/watch !x) (e/watch !y))))))
     % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 ["hello" "electric"]}, :freeze #{}}
     (reset! !y "world")
     % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 ["hello" "world"]}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !n (atom 3))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/for-by identity [x (range (e/watch !n))
                               y (range x)]
             [x y]))))
     % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 [[1 0] [2 0] [2 1]]}, :freeze #{}}
     (swap! !n inc)
     % := {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {0 [[1 0] [2 0] [2 1] [3 0] [3 1] [3 2]]}, :freeze #{}}))

#?(:clj ; FIXME fails in cljs
   (tests
     (def !x (atom 0))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (e/drain (rcf/tap (e/watch !x))))))
     % := 0
     % := {:degree 0, :permutation {}, :grow 0, :shrink 0, :change {}, :freeze #{}}
     (swap! !x inc)
     % := 1))

#?(:clj                               ; FIXME fails in cljs
   (tests
     (def !x (atom 0))
     (on-diff! rcf/tap
       (r/frame-result
         (root :client
           (let [Foo (e/fn [x] (e/fn [] x))
                 x (e/watch !x)]
             (= (e/$ Foo x) (e/$ Foo x))))))
     % := {:degree 1, :permutation {}, :grow 1, :shrink 0, :change {0 true}, :freeze #{0}}
     % := nil))

#?(:clj                                 ; FIXME fails in cljs
   (tests
     (def client (root :client (rcf/tap (e/server (identity :foo)))))
     (def server (root :server (rcf/tap (e/server (identity :foo)))))
     (def r-ps ((m/reduce (constantly nil) (r/frame-result client)) {} {}))
     (def c-ps ((r/root-socket client {}
                  (fn [!]
                    (def s->c !)
                    #(prn :dispose)))
                #(rcf/tap :step-c)
                #(prn :done-c)))
     % := :step-c
     (def s-ps ((r/root-socket server {}
                  (fn [!]
                    (def c->s !)
                    #(prn :dispose)))
                #(rcf/tap :step-s)
                #(prn :done-s)))
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)
     % := :step-s
     (s->c @s-ps)
     (hash-set % % %) := #{:foo :step-c :step-s}
     (s->c @s-ps)
     (c->s @c-ps)
     % := :step-c
     (c->s @c-ps)))

#?(:clj ; FIXME fails in cljs
   (tests
     (def client (root :client (rcf/tap (e/client (e/$ (e/server (e/fn [] :foo)))))))
     (def server (root :server (rcf/tap (e/client (e/$ (e/server (e/fn [] :foo)))))))
     (def r-ps ((m/reduce (constantly nil) (r/frame-result client)) {} {}))
     (def c-ps ((r/root-socket client {}
                  (fn [!]
                    (def s->c !)
                    #(prn :dispose)))
                #(rcf/tap :step-c)
                #(prn :done-c)))
     % := :step-c
     (def s-ps ((r/root-socket server {}
                  (fn [!]
                    (def c->s !)
                    #(prn :dispose)))
                #(rcf/tap :step-s)
                #(prn :done-s)))
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)
     % := :step-s
     (s->c @s-ps)
     (hash-set % % %) := #{:foo :step-c :step-s}
     (s->c @s-ps)
     (c->s @c-ps)
     % := :step-c
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)))

#?(:clj ; FIXME fails in cljs
   (tests
     (def client (root :client (rcf/tap (e/client (e/$ (e/server (let [foo :foo] (e/fn [] foo))))))))
     (def server (root :server (rcf/tap (e/client (e/$ (e/server (let [foo :foo] (e/fn [] foo))))))))
     (def r-ps ((m/reduce (constantly nil) (r/frame-result client)) {} {}))
     (def c-ps ((r/root-socket client {}
                  (fn [!]
                    (def s->c !)
                    #(prn :dispose)))
                #(rcf/tap :step-c)
                #(prn :done-c)))
     % := :step-c
     (def s-ps ((r/root-socket server {}
                  (fn [!]
                    (def c->s !)
                    #(prn :dispose)))
                #(rcf/tap :step-s)
                #(prn :done-s)))
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)
     % := :step-s
     (s->c @s-ps)
     (hash-set % %) := #{:step-c :step-s}
     (s->c @s-ps)
     (c->s @c-ps)
     % := :step-c
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)
     % := :step-s
     (s->c @s-ps)
     (hash-set % %) := #{:step-c :step-s}
     (s->c @s-ps)
     (c->s @c-ps)
     (hash-set % %) := #{:foo :step-c}
     (c->s @c-ps)))

#?(:clj ; FIXME fails in cljs
   (tests
     (def client (root :client (rcf/tap (e/join (e/pure (let [x (e/server (identity 2))] x))))))
     (def server (root :server (rcf/tap (e/join (e/pure (let [x (e/server (identity 2))] x))))))
     (def r-ps ((m/reduce (constantly nil) (r/frame-result client)) {} {}))
     (def c-ps ((r/root-socket client {}
                  (fn [!]
                    (def s->c !)
                    #(prn :dispose)))
                #(rcf/tap :step-c)
                #(rcf/tap :done-c)))
     % := :step-c
     (def s-ps ((r/root-socket server {}
                  (fn [!]
                    (def c->s !)
                    #(prn :dispose)))
                #(rcf/tap :step-s)
                #(rcf/tap :done-s)))
     (c->s @c-ps)
     % := :step-s
     (s->c @s-ps)
     % := :step-s
     (s->c @s-ps)
     (hash-set % % %) := #{2 :step-c :step-s}
     (s->c @s-ps)
     (c->s @c-ps)
     % := :step-c
     (c->s @c-ps)))

(tests
  (set (keys (r/->defs {:a (fn [_ _] {:b (fn [_ _] {:a (fn [_ _])})})}))) := #{:a :b})

(tests
  (let [client-peer (root :client (e/server (identity (e/client (#(throw (ex-info "oops" {})))))))
        client-writer-ps ((r/root-socket client-peer {}
                            (fn [_]
                              (rcf/tap :reader-spawn)
                              #(rcf/tap :reader-dispose)))
                          #(rcf/tap :writer-step)
                          #(rcf/tap :writer-done))
        client-root-ps ((r/frame-result client-peer)
                        #(rcf/tap :root-step)
                        #(rcf/tap :root-done))]
    % := :reader-spawn
    % := :writer-step
    % := :root-step
    @client-root-ps := (i/empty-diff 0)
    @client-writer-ps
    % := :reader-dispose
    % := :root-step
    % := :writer-step
    @client-root-ps := (i/empty-diff 0)
    (try @client-writer-ps
         (catch #?(:clj Throwable :cljs :default) _
           (rcf/tap :error)))
    % := :writer-done
    % := :error))
