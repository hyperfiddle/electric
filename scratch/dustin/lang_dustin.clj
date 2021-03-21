(ns dustin.lang-dustin
  "tests to communicate about leo lang"
  (:require [hfdl :refer [dataflow]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(declare dustin-lang debug! pure lift cp)

(tests

  "Dustin-lang sees every sexpr containing a ~ as monadic expression. This property yields the
  isomorphism between textual/visual representation as bind-nodes, apply-nodes and fmap-nodes
  are explicit in the AST:

    fmap    (inc ~x)
    fapply  (~f ~x)
    bind    (f. ~x)

  I don't know if this approach can be made good. The programmer must know which symbols are variable
  and which functions return variables. However the lang does not need ifn."

  "the environment is reactive (pre-lifted)"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dustin-lang ~a))
  (def process (debug! program))
  @process := {:state :running :vars {'a 1}}                ; vars named by sourcemap, 'a is the output node
  (swap! !a inc)
  @process := {:state :running :vars {'a 2}}

  "sexpr application is fapply"
  (def f (fn [x] (cp (inc x))))                             ; f :: a -> m b
  (def !a (atom 1)) (def a (m/watch !a))                    ; a :: m a
  (def program (dustin-lang (f ~a)))                        ; foreign env is available, mark lifted values
  (def process (debug! program))
  @process := {:state :running :vars {'a 1 '(f a) 2}}
  (swap! !a inc)
  @process := {:state :running :vars {'a 2 '(f a) 3}}

  "interop to a foreign value"
  (def x 1)                                                 ; a :: Number
  (def program (dustin-lang x))
  (def process (debug! program))
  @process := {:state :terminated :vars {'x 1}}

  ""
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dustin-lang (+ ~a 100 ~(inc ~b))))
  (def process (debug! program))
  @process := {:state :running :vars {'a 1 'b 2 '(inc ~b) 3 '(+ ~a 100 ~(inc ~b)) 104}}

  ;"literals are auto-lifted"
  ;(def process (debug! (dustin-lang (f 1))))
  ;@process := {:state :running                              ; f is dynamic, so we can't terminate
  ;             :vars  {'(f 1) 2}}

  ;"interop to foreign clojure fns"
  ;(def x 42)
  ;(def process (debug! (dustin-lang (+ 1 x))))
  ;@process := {:state :terminated :vars {'(+ 1 x) 43}}    ; ??? terminated

  ""
  (defn submissions [x]                                     ; :: a -> m b
    #_(m/via m/cpu (d/q ... x))
    (cp (filter #(= x (:id %)) [{:id 1 :name "alice"} {:id 2 :name "bob"}])))
  (def process
    (debug!
      (dustin-lang
        ;(defn foo [b] (submissions b)) ; what would defn mean in dustin-lang
        (submissions. ~a))))                                ; mark var and invoke effect with bind
  @process := {:state :running
               :vars  {'a                 1
                       '(submissions. ~a) [{:id 1 :name "alice"} {:id 2 :name "bob"}]}}

  "let is not m/signal"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dustin-lang (+ ~(inc ~a) ~(inc ~b))))
  (def process (debug! program))
  @process := {:state :running :vars {'a                       1
                                      'b                       2
                                      '(inc ~a)                2
                                      '(inc ~b)                3
                                      '(+ ~(inc ~a) ~(inc ~b)) 5}}

  "bind vs do-notation"
  (def !a (atom 1)) (def a (m/watch !a))
  (def p1 (dustin-lang (first ~(submissions. ~a))))
  @(debug! p1) := {:state :running
                   :vars  {'a                 1
                           '(submissions. ~a) [{:id 1 :name "alice"}]
                           '(first %)         {:id 1 :name "alice"}}}

  (def p2 (dustin-lang (let [% ~(submissions ~a)]
                         ; do notation lets us jump out and back into the flow ? is that right?
                         (cp (first %)))))
  @(debug! p2) := {:state :running
                   :vars  {'a                 1
                           '(submissions. ~a) [{:id 1 :name "alice"}]
                           '(first %)         {:id 1 :name "alice"}}}

  )