(ns dustin.via.fode
  (:require
    [clojure.walk :refer [postwalk]]
    [meander.epsilon :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(defprotocol Interpreter
  ;(apply-id [_ as])
  (fapply [_ ms])
  (join [_ mma])
  (pure [_ c])                                              ; dumb
  (inject [M form])
  (run [M ast])
  (boot [M ast resolve! reject!] [M ast]))

(defn resolve' [M env x] {:post [(doto % (assert "unresolved symbol"))]}
  (assert (symbol? x) (str "resolve error, x: " (pr-str x) " is not a symbol"))
  (if (contains? env x)
    (pure M (env x)) (some-> (resolve x) deref)))

(defn lift-and-resolve [M effects x] #_(println 'lift-and-resolve x)
  (match x
    ('quote ?a) (pure M (lift-and-resolve M effects ?a))
    'quote x
    ?a (if (symbol? ?a) (resolve' M effects ?a) ?a)))

(defn if2 [test ma mb] (if test ma mb))

(defn bind [M ma eff] (join M (fapply M [(pure M eff) ma])))

; sequence :: t (m a) -> m (t a)
(defn sequence' [M mas] (fapply M (cons (pure M list) mas)))

; traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
(defn foreach [M list-a eff] (sequence' M (map eff list-a)))

(defn Eval-incr [effects]
  ; free monad ish lisp thing.
  ; You give us the effects and a monad instance.
  ; We give you lisp interpreter for free
  (reify
    Interpreter
    (fapply [M ms] #_(println 'fapply ms)
      (m/signal! (apply m/latest #(apply % %&) ms)))
    (join [M mma] #_(println 'join mma)
      (m/signal! (m/relieve {} (m/ap (m/?! (run M (m/?! mma)))))))
    (pure [M c] (m/ap c))
    (inject [M x] (println 'inject x)
      (match x
        (`deref ?x) (join M ?x)
        ('quote _) x
        (!xs ...) (let [mas (doall (map #(lift-and-resolve M effects %) !xs))
                        mmb (fapply M mas)] (join M mmb))
        _ x))

    (run [M ast] #_(println 'run ast)
      (postwalk #(inject M %) ast))

    (boot [M ast resolve! reject!]
      ((m/reactor (run M ast)) resolve! reject!))

    (boot [M ast]
      (boot M ast #(println ::finished %) #(println ::crashed %)))))



(tests

  "incremental "
  (defn print-out! [!out x] #_(println 'print-out! !out x)
    ; effects are async backpressured signals
    `~(m/ap #_(m/? (m/sleep 100))   ; sleep is not fun to test
        (swap! !out conj (with-out-str (print x)))))

  (def !x (atom 0)) (def >x (m/watch !x))
  (def !p (atom :odd)) (def >p (m/watch !p))
  (def !q (atom :even)) (def >q (m/watch !q))

  (defn App [>x]
    `(inc @(if. (odd? @>x) >p >q)))

  (def !out (atom []))
  (boot (Eval-incr {`println #(apply print-out! !out %&)
                    `if if2})
    `(println. (App. '>x)))

  (swap! !x inc)
  (swap! !x inc)
  @!out := [:even :odd :even]
  )

; differential primitives

(defn diff [- init]
  (fn [rf]
    (let [p (volatile! init)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [r (rf r (- x @p))]
           (vreset! p x) r))))))

(defn dx "differentiate a flow" [>xs]
  (m/transform
    (comp
      (map set)                                             ; cheat
      (diff (fn [x y]
              ; Derivative of a set is its additions and removals
              ; (like git patches)
              [(clojure.set/difference x y)
               (clojure.set/difference y x)])
        #{})) >xs))

(defn patch [] ...)

(tests
  "differential set"
  (m/? (m/aggregate conj
         (dx  ; flow boilerplate to test just this
           (m/enumerate [[1 2 3] [1 2 4] [2]]))))
  := [[#{1 2 3} #{}] [#{4} #{3}] [#{} #{1 4}]])

(tests
  "differential datomic query"
  (def !db (atom *$*))
  (def z (dx (m/ap (datomic.api/q '[:find ?e :where [?e :dustingetz/email]] (m/?! (m/watch !db))))))
  (def !z (z #(prn :ready) #(prn :done)))
  @!z := [#{[10] [9] [11]} #{}]
  (swap! !db #(:db-after (datomic.api/with % [{:dustingetz/email "dan@example.com"}])))
  @!z := [#{[12]} #{}]
  (swap! !db #(:db-after (datomic.api/with % [[:db/add 12 :dustingetz/email "dan2@example.com"]
                                              [:db/retractEntity 11]])))
  @!z := [#{} #{[11]}])

(tests
  (defn App [>db]
    `(div.
       (let [>xs' (dx (datomic.api/q (quote [:in $ :find ?e :where [?e :person/name]]) '>db))]
         (pre. (pr-str @(m/reductions patch >xs')))            ; [101 102 104 106]
         (for. [>id >xs']                                   ; differential for
           (pre.
             (pr-str @(entity-get' >db >id ':person/tags))
             (for. [>k (entity-ks' >db >id)]
               (pr-str @(entity-get' >db >id >k))))))))
  := _
  )





(defmacro defnode [& body] `~body)                                 ; defobject class deftype

;(deftype Foo [x])
;(Foo 1)
;(tests Foo := _)

(defmacro rfor [bindings body])
(defnode div [& children])
(defnode pre [& children])
(defnode entity-get [db e & [k]])
(defnode entity-ks [db e])
(defnode dx [x])

(defn q [db] (m/via _ (datomic.api/q (quote [:in $ :find ?e :where [?e :person/name]]) db)))
(defeffect q [db] (m/via _ (datomic.api/q (quote [:in $ :find ?e :where [?e :person/name]]) db)))
(defnode q [db] (datomic.api/q (quote [:in $ :find ?e :where [?e :person/name]]) @db))

(defnode if2 [test a b] ({true a false b} @test))           ; m bool -> m a -> m a -> m a

; :: FLow [Task a] -> Flow a
(defn run2 [>task] (m/ap (m/? (m/?! >task))))

; m a -> m b ; a = Task _
(defnode run [task] (run2 task))

(defn place! [s] (m/ap ()))

(leo-lang
  (let [a (m/signal! (m/ap 42))
        b (m/signal! (m/ap (inc (m/?! a))))
        c (m/signal! (m/ap 0))]
    (+ (inc a) b (inc a) c)))


(defnode input []
  (let [!needle (atom "")
        needle ]

    ))

(tests
  (defnode App [db x]
    (div
      (let [xs' (dx (run (q (if2 (boolean (inc (inc 0))) db db))))]
        (pre (pr-str @(m/reductions patch xs')))
        (rfor [id xs']
          (pre
            (pr-str @(entity-get db id))
            (rfor [k (entity-ks db id)]
              (div
                (input)
                (pr-str (entity-get db id k)))))))))
  := _)



; given f a flow transformer, how do you apply f to a flow in a dag
