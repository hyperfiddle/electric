(ns dustin.via7
  (:require
    [meander.epsilon :as m]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]))

(defn unquote-in-form? [form]
  (m/find form
    (m/scan (`unquote _))                                   ; (inc ~a b) but not (a (b ~c)) nor (inc a) nor ~a
    true))

(defn rewrite-binds [binds]
  (->> (partition 2 binds)
    (mapcat (fn [binding]
              (m/match binding
                (?k (`unquote ?form))                       ; [a ~(just 1)]
                [?k ?form]                                  ; {a (just 1)}

                (?k ?form) (m/subst [?k (pure ?form)]))))
    vec))

(defn rewrite-aps [xs]
  (map (fn [x]
         (m/rewrite x
           (`unquote ?a) ?a
           ?a (pure ?a)))
    xs))

(defn mlet [binds body]                                     ; todo applicative-do
  (->> (reverse (partition 2 binds))
    (reduce (fn [acc [l r]]
              `(~'bind ~r (fn [~l] ~acc)))                ; free bind
      body)))

(defn rewrite-await* [form]
  (m/match form

    (`unquote ?v)                                           ; ~a (clojure.core/unquote a)
    `(unquote ~?v)                                          ; pass through, likely a type error e.g. (for [a ~[1]] ~a)

    (let [!binds ...] ?body)
    (mlet (rewrite-binds !binds) ?body)

    ((m/pred symbol? ?f) (`unquote ?v))
    `(~'fmap ~?f ~?v)

    (m/pred unquote-in-form?)                               ; (inc ~a b c) [a ~b] but not ~a
    `(~'fapply ~@(rewrite-aps form))

    _ form))

(def rewrite-await (r/until = (r/top-down (r/attempt rewrite-await*))))

(tests

  (rewrite-await '(inc ~x))
  := '(fmap inc x)

  (rewrite-await '(+ a 1))
  := '(+ a 1)

  (rewrite-await '(inc ~a))
  := '(fmap inc a)

  (rewrite-await '(inc ~a ~b c))
  := '(fapply (pure inc) a b (pure c))

  (rewrite-await '(just 1))
  := '(just 1)

  (rewrite-await '(+ a ~(just 42)))
  := '(fapply (pure +) (pure a) (just 42))

  (rewrite-await '(let [a 1] ...))
  := '(bind (pure 1) (clojure.core/fn [a] ...))

  (rewrite-await '(let [a ~(just 1)] ...))
  := '(bind (just 1) (clojure.core/fn [a] ...))

  (rewrite-await '(let [a ~(pure 1)
                        b ~(+ a ~(pure 42))
                        c 1]
                    ...))
  := '(bind (pure 1) (clojure.core/fn [a]
      (bind (fapply (pure +) (pure a) (pure 42)) (clojure.core/fn [b]
      (bind (pure 1) (clojure.core/fn [c]
        ...))))))

  (rewrite-await '~a)
  := '(clojure.core/unquote a)

  (rewrite-await '(let [] ~a))                              ; likely type error
  := '(let [] (clojure.core/unquote a))                     ; leave it

  (rewrite-await '(let [a ~(just 1)] ...))
  := '(bind (just 1) (clojure.core/fn [a] ...))

  ;(macroexpand-1 '(do> (bind {} (clojure.core/unquote a))))
  ;:= '(mlet [] (clojure.core/unquote a))

  (rewrite-await
    '(let [a ~(just 1)
           b ~(+ a ~(just 42))
           c 1]
       ...))

  := '(bind (just 1) (clojure.core/fn [a]
      (bind (fapply (pure +) (pure a) (just 42)) (clojure.core/fn [b]
      (bind (pure 1) (clojure.core/fn [c]
        ...))))))

  )

(def ^:dynamic *monadic-scope* nil)

(defmacro via [m body]
  `(binding [*monadic-scope* ~m]
     (let [m# ~m
           ~'bind (m# :bind)
           ~'fmap (m# :fmap)
           ~'fapply (m# :fapply)
           ~'pure (m# :pure)]
       ~(rewrite-await body))))

(def maybe {:pure (fn [a] {:Maybe/just a})
            :fmap (fn [f & as]
                    (let [vs (map :Maybe/just as)]
                      (if (every? identity vs)
                        {:Maybe/just (apply f vs)})))
            :fapply (fn [& as] (apply (:fmap *monadic-scope*) #(apply % %&) as))
            :bind  (fn [{v :Maybe/just} f] (if v (f v)))})

(tests

  (via maybe (inc ~(pure 1)))
  := #:Maybe{:just 2}

  (via maybe (inc ~(via maybe (inc ~(pure 1)))))
  := #:Maybe{:just 3}

  (via maybe
    (let [f (pure +)
          a (pure 1)
          b ~(~f 10 ~a)]
      (pure (inc b))))
  := #:Maybe{:just 12}

  #_(macroexpand '(via maybe 1))

  #_#_#_
  (macroexpand '(via maybe
                  (let [f (pure +)
                        a (pure 1)
                        b ~(~f 10 ~a)]
                    (pure (inc b)))))
  := '(let* [m__28150__auto__ maybe
           bind (m__28150__auto__ :bind)
           fmap (m__28150__auto__ :fmap)
           fapply (m__28150__auto__ :fapply)
           pure (m__28150__auto__ :pure)]
      (bind (pure (pure +)) (clojure.core/fn [f]
      (bind (pure (pure 1)) (clojure.core/fn [a]
      (bind (fapply f (pure 10) a) (clojure.core/fn [b]
        (pure (inc b)))))))))


  )