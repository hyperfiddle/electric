(ns dustin.evaluator3)

; via as of December 2020

(defn Greeting [isLoggedIn]
  (let [foo (if isLoggedIn
              (eval `(UserGreeting. ~{}))
              (eval `(GuestGreeting. ~{})))
        ast `(Wrapper
               (div (PromptBrandLogo ~isLoggedIn))
               (fib 40)
               ~foo)]
    (eval ast)))

; Do you interpret immediately, like via?
; Do you return an AST to the caller to evaluate, like React?

(defn Greeting [isLoggedIn]                                 ;  a -> AST b
  (let [foo (if isLoggedIn
              `(UserGreeting. ~(assoc {} :x (inc y)))
              `(GuestGreeting. ~{}))
        ast `(Wrapper
               (div (PromptBrandLogo ~isLoggedIn))
               (fib 40)
               (fib 40)
               ~foo)]
    ast))


(def y ...)
(compile
  )

(dataflow
  (defn Greeting [isLoggedIn]                         ;  a -> AST b
    ; Constructor
    (Wrapper.
      (div. (PromptBrandLogo. @isLoggedIn))
      @(fib 40)
      @(fib 40)
      (if @isLoggedIn
        (UserGreeting. (assoc {} :x (inc y)))
        (GuestGreeting. {}))))
  (new Greeting (unquote false)))

(defn Greeting [isLoggedIn]                                 ; a -> b
  (via (reify Evaluator
         ;ITraverse (traverse [continuation ast])                 ; (for [x ys] (continuation x)) -> (traverse continuation ys)
         IMonad
         (bind   [ma continuation] ...)
         (fapply [mf & mas] ...)
         (fmap   [f ma] ...))
    (let [a (fib 40)
          a (fib 40)
          foo (if isLoggedIn
                `(UserGreeting. ~(assoc {} :x (inc y)))
                `(GuestGreeting. ~{}))
          ast `(Wrapper
                 (div (PromptBrandLogo ~isLoggedIn))
                 (fib 40)
                 (fib 40)
                 ~foo)]
      ast)))

(incr-eval (Greeting false))                                     ; (spawn (Greeting ...))

(def evaluator (reify Evaluator
                 ;ITraverse (traverse [continuation ast])                 ; (for [x ys] (continuation x)) -> (traverse continuation ys)
                 IMonad
                 (bind   [ma continuation] ...)
                 (fapply [mf & mas] (apply m/latest call mf mas))
                 (fmap   [f ma] ...)))
(def ast `(+ (fib. 40) (fib. 40)))
(via evaluator ast {'+ (pure +) 'fib (pure (fn fib [n] ...))})
:= (pure 99999)


(defn Greeting [isLoggedIn]                                 ; a -> b
  (let [a (fib 40)
        a (fib 40)
        foo (if isLoggedIn
              `(UserGreeting. ~(assoc {} :x (inc y)))
              `(GuestGreeting. ~{}))
        ast `(Wrapper
               (div (PromptBrandLogo ~isLoggedIn))
               (fib 40)
               (fib 40)
               ~foo)]
    ast))

(via (reify Evaluator
       ;ITraverse (traverse [continuation ast])                 ; (for [x ys] (continuation x)) -> (traverse continuation ys)
       IMonad
       (bind [ma continuation] ...)
       (fapply [mf & mas] ...)
       (fmap [f ma] ...))
  (Greeting false))

; Leo-lang contstructs dags at compile time thus do not need react-keys on every node


(defn fib [n])
(defn g [isLoggedIn]
  (assoc {} :x isLoggedIn))

; we agree we must choose which direction to bias

(def compiled-program
  (compile
    (defn Greeting [isLoggedIn]                                 ; a -> b
      (let [a (println 1)
            b (println 2)]
        (Wrapper.
          (div. (PromptBrandLogo. isLoggedIn))
          (fib. a)
          (fib. b)
          (if isLoggedIn
            (UserGreeting. ~(g isLoggedIn))
            (GuestGreeting. {})))))))

(evaluate evaluator compiled-program)