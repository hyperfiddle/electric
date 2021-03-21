(ns dustin.evaluator1)

;let ReactDOM;
;let UserGreeting = (props) => <h1>Welcome back!</h1>;
;let GuestGreeting = (props) => <h1>Please sign up.</h1>;
;let Greeting = (props) => props.isLoggedIn ? <UserGreeting /> : <GuestGreeting />;
;ReactDOM.render(<Greeting isLoggedIn={false} />, document.getElementById('root'));

(defn GuestGreeting [props] (println 'GuestGreeting) 42)
(defn UserGreeting [props] (println 'UserGreeting) 43)


(defn Greeting1 [props] (UserGreeting props))
(defn Greeting2 [props]
  (println 'greeting2)
  (if (:is-logged-in props)
    `(UserGreeting ~props)
    `(GuestGreeting ~props)))

(defn App [props]
  `(html
    (Greeting2 ~(assoc props :x 2))
    (Greeting2 ~props)))

(defn App [props]
  `[html
    [Greeting2 ~(identity props)]
    [Greeting2 ~props]])

(defn App [props]
  (via eval1 eval2
    (html
      (Greeting2 ~(identity props))
      (Greeting2 ~props))))

; greeting2
; greeting2
; UserGreeting
; GuestGreeting
; UserGreeting
; GuestGreeting


(def eval1 eval)
(def eval2 eval)                                            ; reactive-eval

(defn react-interpreter [lisp-ast]
  (let [reactive-ast (eval1 lisp-ast)]
    (eval2 reactive-ast)))

(react-interpreter '(Greeting2 {}))

(defn Greeting2> [props]
  (if (:is-logged-in props)
    (via (let [x ~(identity props)] (UserGreeting props)))
    (bind (identity props) (fn [x] :rk (UserGreeting props1)))
    (via (GuestGreeting props))))


(binding [*reactor* ...]
  (Greeting2> ...))








(declare h1 via deflow)

; Clojure evaluates stack wise
; JSX evaluates queue-wise (dataflow)

; 1 clojure (stack-wise, depth first)
(defn UserGreeting [props] (h1 "Welcome Back!"))
(defn GuestGreeting [props] (h1 "Please sign up."))
(defn Greeting [props] (if (:is-logged-in props) (UserGreeting props) (GuestGreeting props)))

; 2 React - runtime ast generation through templates ?
; Reactive AST is templated stackwise, then evaluated queue-wise with memoizing
; note the clojure stack unwinds before the react AST evals !
(defn UserGreeting [props] [:h1 "Welcome Back!"])
(defn GuestGreeting [props] [:h1 "Please sign up."])
(defn Greeting [props] (if (:is-logged-in props) [UserGreeting props] [GuestGreeting props]))
; React needs an external evaluator engine / trampoline thing

; 3 reagent, but use s-expressions over hiccup, does this add power or is it worse?
(defn UserGreeting [props] `(h1 "Welcome Back!"))
(defn GuestGreeting [props] `(h1 "Please sign up."))
; Scope crossing is explicit. Clojure stack unwinds
(defn Greeting [props] (if (:is-logged-in props) `(UserGreeting ~props) `(GuestGreeting ~props)))
; Like React, needs trampoline

; 4 monad composition ? via is eval - a clojure interpreter with breadth-first evaluation and memoizing. (isolated reactor)
(defn UserGreeting> [props] (via (h1 "Welcome Back!")))
(defn GuestGreeting> [props] (via (h1 "Please sign up.")))
; inside the via is planned and memoized. lexical scope is shared
(defn Greeting> [props] (if (:is-logged-in props) (via (UserGreeting> props)) (via (GuestGreeting> props))))
; No external trampoline needed

(defn coin-flip! [] (rand-nth [true false]))

; I see now that via is eval. But the argument to via is a literal, so it can be compiled !
; There is no need to support runtime generation of the AST passed to via
(defn Greeting> [props] (if (:is-logged-in props)
                          (via `(~(if (coin-flip!) UserGreeting> f>)) props)
                          (via (GuestGreeting> props))))

; 5 stackless clojure? This erases the function seams producing big lexical AST
(deflow h1 [s] (patch-dom! (str "<h1>" s "</h1>")))
(deflow UserGreeting [props] (h1 "Welcome Back!"))
(deflow GuestGreeting [props] (h1 "Please sign up."))
; erases the reactive call, inlining the result ?
(deflow Greeting [props] (if (:is-logged-in props) (UserGreeting props) (GuestGreeting props)))
:= '(let [props ...]
      (if (:is-logged-in props)
        (let [props props] (h1 "Welcome Back!"))
        (let [props props] (h1 "Please sign up."))))
; This flattens the DAG (inlining all nesting). At what cost? How do you react at a function call?
; The AST is fixed therefore the DAG is fixed ?
; Since there is no nest/join, then the leaf nodes must be effects?

; 6 quote everything, what even is this
'[(defn UserGreeting [props] (h1 "Welcome Back!"))
  (defn GuestGreeting [props] (h1 "Please sign up."))
  (defn Greeting [props] (if (:is-logged-in props) (UserGreeting props) (GuestGreeting props)))]

; where are the >a stream values and the do-notation? We lost them


; Via is eval
; It could be clojure-eval or reactor-eval
; Reactive via is a complete clojure interpreter but with breadth-first evaluation
; This makes tracing hard, why?
; Can it be a compiler instead of interpreter? Does that save us?


(via maybe (inc ~(via maybe (inc ~(pure 1)))))
'(defn foo [a b] (+ (inc a) (inc b)))                       ; explicit clojure interpreter e.g. SCI, bypassing default compiler
'(defn foo [a b] (via (+ (inc ~>a) (inc b))))