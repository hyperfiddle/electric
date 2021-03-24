(ns dustin.react
  (:require
    reagent.dom
    [reagent.core :as r]))

; 1 translate to reagent


(defn UserGreeting [props] [:h1 "Welcome back!"])
(defn GuestGreeting [props] [:h1 "Please sign up."])
(defn Wrapper [& children] ...)
(defn PromptBrandLogo [isLoggedIn] ...)

    (defn Greeting [{:keys [isLoggedIn]}]
      [Wrapper [:div [PromptBrandLogo isLoggedIn]]
       (if isLoggedIn [UserGreeting {}] [GuestGreeting {}])])

(reagent.dom/render
  [Greeting {:isLoggedIn false}]
  (.-body js/document))


; 2 remove the hiccup and use s-expressions


(defn UserGreeting [props] `(:h1 "Welcome back!"))
(defn GuestGreeting [props] `(:h1 "Please sign up."))

(defn Wrapper [& children] ...)
(defn div [& children] ...)
(defn PromptBrandLogo [isLoggedIn] ...)
(defn fib [x]
  (Thread/sleep 1000)
  (println "done")
  `~x)

    (defn Greeting [{:keys [isLoggedIn]}]
      `(Wrapper [:div (PromptBrandLogo ~isLoggedIn)]
         ~(if isLoggedIn `(UserGreeting ~{}) `(GuestGreeting ~{}))))



(react/render! `(Greeting false) (.-body js/document))
; println 42
(react/render! `(Greeting true) (.-body js/document))
; -


; 2
(def isLoggedIn (atom false))
(add-watch isLoggedIn :a (fn [_ _ old new]
                           (react/render! `(Greeting ~@isLoggedIn) (.-body js/document))))
(react/render! `(Greeting ~isLoggedIn) (.-body js/document))

; 3
(def isLoggedIn (atom false))
(react/render! `(Greeting ~isLoggedIn) (.-body js/document))
; println 42
(reset! isLoggedIn true)
; -




(defn reactDomRender [ast $el]
  (let [ast1 ast
        ast2 (clojure.core/eval ast1)                       ; runtime eval = interpreter
        new-vdom (incremental-eval ast2)]                   ; interpreter
    (println new-vdom)
    (reconcile! $el new-vdom)))

(reactDomRender '(Greeting {:isLoggedIn false}) (.-body js/document))

(eval '(Greeting {:isLoggedIn false}))
:= `(Wrapper (div (PromptBrandLogo isLoggedIn))
      (fib 40)
      (GuestGreeting {}))

; Abstract Syntax Tree vs Tree
; evaluation rules will be provided later to faciliate evaluation of the abstractions

'(Wrapper (div (PromptBrandLogo false)))
[:div.wrapper [:div [:div.logo "you are not loggedin"]]]
(new ReactElement "div" (new ReactElement "div" (new ReactElement "div")))
