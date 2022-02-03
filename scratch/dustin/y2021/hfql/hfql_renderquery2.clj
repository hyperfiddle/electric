(ns dustin.hfql.hfql-renderquery2
  (:require [clojure.pprint :as pprint]
            [hfdl.impl.trace :refer [system debug]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]
            [minitest :refer [tests]]
            [datascript.core :as d]))

; input can change while query is running, its really a flow problem not a task problem
;(defn query-email-flow [>db >val]
;  (m/relieve {} (m/ap (m/? (m/?! (m/latest query-email! >db >val)))))
;  #_(m/relieve {} (m/ap (m/? (query-email! (m/?! >db) (m/?! >val))))))

;(defn run [f & [>a >b & args]]                              ; (a -> Task b) -> [Flow a] -> Flow b
;  (m/relieve {} (m/ap (m/? (m/?! (apply m/latest f args)))))) ; overlap with what HFDL does

; something that takes a reactive task and returns reactive result of running the task
(defn run [body]                                         ; Flow Task a -> Flow a
  (m/relieve {} (m/ap (m/? (m/?! body)))))

(defmacro ! [body]                                          ; Task a -> a
  `@(run (unquote ~body)))

(defmacro defnode [& body])                                 ;  a -> m b
; pretty much definline - instead of defining function, define a macro that expands to the body of the fn

; effects

(defn q [db x]
  (m/sp (d/q ... db x)))

(defnode div [_]                                            ; effect with lifecycle = defnode ?
  ...?
  (js/document.createElement ...))

(defn pprint [v]
  (m/sp (cljs.pprint/pprint v)))

(defn query-email! [db val]
  (m/sp (:dustingetz/email (datomic.api/entity db val))))

(defnode render-email [val a]
  (cljs.pprint/pprint ~@(run (q ... val))))

(defnode render-form [db val]
  (div @(render-email (! (query-email! db val)) (m/ap (println 1))))
  #_[div [render-email [query-email! db val] (m/ap (println 1))]])

(tests
  "compile - inline"
  (macroexpand-all '(render-form >db >id))
  := '(... div @(run (pprint ~@@(run (q ... @(run ~(query-email! @>db @>id))))))) ; one transfer)

; a -> m b     -- effect
; m a -> m b   -- special form for differential operations (for, if)

; if :: m bool -> m a -> m a -> m a
; for :: m [a] -> (a -> m b) -> m [b]

(tests
 (def !id (atom 9))
 (def >id (m/watch !id))
 (def !db (atom hyperfiddle.api/*$*))
 (def >db (m/watch !db))
 (def dag (lang (render-form @>db @>id)))
 ((system (debug sampler dag)) prn prn)
 @sampler
 )




;(defnode render-form [>db >val]
;  [div [render-email [query-email! >db >val]]])