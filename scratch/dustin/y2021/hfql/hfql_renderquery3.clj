(ns dustin.hfql.hfql-renderquery3
  (:require [clojure.pprint :as pprint]
            [hfdl.impl.trace :refer [system debug]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]
            [minitest :refer [tests]]
            [datascript.core :as d]
            [missionary.core :as m]))

; a -> b – clojure interop, e.g. (inc a)
; a -> m b – clojure effects, e.g. (async (d/q … x))
; m a -> m b – DAGs – dynamic or differential operations

; if :: m bool -> m a -> m a -> m a
; for :: m [a] -> (a -> m b) -> m [b]


; something that takes a reactive task and returns reactive result of running the task
(defn run [body]                                         ; Flow Task a -> Flow a
  (m/relieve {} (m/ap (m/? (m/?! body)))))

(defmacro ! [body]                                          ; Task a -> a
  `@(run (unquote ~body)))

(defmacro defnode [& body])                                 ;  a -> m b
; pretty much definline - instead of defining function, define a macro that expands to the body of the fn

; effects

(defn q [db x] (m/sp (d/q ... db x)))

(defn pprint [v] (m/sp (with-out-str (cljs.pprint/pprint v))))

(defn query-email! [db val] (m/sp (:dustingetz/email (datomic.api/entity db val))))

(defnode div [_]                                            ; effect with lifecycle = defnode ?
  (m/observe (fn [!]
               (let [ref (.createElement js/document "div")]
                 (! ref)
                 (fn []
                   (.. ref -parentElement (removeChild ref)))))))

(defnode render-email [val]
  (! pprint ~@(! q ... val)))

(defnode render-form [db e]                                 ; m a ... -> m b
  [div [render-email (! query-email! db (inc e))]])

[div (! pprint ~@(! q ... (! query-email! db (inc e))))]

(defn render-form [>db >e]
  (dataflow
    @(div @(render-email @(query-email! @>db (inc @>e))))))

(tests
  "compile - inline"
  (macroexpand-all '(render-form >db >id))
  := '(... div @(run (pprint ~@@(run (q ... @(run ~(query-email! @>db @>id))))))) ; one transfer)



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