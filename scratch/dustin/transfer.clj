(ns dustin.transfer
  (:require [datascript.core :as d]
            [missionary.core :as m]
            [minitest :refer [tests]]
            [hfdl.lang :refer [dataflow debug! heap-dump]]
            [hfdl.lib :refer [reactive-for]]))

(defn result [program process]
  (get (heap-dump process) [(:result program)]))

;(defn app [>db]
;  (dataflow
;    (let [db @>db]
;      [:pre (pr-str ! (:task/name (d/entity ~db 1)))])))

; diamond shape
; one branch has a transfer, other has two switches
;(dataflow
;  (let [a @>db]
;    [:pre
;     (client-inc ! (hash (:user/password (d/entity a 1))))
;     (server-dec ! (client-inc ! (:task/name (d/entity a 1))))]))


; zookeeper - master registry of distributed peers -  a large KV distributed store

; Q: when should transfer happen
; 1: should we detect locality with resolve?
; 2: should we mark



#?(:clj (def server-inc inc))
#?(:cljs (def client-inc inc))

(defn app [>n]
  (dataflow
    (let [n @>n]
      (client-inc ! (server-inc n)))))

(tests

  (def !n (atom 0))
  (def dag (app (m/watch !n)))


  (def black (debug! dag :black))
  (result dag @black) := :no-result
  (:log @black)
  := '[{dustin.transfer/server-inc                                                              _,
        [missionary.core$watch$fn__10677 0x752924a4 "missionary.core$watch$fn__10677@752924a4"] _,
        (clojure.core/deref _)                                                                  0,
        (dustin.transfer/server-inc
          (clojure.core/deref _))                                                               1}]

  (def white (debug! dag :white (:log @p)))
  (:log @white) := _                                            ; next frame available
  (result dag @white) := 2

  (reset! !n 100)
  (result dag @black) := :no-result                         ; intermediate log has 1
  (result dag @white) := 102
  )
