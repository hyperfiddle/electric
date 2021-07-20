(ns user.lang2
  "Photon language tutorial"
  (:require [hyperfiddle.photon :as r :refer [defnode node]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(defnode parent)
(defnode options)
(defnode typeahead-select []
  (let [div (d/element "div")]
    (bind [(parent [] div)]
          (let [input (doto (d/element "input")
                        (d/set-attribute! "type" "text"))
                needle (-> ~(m/relieve {} (d/events input d/input-event))
                           d/event-target d/get-value)]
            ~@(h/for [e (options ~@needle)]
                     (let [id ~(nav e :db/ident)]
                       ~@~(doto (d/element "div")
                            (d/set-text-content! (name id)))))))))
(main
  (let [entity hf/alice]
    (bind [(parent [] (d/by-id "root"))
           (options [needle] (shirt-sizes ~(nav entity :dustingetz/gender :db/ident) needle))]
          (typeahead-select))))


(defnode typeahead-select [v options]
  (dom/div
    (let [needle (dom/input)]
      (dom/select
        (for [x (options needle)]
          (dom/option (pr-str (database/query ... x))))))))

(defnode typeahead-select [v options]
  (let [ref (atom "")]
    (dom/div
      (dom/input ref)
      (dom/select
        (for [x (options ~(m/watch ref))]
          (dom/option (pr-str (database/query ... x))))))))

; order of operations, a consequence of dynamic scope
; clarity of syntax
; ability to manipulate raw objects for advanced use cases



(typeahead-select v (node [needle] (database/query ... needle)))

(defnode if' [x p q] (if x p q))
(defnode g)
(defnode f [x]
  (do
    (println x)                                             ; stream
    ~(m/ap (println x))))                                   ; signal, but attached main which is a stream


(photon/bind [(g [] 1)]
      (dom/div (if' true (f 1) (f 2))))

(defn ignore [a] 1)
(ignore (inc (inc 2)))







(defnode typeahead-select [v options]
  ~@(dom/div
      (let [needle (dom/input)]
        (dom/select v
                    (for [x ~@(options needle)]
                      (dom/option x))))))

(photon/main
  (binding [dom/*parent* (js/document.querySelector "#root")]
    ~@(binding [*db* (datomic.api/db ...)]
        (typeahead-select 42
                          (node [needle] (datomic.api/q ... needle *db*))))))