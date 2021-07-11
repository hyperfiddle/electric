(ns user.form-typeahead2
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p :refer [defnode vars local2 bind]]
            [hfdl.impl.util :as u]
            [hyperfiddle.api :as hf :refer [*$*]]
            [hyperfiddle.q2 :as q :refer [q nav hfql]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions shirt-sizes]])
  #?(:cljs
     (:require-macros [user.gender-shirt-size :refer [ref2 render-form page-submissions]])))

(defnode render)
(defnode options)

;(defnode typeahead-select-no-dynamic [v props]
;  (let [div (dom/element "div")
;        input (doto (dom/element "input")
;                (dom/set-attribute! "type" "text"))
;        results (doto (dom/element "select")
;                  #_(d/set-visible! (d/focused? input)))
;        needle (-> ~(m/relieve {} (dom/events input dom/input-event))
;                   dom/event-target dom/get-value)]
;    ~(dom/mount (dom/parent) div)
;    ~(dom/mount div input)
;    ~(dom/mount div results)
;    ~@(p/for [e (options ~@needle)]
;             (let [id ~(nav e :db/ident)]
;               ~@~(dom/mount results
;                             (doto (dom/element "option")
;                               (dom/set-text-content! (name id))))))))

(defnode typeahead-select [v props]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select
        ~@(p/for [e (options ~@needle)]
                 ~@(dom/option (dom/span ~@(name o))))))))

(defmacro render-ref [v props]
  `(p/bind [(options [needle] ~(::hf/options props))]
           (typeahead-select ~v (dissoc props ::hf/options))))

(defnode render-text [x opts] (dom/input x))

(defnode page-submissions []
  (p/bind [(dom/parent [] ...)]
          (hfql
            [{(submissions "")
              [:db/id
               (:person/email ::hf/render render-text)
               {(:person/shirt-size)
                ::hf/options (shirt-sizes :dustingetz/male .)
                ::hf/render render-ref
                ;(options [_1] (shirt-sizes :dustingetz/male _1))
                ;(render [v] (typeahead-select v))
                [:db/ident]}]}])))

(def !needle (atom ""))

(comment
  ((local2 exports (page-submissions ~@~(m/watch !needle))) prn u/pst)
  )
